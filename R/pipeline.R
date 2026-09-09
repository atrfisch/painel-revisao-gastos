# =============================================================================
# Painel de Revisão de Gastos — pipeline diário
#
# O que este script faz, em ordem:
#   1. Baixa a despesa detalhada do exercício corrente e do anterior (SIOP,
#      via pacote orcamentoBR).
#   2. Baixa o IPCA (Banco Central, série SGS 433) e monta o deflator.
#   3. Lê o histórico mensal por ação (gerado por R/backfill_mensal.R) para
#      obter o acumulado do mesmo período do ano anterior e o perfil de
#      sazonalidade de cada ação.
#   4. Calcula os desvios contra os dois parâmetros (LOA e ano anterior real).
#   5. Aplica as regras de triagem de oportunidades de ajuste.
#   6. Escreve os JSONs consumidos pelo site em docs/dados/.
#   7. Grava o snapshot do dia em historico/.
#
# Roda inteiro dentro do GitHub Actions. Não precisa de R na sua máquina.
# =============================================================================

suppressPackageStartupMessages({
  library(orcamentoBR)
  library(dplyr)
  library(tidyr)
  library(jsonlite)
  library(readr)
  library(stringr)
  library(lubridate)
})

options(timeout = 900, scipen = 999)

# ---------------------------------------------------------------------------
# 0. Parâmetros da metodologia — todos os limiares ficam aqui
# ---------------------------------------------------------------------------

P <- list(
  exercicio          = as.integer(format(Sys.Date(), "%Y")),
  deflator_serie     = 433,      # IPCA variação mensal, SGS/BCB
  # Materialidade: nada abaixo disso entra na lista de oportunidades.
  piso_material      = 50e6,     # R$ 50 milhões
  piso_material_baixo= 10e6,     # R$ 10 milhões (regras de ação simbólica)
  # Limiares das regras de triagem
  lim_subexecucao    = 0.20,     # 20 p.p. abaixo do ritmo esperado
  lim_expansao_real  = 0.15,     # +15% real contra o mesmo período do ano anterior
  lim_retracao_real  = -0.15,
  lim_credito        = 0.30,     # dotação 30% acima da LOA
  lim_exec_simbolica = 0.05,     # menos de 5% da dotação empenhada
  lim_dezembro       = 0.40,     # mais de 40% do empenho do ano em dezembro
  lim_z_robusto      = 3,        # desvio robusto dentro da função
  frag_teto_uo       = 5e6,      # dotação por UO abaixo disso conta como pulverizada
  frag_min_uo        = 10,       # ação presente em 10+ UOs
  # Regime Fiscal Sustentável (LC 200/2023): banda de crescimento real anual da
  # despesa primária. A taxa de referência do exercício pode ser sobrescrita
  # pela variável de ambiente RFS_TAXA no workflow.
  rfs_taxa           = as.numeric(Sys.getenv("RFS_TAXA", "0.025")),
  rfs_piso           = 0.006,
  rfs_teto           = 0.025,
  # Resultado primário considerado discricionário (espaço de manobra real)
  rp_discricionario  = c("2", "6", "7", "8", "9"),
  # Funções sem sentido de revisão por desvio (excluídas do ranking, não da base)
  funcoes_excluidas  = c("28", "99")  # Encargos especiais e Reserva de contingência
)

dir.create("docs/dados", recursive = TRUE, showWarnings = FALSE)
dir.create("historico", recursive = TRUE, showWarnings = FALSE)

msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# Localiza uma coluna pelo padrão do nome. O orcamentoBR já mudou rótulos entre
# versões; isso evita quebrar o pipeline por causa de acento ou camelCase.
col_de <- function(df, ...) {
  padroes <- c(...)
  nomes <- names(df)
  for (p in padroes) {
    hit <- nomes[str_detect(str_to_lower(nomes), str_to_lower(p))]
    if (length(hit) > 0) return(hit[1])
  }
  NA_character_
}

pega <- function(df, ..., default = NA) {
  cn <- col_de(df, ...)
  if (is.na(cn)) return(rep(default, nrow(df)))
  df[[cn]]
}

num <- function(x) {
  if (is.numeric(x)) return(x)
  suppressWarnings(as.numeric(gsub("[^0-9\\.\\-]", "", as.character(x))))
}

# ---------------------------------------------------------------------------
# 1. Coleta no SIOP
# ---------------------------------------------------------------------------

baixa_despesa <- function(ano) {
  msg("Baixando despesa detalhada do exercício ", ano, " no SIOP")
  despesaDetalhada(
    exercicio           = ano,
    Esfera              = TRUE,
    Orgao               = TRUE,
    UO                  = TRUE,
    Funcao              = TRUE,
    Subfuncao           = TRUE,
    Programa            = TRUE,
    Acao                = TRUE,
    GND                 = TRUE,
    ResultadoPrimario   = TRUE,
    valorPLOA           = TRUE,
    valorLOA            = TRUE,
    valorLOAmaisCredito = TRUE,
    valorEmpenhado      = TRUE,
    valorLiquidado      = TRUE,
    valorPago           = TRUE,
    incluiDescricoes    = TRUE,
    timeout             = 900
  )
}

padroniza <- function(df, ano) {
  tibble(
    exercicio   = ano,
    esfera      = as.character(pega(df, "^esfera$", "esfera")),
    orgao_cod   = as.character(pega(df, "codigo.*orgao", "orgao.*codigo", "^orgao$")),
    orgao_nome  = as.character(pega(df, "orgao.*(nome|descri)", "nome.*orgao")),
    uo_cod      = as.character(pega(df, "codigo.*unidade", "unidade.*codigo", "^uo$", "unidade")),
    uo_nome     = as.character(pega(df, "unidade.*(nome|descri)", "nome.*unidade")),
    funcao_cod  = as.character(pega(df, "codigo.*funcao(?!.*sub)", "^funcao$")),
    funcao_nome = as.character(pega(df, "funcao.*(nome|descri)")),
    subfuncao   = as.character(pega(df, "subfuncao")),
    programa    = as.character(pega(df, "programa")),
    acao_cod    = as.character(pega(df, "codigo.*acao", "^acao$")),
    acao_nome   = as.character(pega(df, "acao.*(nome|descri)", "nome.*acao")),
    gnd_cod     = as.character(pega(df, "codigo.*gnd", "^gnd$", "grupo.*despesa")),
    gnd_nome    = as.character(pega(df, "gnd.*(nome|descri)", "grupo.*(nome|descri)")),
    rp_cod      = as.character(pega(df, "codigo.*resultado", "resultado.*primario")),
    ploa        = num(pega(df, "ploa", default = 0)),
    loa         = num(pega(df, "^valorloa$", "loa(?!.*credito)", default = 0)),
    dotacao     = num(pega(df, "credito", default = 0)),
    empenhado   = num(pega(df, "empenhado", default = 0)),
    liquidado   = num(pega(df, "liquidado", default = 0)),
    pago        = num(pega(df, "pago", default = 0))
  ) %>%
    mutate(across(c(ploa, loa, dotacao, empenhado, liquidado, pago),
                  ~ replace_na(.x, 0))) %>%
    # Limpa códigos: o SIOP às vezes devolve "26000 - Ministério da Educação"
    mutate(across(ends_with("_cod"), ~ str_trim(str_replace(.x, "\\s*-.*$", "")))) %>%
    group_by(exercicio, esfera, orgao_cod, orgao_nome, uo_cod, uo_nome,
             funcao_cod, funcao_nome, acao_cod, acao_nome, gnd_cod, gnd_nome, rp_cod) %>%
    summarise(across(c(ploa, loa, dotacao, empenhado, liquidado, pago), sum, na.rm = TRUE),
              .groups = "drop")
}

atual    <- padroniza(baixa_despesa(P$exercicio), P$exercicio)
anterior <- padroniza(baixa_despesa(P$exercicio - 1L), P$exercicio - 1L)

msg("Linhas coletadas: ", nrow(atual), " (t) e ", nrow(anterior), " (t-1)")

# ---------------------------------------------------------------------------
# 2. Deflator IPCA
# ---------------------------------------------------------------------------

msg("Baixando IPCA na API do Banco Central")
ipca <- tryCatch({
  url <- sprintf(
    "https://api.bcb.gov.br/dados/serie/bcdata.sgs.%d/dados?formato=json&dataInicial=01/01/2018",
    P$deflator_serie
  )
  fromJSON(url) %>%
    mutate(data = dmy(data), valor = as.numeric(valor)) %>%
    arrange(data) %>%
    mutate(indice = cumprod(1 + valor / 100)) %>%
    select(data, indice)
}, error = function(e) {
  msg("AVISO: IPCA indisponível (", conditionMessage(e), "). Seguindo sem deflacionar.")
  NULL
})

mes_ref <- floor_date(Sys.Date(), "month")
if (!is.null(ipca)) {
  base <- ipca$indice[which.max(ipca$data)]
  fator_de <- function(d) {
    d <- floor_date(as.Date(d), "month")
    idx <- ipca$indice[match(d, ipca$data)]
    ifelse(is.na(idx), 1, base / idx)
  }
  mes_ref <- max(ipca$data)
} else {
  fator_de <- function(d) rep(1, length(d))
}

# ---------------------------------------------------------------------------
# 3. Histórico mensal (mesmo período do ano anterior + sazonalidade)
# ---------------------------------------------------------------------------
# Gerado por R/backfill_mensal.R a partir da execução mensal consolidada do
# Portal da Transparência. Se o arquivo ainda não existe, o painel roda em
# "modo parcial": o eixo LOA funciona e o eixo interanual usa pro rata.

arq_hist <- "historico/mensal_acao.csv.gz"
tem_hist <- file.exists(arq_hist)

mes_corrente <- as.integer(format(Sys.Date(), "%m"))
dia_do_ano   <- as.integer(format(Sys.Date(), "%j"))
frac_ano     <- dia_do_ano / as.integer(format(as.Date(sprintf("%d-12-31", P$exercicio)), "%j"))

if (tem_hist) {
  msg("Lendo histórico mensal por ação")
  hist <- read_csv(arq_hist, show_col_types = FALSE) %>%
    filter(exercicio == P$exercicio - 1L) %>%
    mutate(fator = fator_de(as.Date(sprintf("%d-%02d-01", exercicio, mes))),
           emp_real = empenhado * fator)

  # Acumulado do ano anterior até o mesmo mês, a preços de hoje.
  # Duas chaves, porque nem sempre o arquivo mensal traz unidade orçamentária:
  # tenta UO+ação e, se não casar, cai para órgão+ação.
  acum_por <- function(df, chave_expr, nome) {
    df %>%
      filter(mes <= mes_corrente) %>%
      group_by(chave = {{ chave_expr }}) %>%
      summarise(!!nome := sum(emp_real, na.rm = TRUE), .groups = "drop")
  }
  ant_uo  <- acum_por(hist, paste(uo_cod, acao_cod, sep = "|"), "ant_uo")
  ant_org <- acum_por(hist, paste(orgao_cod, acao_cod, sep = "|"), "ant_org")

  # Perfil de sazonalidade: fração do empenho anual já realizada até este mês.
  perfil_por <- function(df, chave_expr, nome_perfil, nome_dez = NULL) {
    r <- df %>%
      group_by(chave = {{ chave_expr }}, mes) %>%
      summarise(emp = sum(empenhado, na.rm = TRUE), .groups = "drop_last") %>%
      arrange(mes, .by_group = TRUE) %>%
      mutate(acum = cumsum(emp), total = sum(emp)) %>%
      filter(total > 0) %>%
      summarise(
        !!nome_perfil := acum[max(which(mes <= mes_corrente))] / total[1],
        dez = sum(emp[mes == 12]) / total[1],
        .groups = "drop"
      )
    if (is.null(nome_dez)) r %>% select(-dez) else r %>% rename(!!nome_dez := dez)
  }
  perfil_uo  <- perfil_por(hist, paste(uo_cod, acao_cod, sep = "|"), "perfil_uo", "dez_share")
  perfil_org <- perfil_por(hist, paste(orgao_cod, acao_cod, sep = "|"), "perfil_org")
  perfil_funcao <- perfil_por(hist, funcao_cod, "perfil_fn") %>%
    rename(funcao_cod = chave)
} else {
  msg("AVISO: histórico mensal ausente. Rode o workflow 'Backfill mensal' uma vez.")
  ant_uo        <- tibble(chave = character(), ant_uo = numeric())
  ant_org       <- tibble(chave = character(), ant_org = numeric())
  perfil_uo     <- tibble(chave = character(), perfil_uo = numeric(), dez_share = numeric())
  perfil_org    <- tibble(chave = character(), perfil_org = numeric())
  perfil_funcao <- tibble(funcao_cod = character(), perfil_fn = numeric())
}

# ---------------------------------------------------------------------------
# 4. Métricas de desvio
# ---------------------------------------------------------------------------

fator_hoje <- 1  # valores do exercício corrente já estão a preços correntes de hoje

exec_anterior <- anterior %>%
  group_by(chave = paste(uo_cod, acao_cod, sep = "|")) %>%
  summarise(dotacao_ant = sum(dotacao), emp_ant_ano = sum(empenhado), .groups = "drop") %>%
  mutate(exec_ant = ifelse(dotacao_ant > 0, emp_ant_ano / dotacao_ant, NA_real_))

base <- atual %>%
  mutate(chave = paste(uo_cod, acao_cod, sep = "|"),
         chave_org = paste(orgao_cod, acao_cod, sep = "|")) %>%
  left_join(exec_anterior, by = "chave") %>%
  left_join(ant_uo,     by = "chave") %>%
  left_join(ant_org,    by = c("chave_org" = "chave")) %>%
  left_join(perfil_uo,  by = "chave") %>%
  left_join(perfil_org, by = c("chave_org" = "chave")) %>%
  left_join(perfil_funcao, by = "funcao_cod") %>%
  mutate(
    emp_ant_periodo = coalesce(ant_uo, ant_org),
    # ---- ritmo esperado -----------------------------------------------------
    # Preferência: perfil da própria ação no ano anterior. Depois, perfil da
    # função. Por último, pro rata temporis (o mais fraco dos três).
    perfil = coalesce(perfil_uo, perfil_org),
    perfil_esperado = coalesce(perfil, perfil_fn, frac_ano),
    origem_perfil = case_when(
      !is.na(perfil)    ~ "acao",
      !is.na(perfil_fn) ~ "funcao",
      TRUE              ~ "pro_rata"
    ),

    # ---- Eixo A: desvio contra a LOA ---------------------------------------
    var_dotacao   = ifelse(loa > 0, dotacao / loa - 1, NA_real_),
    exec_dotacao  = ifelse(dotacao > 0, empenhado / dotacao, NA_real_),
    exec_loa      = ifelse(loa > 0, empenhado / loa, NA_real_),
    esperado_loa  = loa * perfil_esperado,
    desvio_loa_rs = empenhado - esperado_loa,
    desvio_ritmo  = exec_dotacao - perfil_esperado,
    # Projeção do fechamento do ano pelo ritmo observado
    proj_ano      = ifelse(perfil_esperado > 0.05, empenhado / perfil_esperado, NA_real_),
    folga_proj    = pmax(dotacao - proj_ano, 0),

    # ---- Eixo B: variação real contra o mesmo período do ano anterior ------
    emp_ant_real  = emp_ant_periodo,
    var_real      = ifelse(!is.na(emp_ant_real) & emp_ant_real > 0,
                           empenhado / emp_ant_real - 1, NA_real_),
    desvio_ano_rs = ifelse(!is.na(emp_ant_real), empenhado - emp_ant_real, NA_real_),

    # ---- Eixo C: crescimento admitido pelo Regime Fiscal Sustentável -------
    # O limite da LC 200/2023 vale para o agregado da despesa primária de cada
    # Poder e órgão autônomo, não para a ação individual. Aqui ele entra como
    # régua de comparação: quanto a ação teria empenhado se tivesse crescido
    # exatamente na taxa de referência do regime.
    emp_rfs       = emp_ant_real * (1 + P$rfs_taxa),
    desvio_rfs_rs = ifelse(!is.na(emp_ant_real), empenhado - emp_rfs, NA_real_),
    var_vs_rfs    = ifelse(!is.na(var_real), var_real - P$rfs_taxa, NA_real_),

    # ---- classificação ------------------------------------------------------
    discricionaria = rp_cod %in% P$rp_discricionario,
    revisavel = discricionaria & !(funcao_cod %in% P$funcoes_excluidas)
  )

# Desvio robusto dentro da função (mediana e MAD, não média e desvio-padrão:
# a distribuição de variações orçamentárias é fortemente assimétrica).
base <- base %>%
  group_by(funcao_cod) %>%
  mutate(
    med_fn = median(var_real, na.rm = TRUE),
    mad_fn = mad(var_real, na.rm = TRUE),
    z_robusto = ifelse(!is.na(mad_fn) & mad_fn > 0, (var_real - med_fn) / mad_fn, NA_real_)
  ) %>%
  ungroup()

# ---------------------------------------------------------------------------
# 5. Triagem: oportunidades de ajuste
# ---------------------------------------------------------------------------
# Cada regra é uma hipótese a ser testada. O valor associado é a ordem de
# grandeza do montante envolvido, não uma economia apurada nem uma proposta de
# redução.

frag <- base %>%
  filter(dotacao > 0, dotacao < P$frag_teto_uo) %>%
  count(acao_cod, name = "n_uo_pulverizadas") %>%
  filter(n_uo_pulverizadas >= P$frag_min_uo)

base <- base %>%
  left_join(frag, by = "acao_cod") %>%
  mutate(
    op_subexecucao = revisavel &
      !is.na(desvio_ritmo) & desvio_ritmo < -P$lim_subexecucao &
      dotacao >= P$piso_material &
      (is.na(exec_ant) | exec_ant < 0.85),

    op_expansao = revisavel &
      !is.na(var_real) & var_real > P$lim_expansao_real &
      !is.na(desvio_ano_rs) & desvio_ano_rs >= P$piso_material,

    op_acima_rfs = revisavel &
      !is.na(var_vs_rfs) & var_vs_rfs > 0 &
      !is.na(desvio_rfs_rs) & desvio_rfs_rs >= P$piso_material,

    op_retracao = revisavel &
      !is.na(var_real) & var_real < P$lim_retracao_real &
      !is.na(desvio_ano_rs) & abs(desvio_ano_rs) >= P$piso_material,

    op_credito = !is.na(var_dotacao) & var_dotacao > P$lim_credito &
      (dotacao - loa) >= P$piso_material,

    op_simbolica = !is.na(exec_dotacao) & exec_dotacao < P$lim_exec_simbolica &
      loa >= P$piso_material_baixo &
      !is.na(exec_ant) & exec_ant < 0.10 &
      perfil_esperado > 0.5,

    op_fragmentacao = !is.na(n_uo_pulverizadas),

    op_dezembro = !is.na(dez_share) & dez_share > P$lim_dezembro &
      dotacao >= P$piso_material,

    op_outlier = revisavel & !is.na(z_robusto) & abs(z_robusto) > P$lim_z_robusto &
      abs(desvio_ano_rs) >= P$piso_material,

    n_sinais = rowSums(across(starts_with("op_")), na.rm = TRUE),

    # Espaço fiscal indicativo, por regra
    espaco = case_when(
      op_subexecucao  ~ folga_proj,
      op_simbolica    ~ dotacao - empenhado,
      op_credito      ~ dotacao - loa,
      op_expansao     ~ desvio_ano_rs,
      op_acima_rfs    ~ desvio_rfs_rs,
      TRUE            ~ NA_real_
    )
  )

# ---------------------------------------------------------------------------
# 6. Agregados para as visualizações de abertura
# ---------------------------------------------------------------------------

agrega <- function(df, ...) {
  df %>%
    group_by(...) %>%
    summarise(
      loa           = sum(loa, na.rm = TRUE),
      dotacao       = sum(dotacao, na.rm = TRUE),
      empenhado     = sum(empenhado, na.rm = TRUE),
      liquidado     = sum(liquidado, na.rm = TRUE),
      pago          = sum(pago, na.rm = TRUE),
      esperado_loa  = sum(esperado_loa, na.rm = TRUE),
      emp_ant_real  = sum(emp_ant_real, na.rm = TRUE),
      emp_rfs       = sum(emp_rfs, na.rm = TRUE),
      n_acoes       = n(),
      .groups = "drop"
    ) %>%
    mutate(
      desvio_loa_rs = empenhado - esperado_loa,
      desvio_loa_pc = ifelse(esperado_loa > 0, empenhado / esperado_loa - 1, NA_real_),
      desvio_ano_rs = ifelse(emp_ant_real > 0, empenhado - emp_ant_real, NA_real_),
      var_real      = ifelse(emp_ant_real > 0, empenhado / emp_ant_real - 1, NA_real_),
      desvio_rfs_rs = ifelse(emp_rfs > 0, empenhado - emp_rfs, NA_real_),
      var_vs_rfs    = ifelse(emp_ant_real > 0, var_real - P$rfs_taxa, NA_real_)
    )
}

ag_funcao <- agrega(base, cod = funcao_cod, nome = funcao_nome)
ag_orgao  <- agrega(base, cod = orgao_cod,  nome = orgao_nome)
ag_uo     <- agrega(base, cod = uo_cod,     nome = uo_nome)
ag_gnd    <- agrega(base, cod = gnd_cod,    nome = gnd_nome)

# ---------------------------------------------------------------------------
# 7. Exportação
# ---------------------------------------------------------------------------

mil <- function(x) round(replace_na(x, 0) / 1000)      # valores em R$ mil
pc  <- function(x) ifelse(is.na(x), NA, round(x, 4))

regras <- tribble(
  ~id,              ~rotulo,                              ~descricao,
  "subexecucao",    "Subexecução persistente",            "Empenho muito abaixo do ritmo esperado para a data e execução fraca também no ano anterior. Dotação provavelmente superestimada.",
  "simbolica",      "Dotação sem execução",               "Ação com dotação relevante e execução quase nula em dois exercícios. Pede exame do desenho da ação.",
  "credito",        "Dotação inflada por créditos",       "Dotação atual muito acima da LOA. Indica erro de previsão na proposta ou realocação não planejada.",
  "expansao",       "Expansão real acelerada",            "Crescimento real relevante contra o mesmo período do ano anterior. Pede exame dos parâmetros de custo e de elegibilidade.",
  "acima_rfs",      "Acima do crescimento do regime fiscal", "Crescimento real superior à taxa de referência do Regime Fiscal Sustentável. O limite legal é agregado, não por ação: aqui serve como régua de comparação.",
  "retracao",       "Retração real acentuada",            "Queda real relevante. Pode indicar problema de entrega, não economia.",
  "fragmentacao",   "Execução pulverizada",               "Mesma ação com dotações pequenas espalhadas por muitas unidades. Custo administrativo tende a superar o benefício.",
  "dezembro",       "Concentração no fim do exercício",   "Parcela alta do empenho anual concentrada em dezembro no ano anterior. Sinal clássico de gasto de baixa qualidade.",
  "outlier",        "Desvio atípico na função",           "Variação muito distante da mediana das demais ações da mesma função (escore robusto)."
)

resumo_regras <- lapply(seq_len(nrow(regras)), function(i) {
  id <- regras$id[i]
  sel <- base[[paste0("op_", id)]]
  sel[is.na(sel)] <- FALSE
  list(
    id       = id,
    rotulo   = regras$rotulo[i],
    descricao= regras$descricao[i],
    n        = sum(sel),
    espaco   = mil(sum(base$espaco[sel], na.rm = TRUE)),
    dotacao  = mil(sum(base$dotacao[sel], na.rm = TRUE))
  )
})

meta <- list(
  atualizado_em  = format(Sys.time(), "%Y-%m-%d %H:%M", tz = "America/Sao_Paulo"),
  exercicio      = P$exercicio,
  dia_do_ano     = dia_do_ano,
  frac_ano       = round(frac_ano, 4),
  mes_corrente   = mes_corrente,
  base_deflator  = format(mes_ref, "%Y-%m"),
  tem_historico  = tem_hist,
  origem_perfil  = as.list(table(base$origem_perfil)),
  totais = list(
    loa       = mil(sum(base$loa)),
    dotacao   = mil(sum(base$dotacao)),
    empenhado = mil(sum(base$empenhado)),
    liquidado = mil(sum(base$liquidado)),
    pago      = mil(sum(base$pago))
  ),
  rfs = list(
    taxa = P$rfs_taxa, piso = P$rfs_piso, teto = P$rfs_teto,
    crescimento_agregado = with(base[!is.na(base$emp_ant_real) & base$emp_ant_real > 0, ],
                                sum(empenhado) / sum(emp_ant_real) - 1),
    empenhado_comparavel = mil(sum(base$empenhado[!is.na(base$emp_ant_real) & base$emp_ant_real > 0])),
    excesso_agregado = mil(sum(base$desvio_rfs_rs, na.rm = TRUE))
  ),
  parametros = P[c("piso_material", "lim_subexecucao", "lim_expansao_real",
                   "lim_credito", "lim_exec_simbolica", "lim_dezembro")]
)

prep_ag <- function(df) {
  df %>%
    transmute(cod, nome,
              loa = mil(loa), dotacao = mil(dotacao), empenhado = mil(empenhado),
              esperado = mil(esperado_loa), ant = mil(emp_ant_real),
              d_loa = mil(desvio_loa_rs), p_loa = pc(desvio_loa_pc),
              d_ano = mil(desvio_ano_rs), p_ano = pc(var_real),
              d_rfs = mil(desvio_rfs_rs), p_rfs = pc(var_vs_rfs),
              n = n_acoes) %>%
    arrange(desc(abs(d_loa)))
}

write_json(
  list(meta = meta,
       oportunidades = resumo_regras,
       agregados = list(
         funcao = prep_ag(ag_funcao),
         orgao  = prep_ag(ag_orgao),
         uo     = prep_ag(ag_uo),
         gnd    = prep_ag(ag_gnd)
       )),
  "docs/dados/painel.json", auto_unbox = TRUE, na = "null", digits = 6
)

flags <- base %>%
  select(starts_with("op_")) %>%
  mutate(across(everything(), ~ replace_na(.x, FALSE)))
base$ops <- apply(flags, 1, function(r) paste(str_remove(names(r)[r], "^op_"), collapse = ","))

acoes <- base %>%
  transmute(
    esf = esfera, org = orgao_cod, orgn = orgao_nome,
    uo = uo_cod, uon = uo_nome,
    fn = funcao_cod, fnn = funcao_nome,
    ac = acao_cod, acn = acao_nome,
    gnd = gnd_cod, gndn = gnd_nome, rp = rp_cod,
    disc = as.integer(discricionaria),
    loa = mil(loa), dot = mil(dotacao), emp = mil(empenhado),
    liq = mil(liquidado), pag = mil(pago),
    esp = mil(esperado_loa), ant = mil(emp_ant_real),
    dloa = mil(desvio_loa_rs), dano = mil(desvio_ano_rs),
    pdot = pc(var_dotacao), pexec = pc(exec_dotacao),
    prit = pc(desvio_ritmo), pano = pc(var_real), z = pc(z_robusto),
    drfs = mil(desvio_rfs_rs), prfs = pc(var_vs_rfs),
    perf = pc(perfil_esperado), oper = origem_perfil,
    espaco = mil(espaco), sinais = n_sinais,
    ops = ops
  ) %>%
  filter(dot > 0 | emp > 0) %>%
  arrange(desc(abs(dloa)))

write_json(acoes, "docs/dados/acoes.json", auto_unbox = TRUE, na = "null", digits = 6)

msg("JSONs escritos: ", nrow(acoes), " ações")

# ---------------------------------------------------------------------------
# 8. Snapshot histórico
# ---------------------------------------------------------------------------
# Agregado diário sempre; detalhe por ação às segundas e no último dia do mês.
# Evita inflar o repositório com 20 mil linhas por dia.

hoje <- Sys.Date()
snap_ag <- base %>%
  group_by(funcao_cod, gnd_cod) %>%
  summarise(across(c(loa, dotacao, empenhado, liquidado, pago), sum), .groups = "drop") %>%
  mutate(data = hoje, .before = 1)

arq_ag <- "historico/serie_diaria_agregada.csv.gz"
if (file.exists(arq_ag)) {
  antigo <- read_csv(arq_ag, show_col_types = FALSE) %>% filter(as.Date(data) != hoje)
  snap_ag <- bind_rows(antigo, snap_ag)
}
write_csv(snap_ag, arq_ag)

fim_do_mes <- hoje == ceiling_date(hoje, "month") - days(1)
if (wday(hoje) == 2 || fim_do_mes) {
  snap_acao <- base %>%
    transmute(data = hoje, uo_cod, acao_cod, funcao_cod, gnd_cod,
              loa, dotacao, empenhado, liquidado, pago)
  arq_sn <- sprintf("historico/snapshot_acao_%s.csv.gz", format(hoje, "%Y-%m-%d"))
  write_csv(snap_acao, arq_sn)
  msg("Snapshot por ação gravado em ", arq_sn)
}

msg("Pipeline concluído.")
