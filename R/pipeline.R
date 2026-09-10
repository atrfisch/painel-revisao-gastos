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
  frag_teto_uo       = 5e6,      # dotação por UO abaixo disso conta como pulverizada
  frag_min_uo        = 10,       # ação presente em 10+ UOs
  # Resultado primário considerado discricionário (espaço de manobra real)
  rp_discricionario  = c("2", "6", "7", "8", "9"),
  # Funções sem sentido de revisão por desvio (excluídas do ranking, não da base)
  funcoes_excluidas  = c("28", "99")  # Encargos especiais e Reserva de contingência
)

dir.create("docs/dados", recursive = TRUE, showWarnings = FALSE)
dir.create("historico", recursive = TRUE, showWarnings = FALSE)

msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# Normaliza o nome de uma coluna em tokens separados por sublinhado, sem acento
# e sem caixa: "Código da Unidade Orçamentária" e "codigoUnidadeOrcamentaria"
# viram ambos "codigo_unidade_orcamentaria". Assim o reconhecimento não depende
# da convenção de escrita que o SIOP usar.
norm <- function(x) {
  # chartr explícito em vez de iconv: a transliteração do iconv varia conforme
  # o locale do runner e pode devolver "?" no lugar da letra acentuada.
  x <- chartr("\u00e1\u00e0\u00e2\u00e3\u00e4\u00e9\u00e8\u00ea\u00eb\u00ed\u00ec\u00ee\u00ef\u00f3\u00f2\u00f4\u00f5\u00f6\u00fa\u00f9\u00fb\u00fc\u00e7\u00c1\u00c0\u00c2\u00c3\u00c4\u00c9\u00c8\u00ca\u00cb\u00cd\u00cc\u00ce\u00cf\u00d3\u00d2\u00d4\u00d5\u00d6\u00da\u00d9\u00db\u00dc\u00c7",
              "aaaaaeeeeiiiiooooouuuucAAAAAEEEEIIIIOOOOOUUUUC", x)
  x <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", x)
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  gsub("^_|_$", "", x)
}

tem_token <- function(nn, tokens) {
  Reduce(`|`, lapply(tokens, function(t) grepl(paste0("(^|_)", t, "(_|$)"), nn)))
}

# Devolve código e descrição de uma dimensão. Entre as colunas candidatas, a de
# valores mais curtos é tratada como código e a de valores mais longos como
# descrição. Se houver uma só coluna no formato "1234 - Descrição", ela é
# partida em duas.
dimensao <- function(df, tokens, excluir = NULL) {
  vazio <- list(cod = rep(NA_character_, nrow(df)), nome = rep(NA_character_, nrow(df)))
  nn <- norm(names(df))
  hit <- which(tem_token(nn, tokens))
  if (length(excluir) && length(hit)) hit <- hit[!tem_token(nn[hit], excluir)]
  if (!length(hit)) return(vazio)

  cols <- lapply(hit, function(i) as.character(df[[i]]))
  comprimento <- vapply(cols, function(v) {
    v <- v[!is.na(v) & nzchar(v)]
    if (!length(v)) return(Inf)
    mean(nchar(v))
  }, numeric(1))

  i_cod  <- which.min(comprimento)
  i_nome <- if (length(cols) > 1) which.max(comprimento) else i_cod
  cod <- cols[[i_cod]]; nome <- cols[[i_nome]]

  if (i_cod == i_nome) {
    junto <- grepl("^\\s*[A-Za-z0-9.]+\\s+-\\s+\\S", cod)
    if (mean(junto, na.rm = TRUE) > 0.5) {
      nome <- sub("^\\s*[A-Za-z0-9.]+\\s+-\\s+", "", cod)
      cod  <- sub("\\s+-\\s+.*$", "", trimws(cod))
    }
  } else {
    cod <- sub("\\s+-\\s+.*$", "", trimws(cod))
  }
  list(cod = trimws(cod), nome = trimws(nome))
}

# Coluna de valor: casa por substring simples, com exclusões explícitas.
valor <- function(df, contem, sem = NULL) {
  nn <- norm(names(df))
  hit <- which(grepl(contem, nn))
  if (length(sem) && length(hit)) hit <- hit[!grepl(sem, nn[hit])]
  if (!length(hit)) return(rep(0, nrow(df)))
  v <- df[[hit[1]]]
  if (is.numeric(v)) return(v)
  suppressWarnings(as.numeric(gsub("[^0-9.\\-]", "", as.character(v))))
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
  msg("Colunas devolvidas pelo SIOP: ", paste(names(df), collapse = " | "))

  org  <- dimensao(df, c("orgao"),     excluir = c("unidade", "uo"))
  uo   <- dimensao(df, c("uo", "unidade", "orcamentaria"))
  fn   <- dimensao(df, c("funcao"),    excluir = c("sub"))
  sfn  <- dimensao(df, c("subfuncao"))
  prog <- dimensao(df, c("programa"))
  ac   <- dimensao(df, c("acao"))
  gnd  <- dimensao(df, c("gnd", "grupo"))
  rp   <- dimensao(df, c("resultado"))
  esf  <- dimensao(df, c("esfera"))

  saida <- tibble(
    exercicio   = ano,
    esfera      = esf$nome,
    orgao_cod   = org$cod,  orgao_nome  = org$nome,
    uo_cod      = uo$cod,   uo_nome     = uo$nome,
    funcao_cod  = fn$cod,   funcao_nome = fn$nome,
    subfuncao   = sfn$cod,
    programa    = prog$cod,
    acao_cod    = ac$cod,   acao_nome   = ac$nome,
    gnd_cod     = gnd$cod,  gnd_nome    = gnd$nome,
    rp_cod      = rp$cod,
    ploa        = valor(df, "ploa"),
    loa         = valor(df, "loa", sem = "credito|ploa"),
    dotacao     = valor(df, "credito"),
    empenhado   = valor(df, "empenhado"),
    liquidado   = valor(df, "liquidado"),
    pago        = valor(df, "pago")
  ) %>%
    mutate(across(c(ploa, loa, dotacao, empenhado, liquidado, pago), ~ replace_na(.x, 0)))

  # Diagnóstico: se uma dimensão não foi reconhecida, o painel inteiro colapsa.
  # Melhor descobrir aqui, no log, do que num agregado com quatro linhas.
  for (col in c("orgao_cod", "uo_cod", "funcao_cod", "acao_cod", "gnd_cod")) {
    n_dist <- n_distinct(saida[[col]], na.rm = TRUE)
    msg("  ", col, ": ", n_dist, " valores distintos")
    if (n_dist == 0) warning("Dimensão não reconhecida: ", col, call. = FALSE)
  }

  saida %>%
    group_by(exercicio, esfera, orgao_cod, orgao_nome, uo_cod, uo_nome,
             funcao_cod, funcao_nome, acao_cod, acao_nome, gnd_cod, gnd_nome, rp_cod) %>%
    summarise(across(c(ploa, loa, dotacao, empenhado, liquidado, pago),
                     \(x) sum(x, na.rm = TRUE)),
              .groups = "drop")
}

atual    <- padroniza(baixa_despesa(P$exercicio), P$exercicio)
anterior <- padroniza(baixa_despesa(P$exercicio - 1L), P$exercicio - 1L)

msg("Linhas coletadas: ", nrow(atual), " (t) e ", nrow(anterior), " (t-1)")

# Trava de sanidade: o orçamento federal tem milhares de ações. Se chegou aqui
# com poucas, alguma dimensão não foi reconhecida e o painel sairia vazio.
# Melhor interromper do que sobrescrever os dados bons com uma base quebrada.
n_acoes_distintas <- n_distinct(atual$acao_cod, na.rm = TRUE)
if (n_acoes_distintas < 200) {
  stop("Apenas ", n_acoes_distintas, " ações distintas reconhecidas. ",
       "Confira, no log acima, a linha 'Colunas devolvidas pelo SIOP' e ajuste ",
       "os tokens em dimensao() no alto deste script.")
}

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
        !!nome_perfil := {
          i <- which(mes <= mes_corrente)
          if (length(i)) acum[max(i)] / total[1] else NA_real_
        },
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


    # ---- classificação ------------------------------------------------------
    discricionaria = rp_cod %in% P$rp_discricionario,
    revisavel = discricionaria & !(funcao_cod %in% P$funcoes_excluidas)
  )

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

    n_sinais = rowSums(across(starts_with("op_")), na.rm = TRUE),

    # Espaço fiscal indicativo, por regra
    espaco = case_when(
      op_subexecucao  ~ folga_proj,
      op_simbolica    ~ dotacao - empenhado,
      op_credito      ~ dotacao - loa,
      op_expansao     ~ desvio_ano_rs,
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
      n_acoes       = n(),
      .groups = "drop"
    ) %>%
    mutate(
      desvio_loa_rs = empenhado - esperado_loa,
      desvio_loa_pc = ifelse(esperado_loa > 0, empenhado / esperado_loa - 1, NA_real_),
      desvio_ano_rs = ifelse(emp_ant_real > 0, empenhado - emp_ant_real, NA_real_),
      var_real      = ifelse(emp_ant_real > 0, empenhado / emp_ant_real - 1, NA_real_)
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
  ~id,              ~rotulo,                            ~descricao,
  "subexecucao",    "Subexecução persistente",          "Empenhou bem menos do que o ritmo desta altura do ano pedia, e também executou pouco no ano passado. Sinal de dotação superestimada.",
  "simbolica",      "Dotação sem execução",             "Tem dotação relevante e praticamente nada empenhado, em dois exercícios seguidos. O desenho da ação é o que está em questão.",
  "credito",        "Dotação inflada por créditos",     "A dotação atual ficou muito acima do que a LOA previu. Indica erro de previsão na proposta ou realocação ao longo do ano.",
  "expansao",       "Expansão real acelerada",          "Gastou bem mais do que no mesmo período do ano passado, já descontada a inflação. Pede exame dos parâmetros de custo e de quem tem direito.",
  "retracao",       "Retração real acentuada",          "Gastou bem menos do que no mesmo período do ano passado, em termos reais. Pode ser economia, mas também pode ser entrega travada.",
  "fragmentacao",   "Execução pulverizada",             "A mesma ação aparece com valores pequenos espalhados por muitas unidades. O custo de administrar tende a superar o benefício.",
  "dezembro",       "Concentração no fim do exercício", "No ano passado, boa parte do empenho saiu em dezembro. É o padrão clássico de gasto feito para não perder dotação."
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
    pago      = mil(sum(base$pago)),
    esperado  = mil(sum(base$esperado_loa, na.rm = TRUE)),
    ant       = mil(sum(base$emp_ant_real, na.rm = TRUE)),
    emp_comparavel = mil(sum(base$empenhado[!is.na(base$emp_ant_real) & base$emp_ant_real > 0])),
    n_sinalizadas  = sum(base$n_sinais > 0, na.rm = TRUE)
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
    prit = pc(desvio_ritmo), pano = pc(var_real),
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
  summarise(across(c(loa, dotacao, empenhado, liquidado, pago),
                   \(x) sum(x, na.rm = TRUE)), .groups = "drop") %>%
  mutate(data = hoje, .before = 1)

arq_ag <- "historico/serie_diaria_agregada.csv.gz"
if (file.exists(arq_ag)) {
  # Tipos explícitos: uma execução anterior pode ter gravado uma coluna vazia,
  # que o leitor interpretaria como lógica e recusaria juntar com texto.
  antigo <- tryCatch(
    read_csv(arq_ag, show_col_types = FALSE,
             col_types = cols(data = col_date(),
                              funcao_cod = col_character(),
                              gnd_cod = col_character(),
                              .default = col_double())) %>%
      filter(as.Date(data) != hoje),
    error = function(e) {
      msg("AVISO: histórico agregado ilegível (", conditionMessage(e), "). Recomeçando o arquivo.")
      NULL
    }
  )
  if (!is.null(antigo) && nrow(antigo)) snap_ag <- bind_rows(antigo, snap_ag)
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
