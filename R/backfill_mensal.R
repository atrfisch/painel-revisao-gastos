# =============================================================================
# Backfill do histórico mensal por ação
#
# O SIOP devolve valores acumulados no momento da consulta, sem dimensão de mês.
# Isso basta para o eixo "desvio contra a LOA", mas não permite comparar o
# acumulado de hoje com o acumulado do mesmo período do ano anterior.
#
# Este script preenche essa lacuna com a execução mensal consolidada da despesa
# publicada em dados abertos pelo Portal da Transparência, e grava
# historico/mensal_acao.csv.gz, que o pipeline diário consome.
#
# Rode uma vez (workflow "Backfill mensal", acionado manualmente) e depois
# uma vez por mês, quando o Portal publica o mês fechado.
# =============================================================================

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(stringr); library(purrr); library(tidyr)
})

options(timeout = 1800, scipen = 999)

ANOS <- as.integer(Sys.getenv("ANOS_BACKFILL",
                              paste(seq(as.integer(format(Sys.Date(), "%Y")) - 2,
                                        as.integer(format(Sys.Date(), "%Y"))),
                                    collapse = ",")) %>%
                     str_split(",", simplify = TRUE))

URL_BASE <- "https://portaldatransparencia.gov.br/download-de-dados/despesas-execucao/%s"

msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")

# Acha uma coluna pelo padrão do nome, ignorando acento e caixa.
acha <- function(nomes, ...) {
  alvo <- str_to_lower(iconv(nomes, to = "ASCII//TRANSLIT"))
  for (p in c(...)) {
    hit <- which(str_detect(alvo, p))
    if (length(hit)) return(nomes[hit[1]])
  }
  NA_character_
}

le_mes <- function(ano, mes) {
  ref <- sprintf("%d%02d", ano, mes)
  url <- sprintf(URL_BASE, ref)
  tmp <- tempfile(fileext = ".zip")
  ok <- tryCatch({
    download.file(url, tmp, mode = "wb", quiet = TRUE); TRUE
  }, error = function(e) FALSE)
  if (!ok || file.size(tmp) < 1000) {
    msg("  ", ref, ": indisponível, pulando")
    return(NULL)
  }
  destino <- tempfile(); dir.create(destino)
  utils::unzip(tmp, exdir = destino)
  csv <- list.files(destino, pattern = "\\.csv$", full.names = TRUE, recursive = TRUE)
  if (!length(csv)) { msg("  ", ref, ": zip sem csv"); return(NULL) }

  df <- read_delim(csv[1], delim = ";", locale = locale(encoding = "latin1",
                                                        decimal_mark = ",",
                                                        grouping_mark = "."),
                   show_col_types = FALSE, progress = FALSE)
  n <- names(df)
  sel <- list(
    orgao_cod  = acha(n, "codigo orgao(?! superior)", "codigo orgao"),
    uo_cod     = acha(n, "codigo unidade orcamentaria", "codigo unidade gestora"),
    funcao_cod = acha(n, "codigo funcao"),
    acao_cod   = acha(n, "codigo acao"),
    gnd_cod    = acha(n, "codigo grupo de despesa", "codigo grupo"),
    empenhado  = acha(n, "empenhado"),
    liquidado  = acha(n, "liquidado", "realizado"),
    pago       = acha(n, "pago")
  )
  falta <- names(sel)[is.na(unlist(sel))]
  if (length(falta)) msg("  ", ref, ": colunas ausentes -> ", paste(falta, collapse = ", "))

  pick <- function(k, num = FALSE) {
    if (is.na(sel[[k]])) return(if (num) 0 else NA_character_)
    v <- df[[sel[[k]]]]
    if (num) suppressWarnings(as.numeric(v)) else as.character(v)
  }

  tibble(
    exercicio  = ano, mes = mes,
    orgao_cod  = pick("orgao_cod"),
    uo_cod     = pick("uo_cod"),
    funcao_cod = pick("funcao_cod"),
    acao_cod   = pick("acao_cod"),
    gnd_cod    = pick("gnd_cod"),
    empenhado  = pick("empenhado", TRUE),
    liquidado  = pick("liquidado", TRUE),
    pago       = pick("pago", TRUE)
  ) %>%
    mutate(across(c(empenhado, liquidado, pago), ~ replace_na(.x, 0))) %>%
    group_by(exercicio, mes, orgao_cod, uo_cod, funcao_cod, acao_cod, gnd_cod) %>%
    summarise(across(c(empenhado, liquidado, pago), sum), .groups = "drop")
}

msg("Baixando execução mensal para: ", paste(ANOS, collapse = ", "))
todos <- map_dfr(ANOS, function(a) {
  msg("Ano ", a)
  map_dfr(1:12, function(m) {
    if (a == as.integer(format(Sys.Date(), "%Y")) && m > as.integer(format(Sys.Date(), "%m")))
      return(NULL)
    le_mes(a, m)
  })
})

if (!nrow(todos)) stop("Nenhum mês baixado. Verifique a URL do Portal da Transparência.")

# O arquivo do Portal pode vir acumulado no ano ou com o valor do mês.
# Detecta comparando a mediana do crescimento mês a mês e, se for acumulado,
# converte para fluxo mensal por diferenciação.
teste <- todos %>%
  group_by(exercicio, acao_cod) %>%
  filter(n() >= 6) %>%
  arrange(mes, .by_group = TRUE) %>%
  summarise(cresce = mean(diff(empenhado) >= 0, na.rm = TRUE), .groups = "drop")
acumulado <- nrow(teste) > 0 && median(teste$cresce, na.rm = TRUE) > 0.9

if (acumulado) {
  msg("Série detectada como ACUMULADA no ano; convertendo para fluxo mensal.")
  todos <- todos %>%
    group_by(exercicio, orgao_cod, uo_cod, funcao_cod, acao_cod, gnd_cod) %>%
    arrange(mes, .by_group = TRUE) %>%
    mutate(across(c(empenhado, liquidado, pago), ~ .x - lag(.x, default = 0))) %>%
    ungroup()
} else {
  msg("Série detectada como FLUXO mensal; mantida como está.")
}

dir.create("historico", showWarnings = FALSE)
write_csv(todos, "historico/mensal_acao.csv.gz")
msg("Gravado historico/mensal_acao.csv.gz com ", nrow(todos), " linhas")
