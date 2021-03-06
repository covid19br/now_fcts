#' Função para gerar nowcasting
#'
#' @param dados dados
#' @param caso Lógico. Se TRUE faz casos e FALSE, óbitos
#' @param tipo covid ou srag
#' @param hospitalizados Lógico, filtra apenas hospitalizados
#' @param trim.now corte para nowcasting
#' @param window janela para nowcasting
#' @param trajectories Lógico. Retorna trajetórias
#' @param ... qualquer parâmetro da função NobBS para controlar o ajuste do nowcasting
#'
#' @export
#'
#' @importFrom stats dbinom
#' @importFrom NobBS NobBS
#' @importFrom dplyr filter select mutate
gera.nowcasting <- function(dados, # dados
                            caso = TRUE, # caso = FALSE faz obitos
                            tipo, # covid ou srag
                            hospitalizados = TRUE,
                            trim.now, # corte para nowcasting
                            window, # janela para nowcasting
                            trajectories = FALSE,
                            ...) { # retorna trajetórias
  if (trajectories)
      NobBS  <- NobBS.posterior
  # 1. nowcasting de casos ###
  if (caso) {
      if (hospitalizados)
        dados <- dados %>% filter(hospital == 1)
    ## 1.1 casos covid ####
    if (tipo == "covid") {
      ##COVID##
      dados2 <- dados %>%
        filter(pcr_sars2 == 1 | classi_fin == 5) %>% #covid com nova classificacao
        select(dt_notific, dt_sin_pri, dt_pcr, dt_digita) %>%
        mutate(dt_pcr_dig = pmax(dt_pcr, dt_digita, dt_notific, na.rm = TRUE))
    }
    ## 1.2. casos srag ####
    if (tipo == "srag") {
      ## %PIP data de registro é data mais recente entre notificação e digitação, não deve incluir data pcr (dt_pcr)
      ## pq SRAG não precisa de teste para ser confirmado
      dados2 <- dados %>%
        select(dt_notific, dt_sin_pri, dt_digita) %>%
        mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE)) # nome aqui é pcr mas não tem pcr
    }

    if (nrow(dados2) != 0) {
      dados.now <- NobBS(
        data = dados2,
        now = max(dados2$dt_sin_pri, na.rm = TRUE) - trim.now,
        onset_date = "dt_sin_pri",
        report_date = "dt_pcr_dig",
        units = "1 day",
        moving_window = window,
        ...)
    } else {
     dados.now <- NULL
    }

    # 2. nowcasting de obitos ####
  } else {
    ## 2.1. obitos covid ####
    if (tipo == "covid") {
      ##obitos COVID ####
      dados2 <- dados %>%
        filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
        filter(evolucao == 2) %>%
        filter(!is.na(dt_evoluca)) %>%
        mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                                 na.rm = TRUE)) %>%
        select(dt_evoluca, dt_notific, dt_encerra)
    }
    ## 2.2. obitos srag ####
    if (tipo == "srag") {
      dados2 <- dados %>%
        filter(evolucao == 2) %>%
        filter(!is.na(dt_evoluca)) %>%
        mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                                 na.rm = TRUE)) %>%
        select(dt_evoluca, dt_notific, dt_encerra)
    }
    if (nrow(dados2) != 0) {
      dados.now <- NobBS(
        data = dados2,
        now = max(dados2$dt_evoluca) - trim.now, ##PIP: nocwasting vai até última data do evento, no caso data do obito
        onset_date = "dt_evoluca",
        report_date = "dt_encerra",
        units = "1 day",
        moving_window = window,
        specs = list(beta.priors = dbinom(0:40, size = 40, prob = 15/50)),
        ...)
    } else {
      dados.now <- NULL
    }
  }
  out <- list(now = dados.now, dados = dados2)
  return(out)
}
