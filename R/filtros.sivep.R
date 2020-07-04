#' Title
#'
#' @param dados df
#' @param tipo covid, srag, obitos_covid, obitos_srag
#' @param hospitalizados Logical. If TRUE filters only hosptalized cases
#'
#' @importFrom dplyr filter select mutate
#' @export
#'
filtros.sivep <- function(dados, tipo, hospitalizados) {
  if (hospitalizados)
    dados <- dados %>% filter(hospital == 1)

  if (tipo == "covid")
    dados2 <- dados %>%
    filter(pcr_sars2 == 1 | classi_fin == 5) %>% #covid com nova classificacao
    select(dt_notific, dt_sin_pri, dt_pcr, dt_digita) %>%
    mutate(dt_pcr_dig = pmax(dt_pcr, dt_digita, dt_notific, na.rm = TRUE))

  if (tipo == "srag")
    dados2 <- dados %>%
      select(dt_notific, dt_sin_pri, dt_digita) %>%
      mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE))

  if (tipo == "obitos.covid")
    dados2 <- dados %>%
      filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
      filter(evolucao == 2) %>%
      filter(!is.na(dt_evoluca)) %>%
      mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                               na.rm = TRUE)) %>%
      select(dt_evoluca, dt_notific, dt_encerra)

  if (tipo == "obitos.srag")
    dados2 <- dados %>%
      filter(evolucao == 2) %>%
      filter(!is.na(dt_evoluca)) %>%
      mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                               na.rm = TRUE)) %>%
      select(dt_evoluca, dt_notific, dt_encerra)

  return(dados2)
}
