#' Title
#'
#' @param dados df
#' @param tipo covid, srag, obitos_covid, obitos_srag
#' @param hospitalizados Lógico. Se TRUE filtra apenas casos hospitalizados
#' @param remove.col Lógico. Se TRUE seleciona apenas colunas de datas usadas em nowcasting
#'
#' @importFrom dplyr filter select mutate
#' @export
#'
filtros.sivep <- function(dados, tipo, hospitalizados, remove.col) {

  if (hospitalizados)
    dados <- dados %>% filter(hospital == 1)

  if (tipo == "covid") {
    dados2 <- dados %>%
      filter(pcr_sars2 == 1 | classi_fin == 5) %>% #covid com nova classificacao
      mutate(dt_pcr_dig = pmax(dt_pcr, dt_digita, dt_notific, na.rm = TRUE))
    if (remove.col) {
      dados2 <- dados2 %>%
        select(dt_notific, dt_sin_pri, dt_pcr, dt_digita)
    }
  }

  if (tipo == "srag") {
    dados2 <- dados %>%
      mutate(dt_pcr_dig = pmax(dt_digita, dt_notific, na.rm = TRUE))
    if (remove.col) {
      dados2 <- dados2 %>%
        select(dt_notific, dt_sin_pri, dt_digita)
    }
  }

  if (tipo == "obitos.covid") {
    dados2 <- dados %>%
      filter(pcr_sars2 == 1 | classi_fin == 5) %>% # covid com nova classificacao
      filter(evolucao == 2) %>%
      filter(!is.na(dt_evoluca)) %>%
      mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                               na.rm = TRUE))
    if (remove.col) {
      dados2 <- dados2 %>%
        select(dt_evoluca, dt_notific, dt_encerra)
    }
  }

  if (tipo == "obitos.srag") {
    dados2 <- dados %>%
      filter(evolucao == 2) %>%
      filter(!is.na(dt_evoluca)) %>%
      mutate(dt_encerra = pmax(dt_encerra, dt_digita, dt_evoluca,
                               na.rm = TRUE))
    if (remove.col) {
      dados2 <- dados2 %>%
        select(dt_evoluca, dt_notific, dt_encerra)
    }
  }

  return(dados2)

}
