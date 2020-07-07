#' Title
#'
#' @param dados df
#' @param tipo covid, srag, obitos_covid, obitos_srag
#'
#' @importFrom dplyr filter select mutate
#' @importFrom magrittr `%>%`
#' @importFrom stringr str_detect
#' @export
#'
filtros.esusve <- function(dados, tipo) {

  if (tipo == "covid")
    dados2 <- dados %>%
      filter(.data$resultadoteste == "Positivo" |
               stringr::str_detect(.data$classificacaofinal, "Confirma")) %>%
      select(.data$datainiciosintomas, .data$datanotificacao, .data$datateste, .data$dataencerramento) %>%
      mutate(dataencerramento = pmax(.data$datateste, .data$datanotificacao, .data$dataencerramento, na.rm = TRUE))
  return(dados2)
}
