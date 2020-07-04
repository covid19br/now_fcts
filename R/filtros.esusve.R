#' Title
#'
#' @param dados df
#' @param tipo covid, srag, obitos_covid, obitos_srag
#'
#' @importFrom dplyr filter select mutate
#' @importFrom magrittr `%>%`
#' @export
#'
filtros.esusve <- function(dados, tipo) {

  if (tipo == "covid")
    dados2 <- dados %>%
      filter(resultadoteste == "Positivo" |
               str_detect(classificacaofinal, "Confirma")) %>%
      select (datainiciosintomas, datanotificacao, datateste, dataencerramento) %>%
      mutate (dataencerramento = pmax(datateste, datanotificacao, dataencerramento, na.rm=TRUE))


  return(dados2)
}
