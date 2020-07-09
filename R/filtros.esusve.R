#' Title
#'
#' @param dados df
#' @param tipo covid, srag, obitos_covid, obitos_srag
#' @param remove.col Lógico. Se TRUE seleciona apenas colunas de datas usadas em nowcasting
#'
#' @importFrom dplyr filter select mutate
#' @importFrom magrittr `%>%`
#' @importFrom stringr str_detect
#' @export
#'
filtros.esusve <- function(dados, tipo, remove.col = TRUE) {

  if (!tipo %in% "covid") {
    stop("Tipo precisa ser covid")
  }

  # funcionando apenas para covid
  if (tipo == "covid")
    dados2 <- dados %>%
      filter(.data$resultadoteste == "Positivo" |
               stringr::str_detect(.data$classificacaofinal, "Confirma")) %>%
      mutate(dataencerramento = pmax(.data$datateste, .data$datanotificacao, .data$dataencerramento, na.rm = TRUE))

  # seleciona apenas colunas usadas no nowcasting
  if (remove.col) {
    dados2 <- dados2 %>%
      select(.data$datainiciosintomas, .data$datanotificacao, .data$datateste, .data$dataencerramento)
  }

  return(dados2)

}
