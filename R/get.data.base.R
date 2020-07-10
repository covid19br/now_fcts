#' Extrai as datas de um vetor (para pegar a última data ver get.last.date)
#'
#' @param names vector
#' @param last Logical, whether the most recent date of the vector should be returned
#'
#' @export
get.data.base <- function(names, last = FALSE) {
  data.base <- names %>%
    stringr::str_extract("(19|20)\\d\\d[_ /.](0[1-9]|1[012])[_ /.](0[1-9]|[12][0-9]|3[01])") %>%
    as.Date(format = "%Y_%m_%d") %>%
    format("%Y_%m_%d")
    if (last)
    data.base <- max(data.base)
    return(data.base)
}
