#' zoo2df funcao para converter zoo em df
#'
#' @param zoo a zoo object
#'
#' @export
#'
zoo2df <- function(zoo) {
  df <- as.data.frame(zoo)
  df$data <- zoo::as.Date(row.names(df))
  return(df)
}
