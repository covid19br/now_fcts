#' Corta uma série temporal no dia zero.
#'
#' @param zoo.obj objeto da classe zoo
#' @param limite limite para corte
#'
#' @details Esta função corta um objeto da classe zoo, tirando todos
#'     os valores anteriores ao primeiro valor igual a um certo limite
#'     (n.casos).
#' @export
diazero <- function(zoo.obj, limite){
    dia.zero <- min(which(zoo.obj >= limite, arr.ind = TRUE))
    zoo.obj[dia.zero:length(zoo.obj)]
}
