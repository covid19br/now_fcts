#' makeNamedList
#'
#' @param ... ...
#'
#' @export
#'
makeNamedList <- function(...) {
  structure(list(...), names = as.list(substitute(list(...)))[-1L])
}
