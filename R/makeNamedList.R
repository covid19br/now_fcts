#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
makeNamedList <- function(...) {
  structure(list(...), names = as.list(substitute(list(...)))[-1L])
}
