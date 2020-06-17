#' Inverso da logit
#'
#' @param x vector
#'
#' @return
#' @export
#'
inv.logit <- function(x) {
    exp(x)/(1+exp(x))
}
