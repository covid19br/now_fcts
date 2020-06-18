#' substitui NAS por zeros
#'
#' @param x x
na.zero <- function(x) {
    ifelse(is.na(x), 0, x)
}
