#' rwaittime
#'
#' @param n
#' @param fit
#'
#'
rwaittime <-  function(n, fit) {
  mu = fixef(fit)[1,"Estimate"]
  shape = mean(as.data.frame(fit)$shape)
  lambda = exp(mu) / gamma( 1 + 1/shape )
  stats::rweibull(n, shape = shape, scale = lambda)
}

#' rwaittime_posterior_age
#'
#' @param n
#' @param age
#' @param fit
#'
#'
rwaittime_posterior_age <- function(n, age, fit) {
  model_matrix = as.matrix(fit)
  x = model_matrix[1, ]
  plyr::aaply(model_matrix, 1, function(x) {
    random_int = x[grep(age, names(x))]
    if (length(random_int) == 0)
      random_int = 0
    mu = x["b_Intercept"] + random_int
    shape = x["shape"]
    lambda = exp(mu) / gamma(1 + 1 / shape)
    stats::rweibull(n, shape = shape, scale = lambda)
  })
}

#' rwaittime_age
#'
#' @param n
#' @param age
#' @param fit
#'
#' @export
#'
rwaittime_age <- function(n, age, fit) {
  mu = stats::coef(fit)$age_class[age,"Estimate","Intercept"]
  shape = summary(fit)$spec_pars[1]
  lambda = exp(mu) / gamma( 1 + 1/shape )
  stats::rweibull(n, shape = shape, scale = lambda)
}

