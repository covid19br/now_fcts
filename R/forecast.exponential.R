#' Forecast usando regressão Poisson sobre série dos casos acumulados
#' @details Esta função ajusta uma regressão Poisson ao trecho final
#'     de uma série temporal e em seguida usa esta regressão para
#'     extrapolar a série para um certo de número de dias seguintes. A
#'     projeção deve ser usada com cautela por se baseada em um ajuste
#'     puramente estatístico, assumindo que (1) há uma relação inear
#'     da série temporal com o tempo; (2) que a relação linear
#'     ajustada se mantenah nos dias futuros para o qual é feita a
#'     projeção.
#' @param zoo.obj objeto zoo com série temporal acumulada
#' @param start data inicial do trecho da série temporal à qual é
#'     feito o ajuste da regressão Poisson
#' @param end data final do trecho da série temporal à qual é feito o
#'     ajuste da regressão Poisson.
#' @param days.forecast inteiro, número de dias após o fim da série
#'     para fazer a projeção.
#'
#' @export
#' @importFrom stats predict
forecast.exponential <- function(zoo.obj, start, end = length(zoo.obj), days.forecast){
    if(class(zoo.obj)!="zoo"|!is.null(dim(zoo.obj)))
        stop("'zoo.obj' deve ser um objeto da classe zoo com uma unica variavel")
    if(is.numeric(start))
        inicio <- time(zoo.obj)[start]
    else
        inicio <- start
    if(is.numeric(end))
        fim <- time(zoo.obj)[end]
    else
        fim <- end
    y <- window(zoo.obj, start = inicio, end = fim)
    if(!is.integer(y)) {
        warning("Resposta nao esta em inteiros, convertendo para inteiro para ajustar glm Poisson")
        y <- as.integer(y)
    }
    fit <- fitP.exp(y, only.coef = FALSE)
    datas.forecast <- fim + (1:days.forecast)
    newdata <- data.frame( ndias = as.vector( datas.forecast - inicio ) )
    pred <- predict(fit, newdata = newdata, se.fit = TRUE)
    df1 <- data.frame(lpred = pred$fit, lse = pred$se.fit)
    df1$predito <- exp(df1$lpred)
    df1$ic.low <-  with(df1, exp(lpred - 2*lse))
    df1$ic.upp <-  with(df1, exp(lpred + 2*lse))
    zoo(df1[,c("predito","ic.low","ic.upp")], datas.forecast)
}
