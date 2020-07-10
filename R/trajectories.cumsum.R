#' Médias e ICs das estimativas de nowcasting cumulativa a cada dia
#' @param NobBS.posterior.output objeto retornado pela função NobBS.posterior.
#' @param NobBS.posterior.output.trajectories trajetórias das estimativas de nowcasting a posteriori, 
#'                                            utilizadas no calculo de estimativas cumulativas
#' @param samples Número de samples a ser considerado.
#' @return data frame com média e quantis 2.5% e 97.5% das
#'     distribuições a posteriori das estimativas de nowcasting da função NobBS.posterior. 
#'     Os valores são cumulativos, e portanto podem ser interpretados como a estimativa cumulativa de casos
#'     ser notificados D dias após o dias o primeiro sintoma, sendo que
#'     vai de zero ao máximo definido pelos argumentos do nowcasting
#'@export
nowcasting.cumsum <- function(NobBS.posterior.output, NobBS.posterior.output.trajectories, samples){
  if(missing(NobBS.posterior.output.trajectories))
    df <- NobBS.posterior.output$trajectories
  else
    df <- NobBS.posterior.output.trajectories
  if (samples > ncol(df))
    stop(paste("samples deve ter tamanho menor ou igual a", ncol(df)))
  else
    df<-NobBS.posterior.output$trajectories[,-1]
  dates<-NobBS.posterior.output$estimates$onset_date
  df1<-df[,sample(ncol(df), samples)]
  df2<-apply(df1, 2, cumsum)
  data.frame(Dates = dates,
             mean = apply(df2, 1, mean),
             lower = apply(df2, 1, quantile, 0.025),
             upper = apply(df2, 1, quantile, 0.975))
}
