#' Função para checar se existem dados de nowcasting para a unidade administrativa
#'
#' @param tipo covid, srag, obitos_covid, obitos_srag
#' @param data data
#' @param output.dir diretorio para checar se existem dados de nowcasting para a unidade administrativa
#' @export
existe.nowcasting2 <- function(tipo,
                               data,
                               output.dir) {
  nowcasting.file <- list.files(path = output.dir,
                                pattern = paste0("nowcasting_acumulado_", tipo, "_20"))
  length(nowcasting.file) > 0
}
