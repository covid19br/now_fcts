#' Writes nowcasting
#'
#' @param now now
#' @param output.dir output.dir
#' @param tipo tipo
#' @param data data
#'
#' @importFrom utils write.csv
#' @export
#'
write.nowcasting <- function(now,
                             output.dir,
                             tipo = "covid",
                             data) {

  nome.now.df <- paste0(output.dir, "nowcasting_", tipo, "_previstos_", data, ".csv")
  write.csv(now$estimates,
            file = nome.now.df,
            row.names = FALSE)

  nome.now.post <- paste0(output.dir, "nowcasting_", tipo, "_post_", data, ".csv")
  write.csv(now$params.post,
            file = nome.now.post,
            row.names = FALSE)
}
