#' Writes nowcasting
#'
#' @param now now
#' @param output.dir output.dir
#' @param tipo tipo de caso/obito: "covid", "srag", "obitos_covid", "obitos_srag"
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

  # seria melhor usar um argumento?
  if ("trajectories" %in% names(now)){
    nome.now.traj <- paste0(output.dir, "nowcasting_", tipo, "_traj_", data, ".csv")
    write.csv(now$trajectories,
              file = nome.now.traj,
              row.names = FALSE)
  }
}
