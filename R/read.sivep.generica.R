#' Função para ler base da sivep
#'
#' @param file.name file name
#' @param ... ...
#'
#' @importFrom readr read_delim
#' @importFrom lubridate as_date parse_date_time
#' @importFrom utils unzip count.fields
#'
read.sivep.generica <- function(file.name,
                       ...) {
  # lendo os dados

  # detecta e lida com arquivo zip
  if (endsWith(file.name, '.zip')) {
    is_zip <- TRUE
    file.name <- unzip(file.name, basename(gsub('.zip$', '.csv', file.name)))
  } else {
    is_zip <- FALSE
  }
  # detecta separador
  linha1 <- readLines(file.name, n = 2)[2]
  if (count.fields(textConnection(linha1), sep = ',') >
      count.fields(textConnection(linha1), sep = ';')) {
    sep <- ','
  } else
    sep <- ';'


  dados <- read_delim(file = file.name,
                      delim = sep,
                      escape_double = FALSE,
                      trim_ws = TRUE,
                      n_max = 1,
                      guess_max = 30000)
  nomes <- names(dados)
  cols <- rep("c", length(nomes))
  names(cols) <- nomes
  cols.list <- as.list(cols)
  dados <- read_delim(file = file.name,
                      delim = sep,
                      col_types = cols.list,
                      escape_double = FALSE,
                      trim_ws = TRUE)
  #linhas problemáticas
  index_nada <- problems(dados)$row

  dados <- data.frame(dados)
  # conveniencia mudando para minusculas
  names(dados) <- tolower(names(dados))
  names(dados) <- gsub(" ", "_", names(dados))
  names(dados) <- gsub("\\.", "_", names(dados))

  if(is_zip)
    file.remove(file.name)

  # formata datas
  # muda nome de colunas e formata datas
  dt.cols <- names(dados)[grepl("dt_", names(dados))]
  ## usa lubridate
  dados[, dt.cols] <- lapply(dados[, dt.cols],
                             function(x)
                               as_date(parse_date_time(x, c("dmy", "ymd", "mdy", "dmy HMs", "ymd HMs"))))
  return(dados)
}


