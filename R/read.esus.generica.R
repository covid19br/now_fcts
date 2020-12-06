#' Função para ler base da eSUS-VE
#'
#' @param file.name file name
#' @param encoding encoding
#' @param ... ...
#'
#' @importFrom readr read_delim guess_encoding problems
#' @importFrom lubridate as_date parse_date_time
#' @importFrom utils unzip count.fields
#'
#' @export
#'
read.esus.generica  <- function(file.name,
                                encoding = NULL,
                                convert.dates = TRUE,
                                ...) {

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

  # detecta o encoding.
  if (is.null(encoding)) {
  encoding <- guess_encoding(file.name, n_max = 500)

  if (nrow(encoding) == 0) {
    encoding <- "UTF-8"
  } else {
    encoding <- data.frame(encoding)
    encoding <- as.character(encoding[which.max(encoding$confidence), "encoding"])
  }
  }
  dados <- readr::read_delim(file = file.name,
                             delim = sep,
                             escape_double = FALSE,
                             trim_ws = TRUE,
                             n_max = 1,
                             locale = readr::locale(encoding = encoding))
  nomes <- names(dados)
  cols <- rep("c", length(nomes))
  names(cols) <- nomes
  cols.list <- as.list(cols)
  dados <- readr::read_delim(file = file.name,
                             delim = sep,
                             col_types = cols.list,
                             escape_double = FALSE,
                             trim_ws = TRUE,
                             locale = readr::locale(encoding = encoding)
  )
  #pega as linhas problemáticas
  index_prob <- problems(dados)$row

  #dados <- dados[-index_prob,]
  #precisamos filtrar por linhas problemáticas sem ser index_prob - tipo checar se estados não está no vetor de estados

  dados <- data.frame(dados)
  # conveniencia mudando para minusculas
  names(dados) <- tolower(names(dados))
  names(dados) <- gsub(" ", "_", names(dados))
  names(dados) <- gsub("\\.", "_", names(dados))

  if (is_zip)
    file.remove(file.name)

  if(convert.dates){ ## PI: permite nao manter as datas como strings (util para exportar para SQLite)
      ## formata datas
      ## muda nome de colunas e formata datas
      dt.cols <- names(dados)[grepl("data", names(dados))]
      ##esusve: "datanotificacao"    "datainiciosintomas" "datanascimento"  "datateste"  "dataencerramento"
      ## usa lubridate
      dados[, dt.cols] <- lapply(dados[, dt.cols],
                                 function(x)
                                     as_date(parse_date_time(x, c("dmy", "ymd", "mdy", "dmy HMs", "ymd HMs"))))
      }
  dados$classificacaofinal <- gsub("Confirma.*", "Confirma", dados$classificacaofinal)
  return(dados)
}
