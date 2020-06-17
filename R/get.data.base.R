# extrai a data mais recente de nowcasting
get.data.base <- function(names) {
  data.base <- names %>%
    stringr::str_extract("(19|20)\\d\\d[_ /.](0[1-9]|1[012])[_ /.](0[1-9]|[12][0-9]|3[01])") %>%
    as.Date(format = "%Y_%m_%d") %>%
    format("%Y_%m_%d")
}
