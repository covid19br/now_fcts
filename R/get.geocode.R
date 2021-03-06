#' get geocode
#'
#' @param nome_municipio nome do municipio
#' @param nome_estado nome do estado
#' @param sigla sigla do estado
#' @param cifras número de cifras do geocode do município, o valor predeterminado é 7 mas aceita 6
#'
#' @importFrom textclean replace_non_ascii
#' @export
get.geocode <- function(nome_municipio,
                        nome_estado = NULL,
                        sigla = NULL,
                        cifras = 7) {
 df <- geocode_ibge
 if (cifras == 6) municipio.code <- df$municipio.code
 if (cifras == 7) municipio.code <- df$municipio.geocode


 nome <- textclean::replace_non_ascii(nome_municipio)
 nome <- gsub(" ", "_", nome)
 nome <- gsub("'", "", nome)

 if (is.null(sigla) & is.null(nome_estado))
   stop("either nome_estado or sigla should be provided")
 if (!is.null(sigla) & is.null(nome_estado)) {
 geocode <- municipio.code[which(df$nome.nonascii == nome &
                                   df$microrregiao.mesorregiao.UF.sigla == sigla)]
 }
 if (is.null(sigla) & !is.null(nome_estado)) {
 estado <- textclean::replace_non_ascii(tolower(nome_estado))
 geocode <- municipio.code[which(df$nome.nonascii == nome &
                                   df$microrregiao.mesorregiao.UF.nome == nome_estado)]
 }
 if (length(geocode) == 0) stop(paste("no geocode found for" , nome_municipio, sigla, nome_estado))
 else return(geocode)
}
