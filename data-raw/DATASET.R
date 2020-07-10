## code to prepare `DATASET` dataset goes here

#geocodes
#url <- paste0("https://servicodados.ibge.gov.br/api/v1/localidades/municipios")
#df <- jsonlite::fromJSON(url)
#df$nome.nonascii <- gsub(" ", "_", textclean::replace_non_ascii(df$nome))
#df$nome.nonascii <- gsub("'", "", df$nome.nonascii)
#write.csv(df, "./dados/geocode_ibge.csv", row.names = FALSE)
geocode_ibge <- read.csv("./data-raw/geocode_ibge.csv")
geocode_ibge$municipio.code <- sapply(geocode_ibge$id, function(x) substr(x, start = 1, stop = 6))
geocode_ibge$municipio.geocode <- sapply(geocode_ibge$id, function(x) substr(x, start = 1, stop = 7))

#ainda nao se se precisa mesmo separar se já está lá
micro.code   <- geocode_ibge$microrregiao.id
meso.code    <- geocode_ibge$microrregiao.mesorregiao.id
estado.code  <- geocode_ibge$microrregiao.mesorregiao.UF.id
estado.sigla <- geocode_ibge$microrregiao.mesorregiao.UF.sigla

#DRS
DRS_SP <- read.csv("./data-raw/DRS_SP.csv")

#R efetivo tables
nd <- read.table("./data-raw/nishi_si_table.txt", header = TRUE)
nishi_si <- read.table("./data-raw/nishi_si_posterior.txt", header = TRUE)


usethis::use_data(geocode_ibge,
                  DRS_SP,
                  micro.code,
                  meso.code,
                  estado.code,
                  estado.sigla,
                  nd,
                  nishi_si,
                  internal = TRUE,
                  overwrite = TRUE)

