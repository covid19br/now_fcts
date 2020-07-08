## code to prepare `DATASET` dataset goes here
geocode_ibge <- read.csv("./data-raw/geocode_ibge.csv")
geocode_ibge$municipio.code <- sapply(geocode_ibge$id, function(x) substr(x, start = 1, stop = 6))

#ainda nao se se precisa mesmo separar se já está lá
micro.code   <- geocode_ibge$microrregiao.id
meso.code    <- geocode_ibge$microrregiao.mesorregiao.id
estado.code  <- geocode_ibge$microrregiao.mesorregiao.UF.id
estado.sigla <- geocode_ibge$microrregiao.mesorregiao.UF.sigla

DRS_SP <- read.csv("./data-raw/DRS_SP.csv")
usethis::use_data(geocode_ibge,
                  DRS_SP,
                  #micro.code,
                  #meso.code,
                  #estado.code,
                  #estado.sigla,
                  internal = TRUE)

