## code to prepare `DATASET` dataset goes here
geocode_ibge <- read.csv("./data-raw/geocode_ibge.csv")
DRS_SP <- read.csv("./data-raw/DRS_SP.csv")
usethis::use_data(geocode_ibge, DRS_SP,internal = TRUE)

