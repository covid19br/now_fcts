% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/check.geocode.R
\name{check.geocode}
\alias{check.geocode}
\title{Função para extrair nome do path a partir do geocode}
\usage{
check.geocode(escala, geocode, sigla, nonascii = TRUE)
}
\arguments{
\item{escala}{Caractere. Escala do filtro: \verb{"municipio}, \code{"estado"}, \code{"micro"}, \code{"meso"}}

\item{geocode}{Caractere. Geocode IBGE. Município 6 ou 7 dígitos; microrregião 5 dígitos; mesorregião 4 dígitos; estado 2 dígitos}

\item{sigla}{Sigla do estado}

\item{nonascii}{Lógico. Se TRUE retorna nome sem caracteres ascii}
}
\description{
Função para extrair nome do path a partir do geocode
}
