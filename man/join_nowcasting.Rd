% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/join_nowcasting.R
\name{join_nowcasting}
\alias{join_nowcasting}
\title{Join nowcasting}
\usage{
join_nowcasting(nowcasting, notified)
}
\arguments{
\item{nowcasting}{a data.frame containing multiple nowcasting series}

\item{notified}{an incidence table with dates and number of cases per day}
}
\description{
Join nowcasting
}
\details{
Creates a data.frame where the notified number of daily cases is replaced by the nowcasting series in their corresponding dates.
}
