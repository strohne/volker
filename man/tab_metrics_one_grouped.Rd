% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{tab_metrics_one_grouped}
\alias{tab_metrics_one_grouped}
\title{Output a five point summary for groups}
\usage{
tab_metrics_one_grouped(
  data,
  col,
  cross,
  ci = FALSE,
  digits = 1,
  labels = TRUE,
  clean = TRUE,
  ...
)
}
\arguments{
\item{data}{A tibble.}

\item{col}{The column holding metric values.}

\item{cross}{The column holding groups to compare.}

\item{ci}{Whether to output 95\% confidence intervals.}

\item{digits}{The number of digits to print.}

\item{labels}{If TRUE (default) extracts labels from the attributes, see \link{codebook}.}

\item{clean}{Prepare data by \link{data_clean}.}

\item{...}{Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.}
}
\value{
A volker tibble.
}
\description{
Output a five point summary for groups
}
\examples{
library(volker)
data <- volker::chatgpt

tab_metrics_one_grouped(data, sd_age, sd_gender)

}
\keyword{internal}
