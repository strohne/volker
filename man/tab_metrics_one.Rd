% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{tab_metrics_one}
\alias{tab_metrics_one}
\title{Output a five point summary table for the values in multiple columns}
\usage{
tab_metrics_one(data, col, negative = F, digits = 1, labels = T, ...)
}
\arguments{
\item{data}{A tibble}

\item{col}{The columns holding metric values}

\item{negative}{If FALSE (default), negative values are recoded as missing values.}

\item{digits}{The number of digits to print.}

\item{labels}{If TRUE (default) extracts labels from the attributes, see \link{codebook}.}
}
\description{
Output a five point summary table for the values in multiple columns
}