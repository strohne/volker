% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tables.R
\name{tab_metrics_items_grouped}
\alias{tab_metrics_items_grouped}
\title{Output the means for groups in one or multiple columns}
\usage{
tab_metrics_items_grouped(
  data,
  cols,
  col_group,
  negative = F,
  values = c("m", "sd"),
  digits = 1,
  labels = T,
  ...
)
}
\arguments{
\item{data}{A tibble}

\item{cols}{The item columns that hold the values to summarize}

\item{negative}{If FALSE (default), negative values are recoded as missing values.}

\item{values}{The output metrics, mean (m), the standard deviation (sd) or both (the default).}

\item{digits}{The number of digits to print.}

\item{labels}{If TRUE (default) extracts labels from the attributes, see \link{codebook}.}

\item{cols_group}{The column holding groups to compare}
}
\description{
TODO: handle completely missing data in single groups
}