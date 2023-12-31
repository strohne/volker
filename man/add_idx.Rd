% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/idx.R
\name{add_idx}
\alias{add_idx}
\title{Calculate the mean value of multiple items}
\usage{
add_idx(data, cols, newcol = NULL, .negative = FALSE)
}
\arguments{
\item{data}{A dataframe}

\item{cols}{A tidy selection of item columns}

\item{newcol}{Name of the index as a character value.
Set to NULL (default) to automatically build a name
from the common column prefix, prefixed with "idx_"}

\item{.negative}{If TRUE, all negative values are recoded to NA}
}
\description{
Calculate the mean value of multiple items
}
