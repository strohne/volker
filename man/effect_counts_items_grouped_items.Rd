% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effects.R
\name{effect_counts_items_grouped_items}
\alias{effect_counts_items_grouped_items}
\title{Effect size and test for comparing multiple variables by multiple grouping variables}
\usage{
effect_counts_items_grouped_items(
  data,
  cols,
  cross,
  method = "cramer",
  labels = TRUE,
  clean = TRUE,
  ...
)
}
\arguments{
\item{data}{A tibble containing item measures and grouping variable.}

\item{cols}{Tidyselect item variables (e.g. starts_with...).}

\item{cross}{The columns holding groups to compare.}

\item{method}{The output metrics: cramer = Cramer's V, f1 = F1-value (only for variable sets with the same labels).}

\item{labels}{If TRUE (default) extracts labels from the attributes, see \link{codebook}.}

\item{clean}{Prepare data by \link{data_clean}.}

\item{...}{Placeholder to allow calling the method with unused parameters from \link{effect_counts}.}
}
\value{
A volker tibble.
}
\description{
Effect size and test for comparing multiple variables by multiple grouping variables
}
\examples{
library(volker)
data <- volker::chatgpt

effect_counts(
  data,
  starts_with("cg_adoption_adv"),
  starts_with("use_")
)

}
\keyword{internal}
