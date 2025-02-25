% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_counts_items_grouped}
\alias{plot_counts_items_grouped}
\title{Plot percent shares of multiple items compared by groups}
\usage{
plot_counts_items_grouped(
  data,
  cols,
  cross,
  category = NULL,
  title = TRUE,
  labels = TRUE,
  clean = TRUE,
  ...
)
}
\arguments{
\item{data}{A tibble containing item measures.}

\item{cols}{Tidyselect item variables (e.g. starts_with...).}

\item{cross}{The column holding groups to compare.}

\item{category}{Summarizing multiple items (the cols parameter) by group requires a focus category.
By default, for logical column types, only TRUE values are counted.
For other column types, the first category is counted.
To override the default behavior, provide a vector of values in the dataset or labels from the codebook.}

\item{title}{If TRUE (default) shows a plot title derived from the column labels.
Disable the title with FALSE or provide a custom title as character value.}

\item{labels}{If TRUE (default) extracts labels from the attributes, see \link{codebook}.}

\item{clean}{Prepare data by \link{data_clean}.}

\item{...}{Placeholder to allow calling the method with unused parameters from \link{plot_counts}.}
}
\value{
A ggplot object.
}
\description{
Plot percent shares of multiple items compared by groups
}
\examples{
library(volker)
data <- volker::chatgpt
plot_counts_items_grouped(
  data, starts_with("cg_adoption_"), adopter,
  category=c("agree","strongly agree")
)

plot_counts_items_grouped(
  data, starts_with("cg_adoption_"), adopter,
  category=c(4,5)
)

}
\keyword{internal}
