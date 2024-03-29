% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_counts_one_grouped}
\alias{plot_counts_one_grouped}
\title{Plot frequencies cross tabulated with a grouping column}
\usage{
plot_counts_one_grouped(
  data,
  col,
  col_group,
  category = NULL,
  ordered = NULL,
  missings = FALSE,
  prop = "total",
  numbers = NULL,
  title = TRUE,
  labels = TRUE,
  clean = TRUE,
  ...
)
}
\arguments{
\item{data}{A tibble}

\item{col}{The column holding factor values}

\item{col_group}{The column holding groups to compare}

\item{category}{The value FALSE will force to plot all categories.
A character value will focus a selected category.
When NULL, in case of boolean values, only the TRUE category is plotted.}

\item{ordered}{Values can be nominal (0) or ordered ascending (1) descending (-1).
By default (NULL), the ordering is automatically detected.
An appropriate color scale should be choosen depending on the ordering.
For unordered values, the default scale is used.
For ordered values, shades of the VLKR_FILLGRADIENT option are used.}

\item{missings}{Include missing values (default FALSE)}

\item{prop}{The basis of percent calculation: "total" (the default), "rows" or "cols".
Plotting row or column percentages results in stacked bars that add up to 100\%.
Whether you set rows or cols determines which variable is in the legend (fill color)
and which on the vertical scale.}

\item{numbers}{The numbers to print on the bars: "n" (frequency), "p" (percentage) or both.}

\item{title}{If TRUE (default) shows a plot title derived from the column labels.
Disable the title with FALSE or provide a custom title as character value.}

\item{labels}{If TRUE (default) extracts labels from the attributes, see \link{codebook}.}

\item{clean}{Prepare data by \link{data_clean}.}

\item{...}{Placeholder to allow calling the method with unused parameters from \link{plot_counts}.}
}
\value{
A ggplot object
}
\description{
Note: only non-missing cases are used to calculate the percentage.
}
\examples{
library(volker)
data <- volker::chatgpt

plot_counts_one_grouped(data, adopter, sd_gender)

}
\keyword{internal}
