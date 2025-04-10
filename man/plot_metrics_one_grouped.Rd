% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_metrics_one_grouped}
\alias{plot_metrics_one_grouped}
\title{Output averages for multiple variables}
\usage{
plot_metrics_one_grouped(
  data,
  col,
  cross,
  ci = FALSE,
  box = FALSE,
  limits = NULL,
  title = TRUE,
  labels = TRUE,
  clean = TRUE,
  ...
)
}
\arguments{
\item{data}{A tibble.}

\item{col}{The column holding metric values.}

\item{cross}{The column holding groups to compare.}

\item{ci}{Whether to add error bars with 95\% confidence intervals.}

\item{box}{Whether to add boxplots.}

\item{limits}{The scale limits. Set NULL to extract limits from the labels.}

\item{title}{If TRUE (default) shows a plot title derived from the column labels.
Disable the title with FALSE or provide a custom title as character value.}

\item{labels}{If TRUE (default) extracts labels from the attributes, see \link{codebook}.}

\item{clean}{Prepare data by \link{data_clean}.}

\item{...}{Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.}
}
\value{
A ggplot object.
}
\description{
Output averages for multiple variables
}
\examples{
library(volker)
data <- volker::chatgpt

plot_metrics_one_grouped(data, sd_age, sd_gender)

}
\keyword{internal}
