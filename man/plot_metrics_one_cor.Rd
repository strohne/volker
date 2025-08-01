% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plots.R
\name{plot_metrics_one_cor}
\alias{plot_metrics_one_cor}
\title{Correlate two items}
\usage{
plot_metrics_one_cor(
  data,
  col,
  cross,
  limits = NULL,
  log = FALSE,
  jitter = FALSE,
  title = TRUE,
  labels = TRUE,
  clean = TRUE,
  ...
)
}
\arguments{
\item{data}{A tibble.}

\item{col}{The first column holding metric values.}

\item{cross}{The second column holding metric values.}

\item{limits}{The scale limits, a list with x and y components, e.g. \code{list(x=c(0,100), y=c(20,100))}.
Set NULL to extract limits from the labels.}

\item{log}{Whether to plot log scales.}

\item{jitter}{Whether to jitter the points.}

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
Correlate two items
}
\examples{
library(volker)
data <- volker::chatgpt

plot_metrics_one_cor(data, use_private, sd_age)

}
\keyword{internal}
