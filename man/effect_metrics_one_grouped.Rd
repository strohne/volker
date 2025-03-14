% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effects.R
\name{effect_metrics_one_grouped}
\alias{effect_metrics_one_grouped}
\title{Output a regression table with estimates and macro statistics}
\usage{
effect_metrics_one_grouped(
  data,
  col,
  cross,
  method = "lm",
  labels = TRUE,
  clean = TRUE,
  ...
)
}
\arguments{
\item{data}{A tibble.}

\item{col}{The column holding metric values.}

\item{cross}{The column holding groups to compare.}

\item{method}{A character vector of methods, e.g. c("t.test","lm").
Supported methods are t.test (only valid if the cross column contains two levels)
and lm (regression results).}

\item{labels}{If TRUE (default) extracts labels from the attributes, see \link{codebook}.}

\item{clean}{Prepare data by \link{data_clean}.}

\item{...}{Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.}
}
\value{
A volker list object containing volker tables with the requested statistics.
}
\description{
The regression output comes from \code{stats::\link[stats:lm]{lm}}.
T-test is performed using \code{stats::\link[stats:t.test]{t.test}}.
Normality check is performed using
\code{stats::\link[stats:shapiro.test]{shapiro.test}}.
Equality of variances across groups is assessed using \code{car::\link[car:leveneTest]{leveneTest}}.
Cohen's d is calculated using \code{effectsize::\link[effectsize:cohens_d]{cohens_d}}.
}
\examples{
library(volker)
data <- volker::chatgpt

effect_metrics_one_grouped(data, sd_age, sd_gender)

}
\keyword{internal}
