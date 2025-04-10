% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{labs_restore}
\alias{labs_restore}
\title{Restore labels from the codebook store in the codebook attribute.}
\usage{
labs_restore(data, cols = NULL)
}
\arguments{
\item{data}{A data frame.}

\item{cols}{A tidyselect column selection.}
}
\value{
A data frame.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\details{
You can store labels before mutate operations by calling
\link{labs_store}.
}
\examples{
library(dplyr)
library(volker)

volker::chatgpt |>
  labs_store() |>
  mutate(sd_age = 2024 - sd_age) |>
  labs_restore() |>
  tab_metrics(sd_age)
}
