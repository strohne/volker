% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/effects.R
\name{effect_metrics}
\alias{effect_metrics}
\title{Output effect sizes and regression model parameters}
\usage{
effect_metrics(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...)
}
\arguments{
\item{data}{A data frame.}

\item{cols}{A tidy column selection,
e.g. a single column (without quotes)
or multiple columns selected by methods such as starts_with().}

\item{cross}{Optional, a grouping column (without quotes).}

\item{metric}{When crossing variables, the cross column parameter can contain categorical or metric values.
By default, the cross column selection is treated as categorical data.
Set metric to TRUE, to treat it as metric and calculate correlations.}

\item{clean}{Prepare data by \link{data_clean}.}

\item{...}{Other parameters passed to the appropriate effect function.}
}
\value{
A volker tibble.
}
\description{
The regression type depends on the number of selected columns:
\itemize{
\item One column: see \link{effect_metrics_one} (not yet implemented)
\item Multiple columns: see \link{effect_metrics_items}
\item One column and one grouping column: see \link{effect_metrics_one_grouped}
\item Multiple columns and one grouping column: see \link{effect_metrics_items_grouped} (not yet implemented)
}

By default, if you provide two column selections, the second column is treated as categorical.
Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
\itemize{
\item Two metric columns: see \link{effect_metrics_one_cor}
\item Multiple columns: see \link{effect_metrics_items_cor}
}

\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#experimental}{\figure{lifecycle-experimental.svg}{options: alt='[Experimental]'}}}{\strong{[Experimental]}}
}
\examples{
library(volker)
data <- volker::chatgpt

effect_metrics(data, sd_age, sd_gender)

}
