#' A reduced skimmer for metric variables
#' Returns a five point summary, mean and sd, items count and alpha for scales added by idx_add()
#'
#' @keywords internal
#'
#' @importFrom skimr skim_with
#' @importFrom skimr sfl
#' @return A skimmer, see \link{skim_with}
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' skim_metrics(data)
#'
#' @export
skim_metrics <- skimr::skim_with(
  numeric = skimr::sfl(
    min = ~ base::ifelse(any(!is.na(.)), base::min(., na.rm = T), NA),
    q1 = ~ stats::quantile(., probs = 0.25, na.rm = TRUE, names = FALSE),
    median = ~ stats::median(., na.rm = T),
    q3 = ~ stats::quantile(., probs = 0.75, na.rm = TRUE, names = FALSE),
    max = ~ base::ifelse(any(!is.na(.)), base::max(., na.rm = T), NA),
    mean = ~ base::mean(., na.rm = T),
    sd = ~ stats::sd(., na.rm = T),
    items = ~ idx_alpha(.)$items,
    alpha = ~ idx_alpha(.)$alpha
  ),
  base = skimr::sfl(
    n = length,
    missing = skimr::n_missing
  ),
  append = FALSE
)

#' Calculate IQR
#'
#' @keywords internal
#'
#' @param x A numeric vector
#' @return The IQR
.iqr <- function(x) {
  q <- stats::quantile(x, probs = c(0.25,0.75), na.rm = TRUE, names = FALSE)
  diff(q)
}

#' Calculate lower whisker in a boxplot
#'
#' @keywords internal
#'
#' @param x A numeric vector
#' @return The lower whisker value
.whisker_lower <- function(x, k=1.5) {
  i <- stats::quantile(x, 0.25, na.rm=T) - k * stats::IQR(x, na.rm=T)
  min(x[x >= i], na.rm=T)
}

#' Calculate upper whisker in a boxplot
#'
#' @keywords internal
#'
#' @param x A numeric vector
#' @return The upper whisker value
.whisker_upper <- function(x, k=1.5) {
  i <- stats::quantile(x, 0.75, na.rm=T) + k * stats::IQR(x, na.rm=T)
  max(x[x <= i], na.rm=T)
}

#' Calculate outliers
#'
#' @keywords internal
#'
#' @param x A numeric vector
#' @return A list of outliers
.outliers <- function(x, k=1.5) {
  list(stats::na.omit(x[(x < .whisker_lower(x)) | (x > .whisker_upper(x))]))
}

#' A skimmer for boxplot generation
#'
#' Returns a five point summary, mean and sd, items count and alpha for scales added by idx_add().
#' Additionally, the whiskers defined by the minimum respective maximum value within 1.5 * iqr are calculated.
#' Outliers are returned in a list column.
#'
#' @keywords internal
skim_boxplot <- skimr::skim_with(
  numeric = skimr::sfl(
    min = ~ base::ifelse(any(!is.na(.)), base::min(., na.rm = T), NA),
    q1 = ~ stats::quantile(., probs = 0.25, na.rm = TRUE, names = FALSE),
    median = ~ stats::median(., na.rm = T),
    q3 = ~ stats::quantile(., probs = 0.75, na.rm = TRUE, names = FALSE),
    max = ~ base::ifelse(any(!is.na(.)), base::max(., na.rm = T), NA),
    mean = ~ base::mean(., na.rm = T),
    sd = ~ stats::sd(., na.rm = T),
    iqr = ~ IQR(., na.rm = T),
    whisker.lo = ~ .whisker_lower(.),
    whisker.hi = ~ .whisker_upper(.),
    outliers = ~.outliers(.),
    items = ~ idx_alpha(.)$items,
    alpha = ~ idx_alpha(.)$alpha
  ),
  base = skimr::sfl(
    n = length,
    missing = skimr::n_missing
  ),
  append = FALSE
)



