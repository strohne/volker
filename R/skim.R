#' A reduced skimmer for metric variables
#' Returns a five point summary, mean and sd, items count and alpha for scales added by add_index()
#'
#' @keywords internal
#'
#' @importFrom skimr skim_with
#' @importFrom skimr sfl
#' @return A skimmer, see \link[skimr:skim_with]{skim_with}
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' skim_metrics(data)
#'
#' @export
skim_metrics <- skimr::skim_with(
  numeric = skimr::sfl(
    min = ~ base::ifelse(any(!is.na(.)), base::min(., na.rm = TRUE), NA),
    q1 = ~ stats::quantile(., probs = 0.25, na.rm = TRUE, names = FALSE),
    median = ~ stats::median(., na.rm = TRUE),
    q3 = ~ stats::quantile(., probs = 0.75, na.rm = TRUE, names = FALSE),
    max = ~ base::ifelse(any(!is.na(.)), base::max(., na.rm = TRUE), NA),
    mean = ~ base::mean(., na.rm = TRUE),
    sd = ~ stats::sd(., na.rm = TRUE),
    ci.low = ~ base::ifelse(length(.) > 3,  stats::t.test(stats::na.omit(.))$conf.int[1], NA),
    ci.high = ~ base::ifelse(length(.) > 3, stats::t.test(stats::na.omit(.))$conf.int[2], NA),
    items = ~ get_alpha(.)$items,
    alpha = ~ get_alpha(.)$alpha
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
#' @param x A numeric vector.
#' @return The lower whisker value.
.whisker_lower <- function(x, k=1.5) {
  i <- stats::quantile(x, 0.25, na.rm= TRUE) - k * stats::IQR(x, na.rm= TRUE)
  min(x[x >= i], na.rm= TRUE)
}

#' Calculate upper whisker in a boxplot
#'
#' @keywords internal
#'
#' @param x A numeric vector.
#' @return The upper whisker value.
.whisker_upper <- function(x, k=1.5) {
  i <- stats::quantile(x, 0.75, na.rm= TRUE) + k * stats::IQR(x, na.rm= TRUE)
  max(x[x <= i], na.rm= TRUE)
}

#' Calculate outliers
#'
#' @keywords internal
#'
#' @param x A numeric vector.
#' @return A list of outliers.
.outliers <- function(x, k=1.5) {
  list(stats::na.omit(x[(x < .whisker_lower(x)) | (x > .whisker_upper(x))]))
}

#' A skimmer for boxplot generation
#'
#' Returns a five point summary, mean and sd, items count and alpha for scales added by add_index().
#' Additionally, the whiskers defined by the minimum respective maximum value within 1.5 * iqr are calculated.
#' Outliers are returned in a list column.
#'
#' @keywords internal
skim_boxplot <- skimr::skim_with(
  numeric = skimr::sfl(
    min = ~ base::ifelse(any(!is.na(.)), base::min(., na.rm = TRUE), NA),
    q1 = ~ stats::quantile(., probs = 0.25, na.rm = TRUE, names = FALSE),
    median = ~ stats::median(., na.rm = TRUE),
    q3 = ~ stats::quantile(., probs = 0.75, na.rm = TRUE, names = FALSE),
    max = ~ base::ifelse(any(!is.na(.)), base::max(., na.rm = TRUE), NA),
    mean = ~ base::mean(., na.rm = TRUE),
    sd = ~ stats::sd(., na.rm = TRUE),
    iqr = ~ IQR(., na.rm = TRUE),
    whisker.lo = ~ .whisker_lower(.),
    whisker.hi = ~ .whisker_upper(.),
    outliers = ~.outliers(.),
    items = ~ get_alpha(.)$items,
    alpha = ~ get_alpha(.)$alpha
  ),
  base = skimr::sfl(
    n = length,
    missing = skimr::n_missing
  ),
  append = FALSE
)



