#' A reduced skimmer for metric variables
#' Returns a five point summary, mean and sd, items count and alpha for scales added by add_idx()
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
    items = ~ get_idx_alpha(.)$items,
    alpha = ~ get_idx_alpha(.)$alpha
  ),
  base = skimr::sfl(
    n = length,
    missing = skimr::n_missing
  ),
  append = FALSE
)

.iqr <- function(x) {
  q <- stats::quantile(x, probs = c(0.25,0.75), na.rm = TRUE, names = FALSE)
  diff(q)
}

.whisker_lower <- function(x, k=1.5) {
  i <- quantile(x, 0.25, na.rm=T) - k * IQR(x, na.rm=T)
  min(x[x >= i], na.rm=T)
}

.whisker_upper <- function(x, k=1.5) {
  i <- quantile(x, 0.75, na.rm=T) + k * IQR(x, na.rm=T)
  max(x[x <= i], na.rm=T)
}

.outliers <- function(x, k=1.5) {
  list(na.omit(x[(x < .whisker_lower(x)) | (x > .whisker_upper(x))]))
}

# Define a custom skimmer function for lower whisker
skim_custom_lower_whisker <- skimr::skim_with(numeric = skimr::sfl(iqr=~IQR(.,na.rm=T)))

#' A skimmer for boxplot generation
#'
#' Returns a five point summary, mean and sd, items count and alpha for scales added by add_idx().
#' Additionally, the whiskers defined by the minimum respective maximum value within 1.5 * iqr are calculated.
#' Outliers are returned in a list column.
#'
#' @export
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
    items = ~ get_idx_alpha(.)$items,
    alpha = ~ get_idx_alpha(.)$alpha
  ),
  base = skimr::sfl(
    n = length,
    missing = skimr::n_missing
  ),
  append = FALSE
)



