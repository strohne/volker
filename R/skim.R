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
