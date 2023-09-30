#' A reduced skimmer for metric variables
#' @export
skim_metrics <- skimr::skim_with(
  numeric = skimr::sfl(
    min = ~ base::min(., na.rm = T),
    q1 = ~ stats::quantile(., probs = 0.25, na.rm = TRUE, names = FALSE),
    median = ~ stats::median(., na.rm = T),
    q3 = ~stats::quantile(., probs = 0.75, na.rm = TRUE, names = FALSE),
    max = ~ base::max(., na.rm = T),
    mean = ~ base::mean(., na.rm=T),
    sd = ~ stats::sd(., na.rm=T)
  ),
  base = skimr::sfl(
    n = length,
    missing = skimr::n_missing
  ),
  append = FALSE
)

#' Get variable labels from their comment attributes
#'
#' @param data A tibble
#' @export
get_labels <- function(data) {
  labels <- tibble(
    item = colnames(data),
    label = sapply(data,attr,"comment"),
    value = lapply(data,attributes)
  ) %>%
    mutate(label=as.character(label)) %>%
    unnest_longer(value)

  if ("value_id" %in% colnames(labels)) {
  labels <- labels %>%
    filter(value_id != "comment", value_id != "class" ) %>%
    mutate(value = as.character(value))
  }

  labels
}
