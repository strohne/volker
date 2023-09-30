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


#' Get the common prefix of character values
#'
#' Helper function taken from the biobase package.
#' Duplicated here instead of loading the package to avoid overhead.
#' See https://github.com/Bioconductor/Biobase
#'
#' @param x Charaxcter vector
#' @param ignore.case Whether case matters
#' @return The longest common prefix of the strings
#' @rdname plot_compare_items
#' @export
get_prefix <- function (x, ignore.case = FALSE)
{
  x <- as.character(x)
  if (ignore.case)
    x <- toupper(x)
  nc <- nchar(x, type = "char")
  for (i in 1:min(nc)) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) {
      return(substr(x[1], 1, i - 1))
    }
  }
  substr(x[1], 1, i)
}
