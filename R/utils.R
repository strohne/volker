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

#' Combine two identically shaped data frames
#' by adding values of each column from the second data frame
#' into the corresponding column in the first dataframe using parentheses
#'
#' TODO: implement newline parameter
#'
#' @param x The first data frame
#' @param y The second data frame
#' @param newline Whether to add a new line character between the values (default: TRUE).
#' @return A combined data frame
#' @export
zip_tables <- function(x, y, newline=TRUE) {

  for (i in 2:ncol(x)) {
    x[[i]] <- paste0(x[[i]], " (", y[[i]], ")")
  }
  x
}


knit_table <- function(df){
  if (knitr::is_html_output()) {
    df %>%
      knitr::kable("html", escape = F) %>%
      kableExtra::kable_styling()
  } else {
    df <- data.frame(lapply(df, function(x) {gsub("<br>", "\n", x)}), stringsAsFactors = F)

    df %>%
      dplyr::mutate_all(kableExtra::linebreak) %>%
      knitr::kable("latex", booktabs = T, escape = F)
  }
}
