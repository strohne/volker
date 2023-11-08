#' Get the p value of an lm model
#'
#' @param fit The result of an lm() call
#' @return The p value of the model
lm_pvalue <- function(fit) {
  fstat <- summary(fit)$fstatistic
  pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
}

#' Get significance stars from p values
#'
#' @param x A vector of p values
#' @return A character vector with stars
#' @export
get_stars <- function(x) {
  sapply(x, function(p) {
    if (p < 0.001)
      return ("***")
    else if (p < 0.01)
      return ("**")
    else if (p < 0.05)
      return ("*")
    else if (p < 0.1)
      return (".")
    else return ("")
  })
}

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
#' @return A tibble with the columns item_name, item_label, value_name, and value_label
#' @export
get_labels <- function(data, cols) {

  if (!missing(cols)) {
    data <- dplyr::select(data,{{cols}})
  }

  labels <- tibble(
    item_name = colnames(data),
    item_label = sapply(data,attr,"comment"),
    value_label = lapply(data,attributes)
  ) %>%

    dplyr::mutate(item_label=as.character(sapply(item_label,function(x) ifelse(is.null(x),NA,x)))) %>%
    tidyr::unnest_longer(value_label)

  if ("value_label_id" %in% colnames(labels)) {
    labels_codes <- labels %>%
      dplyr::rename(value_name = value_label_id) %>%
      dplyr::filter(value_name != "comment", value_name != "class", value_name != "levels") %>%
      dplyr::mutate(value_label = as.character(value_label)) %>%
      dplyr::select(item_name, item_label, value_name, value_label)

    labels <- labels %>%
      dplyr::rename(value_name = value_label_id) %>%
      dplyr::select(item_name, item_label) %>%
      anti_join(labels_codes, by="item_name") %>%
      bind_rows(labels_codes)
  }

  labels
}


#' Set variable labels by setting their comment attributes
#'
#' @param data A tibble
#' @param labels A tibble with variable names in the first column (item_name) and their labels in the second column (item_label)
#' @return A tibble with new variable labels
#' @export
set_item_labels <- function(data, labels) {

  for (no in c(1:nrow(labels))) {
    item_name <- labels[[1]][no]
    item_label <- labels[[2]][no]
    comment(data[[item_name]]) <- item_label
  }

  data
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

  newline <- newline && (knitr::is_html_output() || knitr::is_latex_output())
  sep <- ifelse(newline,"\n"," ")

  for (i in 2:ncol(x)) {
    x[[i]] <- paste0(x[[i]], sep, "(", y[[i]], ")")
  }
  x
}


#' Knit table with defaults for the package
#'
#' @param df Data frame
#' @return Formatted table
knit_table <- function(df, ...){

  if (knitr::is_html_output()) {

    # Replace \n by <br>
    df <- df %>%
      dplyr::mutate(across(dplyr::where(is.character), ~ gsub("\n", "<br>", .))) %>%
      knitr::kable("html", escape = F, align=c("l", rep("r",ncol(df) - 1)), ...) %>%
      kableExtra::kable_styling()


  } else if (knitr::is_latex_output()) {

    df <- df %>%
      dplyr::mutate_all(kableExtra::linebreak) %>%
      knitr::kable("latex", booktabs = T, escape = F, align=c("l", rep("r",ncol(df) - 1)), ...)

  } else {

    df <- df %>%
      knitr::kable("pipe" , align=c("l", rep("r",ncol(df) - 1)), ...)

  }

  df
}
