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
#' Returns a five point summary, mean and sd, items count and alpha for scales added by add_idx()
#' @export
skim_metrics <- skimr::skim_with(
  numeric = skimr::sfl(
    min = ~ base::min(., na.rm = T),
    q1 = ~ stats::quantile(., probs = 0.25, na.rm = TRUE, names = FALSE),
    median = ~ stats::median(., na.rm = T),
    q3 = ~stats::quantile(., probs = 0.75, na.rm = TRUE, names = FALSE),
    max = ~ base::max(., na.rm = T),
    mean = ~ base::mean(., na.rm=T),
    sd = ~ stats::sd(., na.rm=T),
    items = ~ get_idx_alpha(.)$items,
    alpha = ~ get_idx_alpha(.)$alpha
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
#' @return A tibble with the columns:
#'        - item_group: First part of the column name, up to an underscore
#'        - item_class: The last class value of an item (e.g. numeric, factor)
#'        - item_name: The column name,
#'        - item_label: The comment attribute of the column
#'        - value_name: In case a column has multiple attributes, the attribute names
#'        - value_label: In case a column has multiple attributes, the attribute values
#' @export
get_labels <- function(data, cols) {

  if (!missing(cols)) {
    data <- dplyr::select(data,{{cols}})
  }

  # Replace empty classes with NA
  item_classes <- sapply(data,attr,"class", simplify = F)
  item_classes <- ifelse(sapply(item_classes, is.null), NA, item_classes)

  item_comments = sapply(data,attr,"comment", simplify = F)
  item_comments <- ifelse(sapply(item_comments, is.null),NA, item_comments)

  labels <- tibble(
    item_name = colnames(data),
    item_class = item_classes,
    item_label = item_comments,
    value_label = lapply(data,attributes)
  ) %>%

    dplyr::mutate(item_label=as.character(sapply(item_label,function(x) ifelse(is.null(x),NA,x)))) %>%
    dplyr::mutate(item_group=stringr::str_remove(item_name,"_.*")) %>%
    dplyr::mutate(item_class=as.character(sapply(item_class, function(x) ifelse(length(x) > 1, x[[length(x)]],x)))) %>%
    select(item_group, item_class, item_name, item_label, value_label) %>%
    tidyr::unnest_longer(value_label)


  if ("value_label_id" %in% colnames(labels)) {

    #Get items with codes
    labels_codes <- labels %>%
      dplyr::rename(value_name = value_label_id) %>%
      dplyr::filter(!(value_name %in% c("comment", "class","levels","tzone"))) %>%
      dplyr::mutate(value_label = as.character(value_label)) %>%
      dplyr::select(item_group, item_class, item_name, item_label, value_name, value_label)

    labels_levels <- labels %>%
      #dplyr::rename(value_name = value_label_id) %>%
      dplyr::filter(value_label_id == "levels") %>%
      dplyr::select(item_group, item_class, item_name, item_label, value_label) %>%
      tidyr::unnest_longer(value_label) %>%
      dplyr::mutate(value_label = as.character(value_label))


    # Combine items without codes and items with codes
    labels <- labels %>%
      #dplyr::rename(value_name = value_label_id) %>%
      dplyr::distinct(item_group, item_class, item_name, item_label) %>%

      anti_join(labels_codes, by="item_name") %>%
      bind_rows(labels_codes) %>%

      anti_join(labels_levels, by="item_name") %>%
      bind_rows(labels_levels)
  } else {
    labels$value_name <- NA
  }

  labels
}

#' Get the numeric range from the labels
#'
#' @param data The labeled data frame
#' @param cols A tidy variable selection
#' @param negative Whether to include negative values
#' @export
get_limits <- function(data, cols, negative=F) {
  values <- get_labels(data, {{cols}}) %>%
    distinct(value_name) %>%
    pull(value_name)

  values <- suppressWarnings(as.numeric(c(values)))

  if (all(is.na(values))) {
    return(NULL)
  }

  if (!negative) {
    values <- suppressWarnings(values[values >= 0])
  }

  if (any(!is.na(values))) {
    return (range(values, na.rm=T))
  }

  return (NULL)
}

#' Remove all comments from the selected columns
#'
#' @param data A tibble
#' @param cols Tidyselect columns
#' @param labels The attributes to remove. NULL to remove all attributes.
#' @return A tibble with comments removed
#' @export
remove_labels <- function(data, cols, labels = NULL) {
  # if (missing(cols)) {
  #   cols <- c(1:ncol(data))
  # } else {
  #   cols <- {{cols}}
  # }

  data %>%
    dplyr::mutate(across({{cols}}, function(x) {
      if (is.null(labels)) {
        attributes(x) <- NULL
      } else {
        attr(x,labels) <- NULL
      }
    x
  }))
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
