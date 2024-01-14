#' Get variable labels from their comment attributes
#'
#' @param data A tibble
#' @param cols A tidy variable selections to filter specific columns
#' @return A tibble with the columns:
#'        - item_group: First part of the column name, up to an underscore.
#'        - item_class: The last class value of an item (e.g. numeric, factor).
#'        - item_name: The column name.
#'        - item_label: The comment attribute of the column.
#'        - value_name: In case a column has numeric attributes, the attribute names
#'        - value_label: In case a column has numeric attributes or T/F-attributes,
#'                       the attribute values.
#'                       In case a column has a levels attribute, the levels
#' @export
get_labels <- function(data, cols) {

  if (!missing(cols)) {
    data <- dplyr::select(data, {{cols}})
  }

  # Replace empty classes with NA
  item_classes <- sapply(data,attr,"class", simplify = F)
  item_classes <- ifelse(sapply(item_classes, is.null), NA, item_classes)

  item_comments = sapply(data,attr,"comment", simplify = F)
  item_comments <- ifelse(sapply(item_comments, is.null),NA, item_comments)

  labels <- dplyr::tibble(
    item_name = colnames(data),
    item_class = item_classes,
    item_label = item_comments,
    value_label = lapply(data,attributes)
  ) %>%

    dplyr::mutate(item_label=as.character(sapply(item_label,function(x) ifelse(is.null(x),NA,x)))) %>%
    dplyr::mutate(item_group=stringr::str_remove(item_name,"_.*")) %>%
    dplyr::mutate(item_class=as.character(sapply(item_class, function(x) ifelse(length(x) > 1, x[[length(x)]],x)))) %>%
    dplyr::select(item_group, item_class, item_name, item_label, value_label) %>%
    tidyr::unnest_longer(value_label)


  if ("value_label_id" %in% colnames(labels)) {

    #Get items with codes
    labels_codes <- labels %>%
      dplyr::rename(value_name = value_label_id) %>%
      #dplyr::filter(!(value_name %in% c("comment", "class","levels","tzone"))) %>%
      dplyr::filter(stringr::str_detect(value_name,"^-?[0-9TF]+$")) %>%
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

      dplyr::anti_join(labels_codes, by="item_name") %>%
      dplyr::bind_rows(labels_codes) %>%

      dplyr::anti_join(labels_levels, by="item_name") %>%
      dplyr::bind_rows(labels_levels)
  } else {
    labels$value_name <- NA
  }

  labels
}

#' Get a common title for a column selection
#'
#' @param data A tibble
#' @param cols A tidy column selection
#' @return A character string
get_title <- function(data, cols) {

  labels <- data %>%
    get_labels({{cols}}) %>%
    tidyr::drop_na(item_label)

  if (nrow(labels) > 0) {
    labels <- labels$item_label
  } else {
    labels <- select(data, {{cols}}) %>% colnames()
  }

  labels %>%
    get_prefix() %>%
    trim_label()

}

#' Get the numeric range from the labels
#'
#' @param data The labeled data frame
#' @param cols A tidy variable selection
#' @param negative Whether to include negative values
#' @export
get_limits <- function(data, cols, negative=F) {
  values <- get_labels(data, {{cols}}) %>%
    dplyr::distinct(value_name) %>%
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

#' Detect whether a scale is a numeric sequence
#'
#' From all values in the selected columns, the numbers are extracted.
#' If no numeric values can be found, returns 0.
#' Otherwise, if any positive values form an ascending sequence, returns -1.
#' In all other cases, returns 1.
#'
#' @param data The dataframe
#' @param cols The tidy selection
#' @param extract Whether to extract numeric values from characters
#' @return 0 = an undirected scale, -1 = descending values, 1 = ascending values
get_scale <- function(data, cols, extract=T) {

  data <- dplyr::select(data, {{cols}})

  # Get all values
  categories <- data %>%
    dplyr::mutate(across(tidyselect::everything(), as.character)) %>%
    tidyr::pivot_longer(tidyselect::everything()) %>%
    dplyr::arrange(value) %>%
    dplyr::mutate(value = ifelse(extract, stringr::str_extract(value,"[0-9-]+"), value)) %>%
    dplyr::distinct(value) %>%
    dplyr::pull(value)

  # Detect whether the categories are a numeric sequence and choose direction
  scale_numeric <- data %>%
    sapply(is.numeric) %>%
    all()

  scale_ordered <- suppressWarnings(as.numeric(c(categories)))
  scale_positive <- scale_ordered[scale_ordered >= 0]

  if (!scale_numeric && all(is.na(scale_ordered))) {
    categories_scale = 0
  }
  else if (any(diff(scale_positive) >= 0) | any(is.na(scale_ordered))) {
    categories_scale = -1
  } else {
    categories_scale = 1
  }

  categories_scale
}

#' Get a column label
#'
#' @param data Labeled data frame
#' @param col The column name
#' @return A character value
get_col_label <- function(data, col) {

  labels <- data %>%
    get_labels({{col}}) %>%
    dplyr::distinct(item_name, item_label)  %>%
    na.omit()


  if ((nrow(labels) > 0)) {
    # TODO: can we return a vector for multiple columns?
    label <- labels$item_label[1]
  }
  else {
    label <- data %>%
      select({{col}}) %>%
      colnames()
  }

  return (label)
}

#' Set column labels by their comment attribute
#'
#' @param data A data frame
#' @param cols Tidyselect column names
#' @param labels The new labels
#' @export
set_col_label <- function(data, cols, labels) {
  cols <- tidyselect::eval_select(expr = enquo(cols), data=data)

  for (i in c(1:length(cols))) {
    attr(data[[cols[i]]], "comment") <- labels[i]
  }

  data
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
#' TODO: merge with set_col_label()
#'
#' @param data A tibble
#' @param labels A tibble with variable names in the first column (item_name)
#'               and their labels in the second column (item_label)
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

#' Replace items in the first column by labels found in the dataset
#'
#' @param result The table with items in the first column,
#'               e.g. produced by tab_item_count()
#' @param data The labeled dataset
#' @param cols The tidyselect columns
replace_item_values <- function(result, data, cols) {
  labels_items <- data %>%
    get_labels(!!cols) %>%
    dplyr::distinct(item_name, item_label) %>%
    na.omit()

  if (nrow(labels_items) > 0) {
    result <- result %>%
      dplyr::left_join(labels_items, by=c("item"="item_name")) %>%
      dplyr::mutate(item = dplyr::coalesce(item_label, item)) %>%
      dplyr::select(-item_label)
  }

  result
}


#' Get the common prefix of character values
#'
#' Helper function taken from the biobase package.
#' Duplicated here instead of loading the package to avoid overhead.
#' See https://github.com/Bioconductor/Biobase
#'
#' @param x Character vector
#' @param ignore.case Whether case matters
#' @param trim Whether non alphabetic characters should be trimmed
#' @return The longest common prefix of the strings
#' @export
get_prefix <- function (x, ignore.case = FALSE, trim=FALSE)
{
  x <- as.character(x)
  if (ignore.case)  {
    x <- toupper(x)
  }

  nc <- nchar(x, type = "char")
  for (i in 1:min(nc)) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) {
      prefix <- substr(x[1], 1, i - 1)
      if (trim) {
        prefix <- trim_label(prefix)
      }
      return(prefix)
    }
  }

  prefix <- trim_label(substr(x[1], 1, i))

  if (trim) {
    prefix <- trim_label(prefix)
  }

  prefix
}

#' Remove trailing zeros and trailing or leading
#' whitespaces, colons,
#' hyphens and underscores
#'
#' @param x A character value
#' @return The trimmed character value
trim_label <- function(x) {
  x <- stringr::str_remove(x, "[: 0_-]*$")
  x <- stringr::str_remove(x, "^[: _-]*")
  x
}
