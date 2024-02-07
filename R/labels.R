#' Get variable labels from their comment attributes
#'
#' @param data A tibble
#' @param cols A tidy variable selections to filter specific columns
#' @return A tibble with the columns:
#'        - item_name: The column name.
#'        - item_group: First part of the column name, up to an underscore.
#'        - item_class: The last class value of an item (e.g. numeric, factor).
#'        - item_label: The comment attribute of the column.
#'        - value_name: In case a column has numeric attributes, the attribute names
#'        - value_label: In case a column has numeric attributes or T/F-attributes,
#'                       the attribute values.
#'                       In case a column has a levels attribute, the levels.
#' @export
codebook <- function(data, cols) {
  if (!missing(cols)) {
    data <- dplyr::select(data, {{ cols }})
  }

  # Replace empty classes with NA
  item_classes <- sapply(data, attr, "class", simplify = F)
  item_classes <- ifelse(sapply(item_classes, is.null), NA, item_classes)

  item_comments <- sapply(data, attr, "comment", simplify = F)
  item_comments <- ifelse(sapply(item_comments, is.null), NA, item_comments)

  labels <- dplyr::tibble(
    item_name = colnames(data),
    item_class = item_classes,
    item_label = item_comments,
    value_label = lapply(data, attributes)
  ) %>%
    dplyr::mutate(item_label = as.character(sapply(item_label, function(x) ifelse(is.null(x), NA, x)))) %>%
    dplyr::mutate(item_label = ifelse(is.na(item_label), item_name, item_label)) %>%
    dplyr::mutate(item_group = stringr::str_remove(item_name, "_.*")) %>%
    dplyr::mutate(item_class = as.character(sapply(item_class, function(x) ifelse(length(x) > 1, x[[length(x)]], x)))) %>%
    dplyr::select(item_name, item_group, item_class, item_label, value_label) %>%
    tidyr::unnest_longer(value_label, keep_empty = T)


  if ("value_label_id" %in% colnames(labels)) {
    # Get items with codes
    labels_codes <- labels %>%
      dplyr::rename(value_name = value_label_id) %>%
      # dplyr::filter(!(value_name %in% c("comment", "class","levels","tzone"))) %>%
      dplyr::filter(stringr::str_detect(value_name, "^-?[0-9TF]+$")) %>%
      dplyr::mutate(value_label = as.character(value_label)) %>%
      dplyr::select(item_name, item_group, item_class, item_label, value_name, value_label)

    labels_levels <- labels %>%
      # dplyr::rename(value_name = value_label_id) %>%
      dplyr::filter(value_label_id == "levels") %>%
      dplyr::select(item_group, item_class, item_name, item_label, value_label) %>%
      tidyr::unnest_longer(value_label) %>%
      dplyr::mutate(value_label = as.character(value_label))


    # Combine items without codes and items with codes
    labels <- labels %>%
      # dplyr::rename(value_name = value_label_id) %>%
      dplyr::distinct(item_name, item_group, item_class, item_label) %>%
      dplyr::anti_join(labels_codes, by = "item_name") %>%
      dplyr::bind_rows(labels_codes) %>%
      dplyr::anti_join(labels_levels, by = "item_name") %>%
      dplyr::bind_rows(labels_levels)
  } else {
    labels$value_name <- NA
  }

  # Detect groups
  # TODO: revise group detection. Will be used for index calculation.
  # groups <- labels %>%
  #   dplyr::distinct(item_name) %>%
  #   dplyr::mutate(
  #     item_prefix = get_prefix(item_name),
  #     item_postfix = stringr::str_sub(item_name, stringr::str_length(item_prefix) + 1)
  #   ) %>%
  #   dplyr::arrange(item_postfix) %>%
  #   dplyr::mutate(
  #     item_next = dplyr::lead(item_postfix),
  #     item_prev = dplyr::lag(item_postfix)
  #   ) %>%
  #   dplyr::rowwise() %>%
  #   dplyr::mutate(
  #     item_infix = ifelse(!is.na(item_next),get_prefix(c(item_postfix, item_next)), ""),
  #     item_infix = ifelse(item_infix == "",get_prefix(c(item_postfix, item_prev)), item_infix),
  #   ) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(item_postfix = stringr::str_sub(item_postfix, stringr::str_length(item_infix) + 1)) %>%
  #   dplyr::select(item_name, item_prefix, item_infix, item_postfix)
  #
  # labels <- dplyr::left_join(labels, groups, by="item_name")
  labels
}

#' Get the current codebook and store it in the codebook attribute.
#'
#' You can restore the labels after mutate operations by calling
#' \link{labs_restore}.
#'
#' @param data A data frame
#' @return A data frame
#' @export
labs_store <- function(data) {
  codes <- codebook(data)
  attr(data,"codebook") <- codes
  data
}

#' Restore labels from the codebook store in the codebook attribute.
#'
#' You can store labels before mutate operations by calling
#' \link{labs_store}.
#'
#' @param data A data frame
#' @param values If TRUE (default), restores value labels in addition to item labels.
#'              Item labels correspond to columns, value labels to values in the columns.
#' @return A data frame
#' @export
labs_restore <- function(data, values=T) {

  codes <- attr(data,"codebook")

  if (is.data.frame(codes)) {
    data <- labs_apply(data, codes, values)
  } else  {
    warning("No codebook found in the attributes.")
  }
  data
}

#' Set variable labels by setting their comment attributes
#'
#' @param data A tibble
#' @param codes A tibble in \link{codebook} format.
#'              To set column labels, use item_name and item_label columns.
#' @param values If TRUE (default), sets value labels.
#'               Value labels are retrieved from the columns
#'               value_name and value_label in your codebook.
#' @return A tibble with new labels
#' @export
labs_apply <- function(data, codes, values=T) {

  # Fix column names
  if (!"item_name" %in% colnames(codes)) {
    colnames(codes)[1] <- "item_name"
  }
  if (!"item_label" %in% colnames(codes)) {
    colnames(codes)[2] <- "item_label"
  }

  # Can only set value labels if present in the codebook
  values <- values &&
    ("value_name" %in% colnames(codes)) &&
    ("value_label" %in% colnames(codes))

  # Set comment attributes
  for (no in c(1:nrow(codes))) {
    item_name <- codes$item_name[no]
    item_label <- codes$item_label[no]

    if (item_name %in% colnames(data)) {
      comment(data[[item_name]]) <- item_label

      if (values) {
        value_name <- codes$value_name[no]
        value_label <- codes$value_label[no]

        if (!is.null(value_name) && !is.na(value_name)) {
          attr(data[[item_name]], value_name) <- value_label
        }
      }

    }

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
labs_clear <- function(data, cols, labels = NULL) {

  remove_attr <-  function(x) {
    if (is.null(labels)) {
      attributes(x) <- NULL
    } else {
      attr(x, labels) <- NULL
    }
    x
  }

  if (missing(cols)) {
    data <-  dplyr::mutate(data, dplyr::across(everything(), ~remove_attr(.)))
  } else {
    data <-  dplyr::mutate(data, dplyr::across({{ cols }}, ~remove_attr(.)))
  }
  data
}


#' Replace item names in a column by their labels
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding item names
#' @param codes The codebook to use: A tibble with the columns item_name and item_label.
#'              Can be created by the \link{codebook} function, e.g. by calling
#'              `codes <- codebook(data, myitemcolumn)`.
#' @return Tibble with new labels
labs_replace_names <- function(data, col, codes) {

  codes <- codes %>%
    dplyr::distinct(item_name, item_label) %>%
    dplyr::rename(.name = item_name, .label = item_label) %>%
    na.omit()


  if (nrow(codes) > 0) {
    data <- data %>%
      dplyr::mutate(.name = {{ col }}) %>%
      dplyr::left_join(codes, by = ".name") %>%
      dplyr::mutate({{ col }} := dplyr::coalesce(.label, .name)) %>%
      dplyr::select(-.name, -.label)
  }

  data
}

#' Replace item value names in a column by their labels
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding item values
#' @param codes The codebook to use: A tibble with the columns value_name and value_label.
#'              Can be created by the \link{codebook} function, e.g. by calling
#'              `codes <- codebook(data, myitemcolumn)`.
#' @return Tibble with new labels
labs_replace_values <- function(data, col, codes) {

  codes <- codes %>%
    dplyr::distinct(value_name, value_label) %>%
    dplyr::rename(.name = value_name, .label = value_label) %>%
    na.omit()


  if (nrow(codes) > 0) {
    data <- data %>%
      dplyr::mutate(.name = {{ col }}) %>%
      dplyr::left_join(codes, by = ".name") %>%
      dplyr::mutate({{ col }} := dplyr::coalesce(.label, .name)) %>%
      dplyr::select(-.name, -.label)
  }

  data
}

#' Get a common title for a column selection
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols A tidy column selection
#' @return A character string
get_title <- function(data, cols) {
  labels <- data %>%
    codebook({{ cols }}) %>%
    tidyr::drop_na(item_label)

  if (nrow(labels) > 0) {
    labels <- labels$item_label
  } else {
    labels <- select(data, {{ cols }}) %>% colnames()
  }

  labels %>%
    get_prefix() %>%
    trim_label()
}

#' Get the numeric range from the labels
#'
#' @keywords internal
#'
#' @param data The labeled data frame
#' @param cols A tidy variable selection
#' @param negative Whether to include negative values
#' @return A list or NULL
get_limits <- function(data, cols, negative = F) {

  # First, try to get limits from the column attributes
  values <- data %>%
    dplyr::select({{ cols }}) %>%
    lapply(attr, "limits") %>%
    unlist()

  # Second, try to get limits from the column labels
  if (is.null(values)) {
    values <- codebook(data, {{ cols }}) %>%
      dplyr::distinct(value_name) %>%
      pull(value_name)
    values <- suppressWarnings(as.numeric(values))
  }

  # Third, try to get limits from the column values
  if (is.null(values) | all(is.na(values))) {
    values <- data %>%
      dplyr::select({{ cols }}) %>%
      unlist()
    values <- suppressWarnings(as.numeric(values))
  }

  if (all(is.na(values))) {
    return(NULL)
  }

  if (!negative) {
    values <- suppressWarnings(values[values >= 0])
  }

  if (any(!is.na(values))) {
    return(range(values, na.rm = T))
  }

  return(NULL)
}

#' Detect whether a scale is a numeric sequence
#'
#' From all values in the selected columns, the numbers are extracted.
#' If no numeric values can be found, returns 0.
#' Otherwise, if any positive values form an ascending sequence, returns -1.
#' In all other cases, returns 1.
#'
#' @keywords internal
#'
#' @param data The dataframe
#' @param cols The tidy selection
#' @param extract Whether to extract numeric values from characters
#' @return 0 = an undirected scale, -1 = descending values, 1 = ascending values
get_direction <- function(data, cols, extract = T) {
  data <- dplyr::select(data, {{ cols }})

  # Get all values
  categories <- data %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) %>%
    tidyr::pivot_longer(tidyselect::everything()) %>%
    dplyr::arrange(value) %>%
    dplyr::mutate(value = ifelse(extract, stringr::str_extract(value, "[0-9-]+"), value)) %>%
    dplyr::distinct(value) %>%
    dplyr::pull(value)

  # Detect whether the categories are a numeric sequence and choose direction
  scale_numeric <- data %>%
    sapply(is.numeric) %>%
    all()

  scale_ordered <- suppressWarnings(as.numeric(c(categories)))
  scale_positive <- scale_ordered[scale_ordered >= 0]

  if (!scale_numeric && all(is.na(scale_ordered))) {
    categories_scale <- 0
  } else if (any(diff(scale_positive) >= 0) | any(is.na(scale_ordered))) {
    categories_scale <- -1
  } else {
    categories_scale <- 1
  }

  categories_scale
}


#' Get the common prefix of character values
#'
#' Helper function taken from the biobase package.
#' Duplicated here instead of loading the package to avoid overhead.
#' See https://github.com/Bioconductor/Biobase
#'
#' @keywords internal
#'
#' @param x Character vector
#' @param ignore.case Whether case matters (default)
#' @param trim Whether non alphabetic characters should be trimmed
#' @return The longest common prefix of the strings
get_prefix <- function(x, ignore.case = FALSE, trim = FALSE) {
  x <- as.character(x)
  if (ignore.case) {
    x <- toupper(x)
  }
  x <- na.omit(x)

  if (length(x) == 0) {
    return (NA)
  }

  if (length(x) == 1) {
    return (x)
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
#' @keywords internal
#'
#' @param x A character value
#' @return The trimmed character value
trim_label <- function(x) {
  x <- stringr::str_remove(x, "[: 0_-]*$")
  x <- stringr::str_remove(x, "^[: _-]*")
  x
}



#' Prepare the scale attribute values
#'
#' @keywords internal
#'
#' @keywords internal
#' @param data A tibble with a scale attribute
#' @return A named list or NULL
prepare_scale <- function(scale) {
  if (!is.null(scale)) {
    scale <- scale %>%
      dplyr::mutate(value_name = suppressWarnings(as.numeric(value_name))) %>%
      dplyr::filter(value_name >= 0) %>%
      na.omit()

    scale <- setNames(
      as.character(scale$value_label),
      as.character(scale$value_name)
    )
  }
  scale
}

#' Wrap labels in plot scales
#'
#' @keywords internal
label_scale <- function(x, scale) {
  ifelse(
    x %in% names(scale),
    stringr::str_wrap(scale[as.character(x)], width = 10),
    x
  )
}

