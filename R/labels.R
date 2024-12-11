#' Get variable labels from their comment attributes
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A tibble.
#' @param cols A tidy variable selections to filter specific columns.
#' @return A tibble with the columns:
#'        - item_name: The column name.
#'        - item_group: First part of the column name, up to an underscore.
#'        - item_class: The last class value of an item (e.g. numeric, factor).
#'        - item_label: The comment attribute of the column.
#'        - value_name: In case a column has numeric attributes, the attribute names.
#'        - value_label: In case a column has numeric attributes or T/F-attributes,
#'                       the attribute values.
#'                       In case a column has a levels attribute, the levels.
#' @examples
#' volker::codebook(volker::chatgpt)
#' @importFrom rlang .data
#' @export
codebook <- function(data, cols) {
  if (!missing(cols)) {
    data <- dplyr::select(data, {{ cols }})
  }

  # Get column classes
  #item_classes <- sapply(data, attr, "class", simplify = FALSE)
  item_classes <- sapply(data, class, simplify = FALSE)
  item_classes <- ifelse(sapply(item_classes, is.null), NA, item_classes)

  # Get column comments
  item_comments <- sapply(data, attr, "comment", simplify = FALSE)
  item_comments <- ifelse(sapply(item_comments, is.null), NA, item_comments)

  # Construct item label and value label dataframe
  labels <- dplyr::tibble(
    item_name = colnames(data),
    item_class = item_classes,
    item_label = item_comments,
    value_label = lapply(data, attributes)
  ) %>%
    dplyr::mutate(item_label = as.character(sapply(.data$item_label, function(x) ifelse(is.null(x), NA, x)))) %>%
    dplyr::mutate(item_label = ifelse(is.na(.data$item_label), .data$item_name, .data$item_label)) %>%
    dplyr::mutate(item_group = sub("_.*", "", .data$item_name)) |>
    dplyr::mutate(item_class = as.character(sapply(.data$item_class, function(x) ifelse(length(x) > 1, x[[length(x)]], x)))) %>%
    dplyr::select(tidyselect::all_of(c("item_name", "item_group", "item_class", "item_label", "value_label"))) %>%
    tidyr::unnest_longer(tidyselect::all_of("value_label"), keep_empty = TRUE)

  if ("value_label_id" %in% colnames(labels)) {

    # Get items with a labels attribute
    labels_attr <- labels %>%
      # dplyr::rename(value_name = value_label_id) %>%
      dplyr::filter(.data$value_label_id == "labels") %>%
      dplyr::select(tidyselect::all_of(c("item_group", "item_class", "item_name", "item_label", "value_label"))) |>
      tidyr::unnest_longer(tidyselect::all_of("value_label"), indices_to = "value_name", values_to = "value_label") |>
      dplyr::mutate(
        value_name = as.character(.data$value_name),
        value_label = as.character(.data$value_label)
      )

    # Get items with numeric or boolean codes
    labels_codes <- labels %>%
      dplyr::rename(value_name = tidyselect::all_of("value_label_id")) %>%
      # dplyr::filter(!(value_name %in% c("comment", "class","levels","tzone"))) %>%
      dplyr::filter(grepl("^-?[0-9TF]+$", .data$value_name)) |>
      dplyr::mutate(value_label = as.character(.data$value_label)) %>%
      dplyr::select(tidyselect::all_of(c("item_name", "item_group", "item_class", "item_label", "value_name", "value_label")))

    # Get factor levels
    labels_levels <- labels %>%
      # dplyr::rename(value_name = value_label_id) %>%
      dplyr::filter(.data$value_label_id == "levels") %>%
      dplyr::select(tidyselect::all_of(c("item_group", "item_class", "item_name", "item_label", "value_label"))) |>
      tidyr::unnest_longer(tidyselect::all_of("value_label")) %>%
      dplyr::group_by(dplyr::across(tidyselect::all_of(c("item_group", "item_class", "item_name", "item_label")))) |>
      dplyr::mutate(
      #  value_name = as.character(dplyr::row_number()),
        value_name = as.character(.data$value_label),
        value_label = as.character(.data$value_label),
      ) |>
      dplyr::ungroup()


    # Combine items without codes or levels and items with codes or levels
    labels <- labels %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("item_name", "item_group", "item_class", "item_label")))) %>%
      dplyr::anti_join(labels_levels, by = "item_name") %>%
      dplyr::bind_rows(labels_levels) |>
      dplyr::anti_join(labels_codes, by = "item_name") %>%
      dplyr::bind_rows(labels_codes) |>
      dplyr::anti_join(labels_attr, by = "item_name") %>%
      dplyr::bind_rows(labels_attr) |>
      dplyr::select(
        tidyselect::all_of(c("item_name", "item_group", "item_class", "item_label")),
        tidyselect::all_of("value_name"),
        tidyselect::all_of("value_label")
      )

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
#' `r lifecycle::badge("experimental")`
#'
#' You can restore the labels after mutate operations by calling
#' \link{labs_restore}.
#'
#' @param data A data frame.
#' @return A data frame.
#' @examples
#' library(dplyr)
#' library(volker)
#'
#' volker::chatgpt |>
#'   labs_store() |>
#'   mutate(sd_age = 2024 - sd_age) |>
#'   labs_restore() |>
#'   tab_metrics(sd_age)
#' @export
labs_store <- function(data) {
  codes <- codebook(data)
  attr(data,"codebook") <- codes
  data
}

#' Restore labels from the codebook store in the codebook attribute.
#'
#' `r lifecycle::badge("experimental")`
#'
#' You can store labels before mutate operations by calling
#' \link{labs_store}.
#'
#' @param data A data frame.
#' @param cols A tidyselect column selection.
#' @return A data frame.
#' @examples
#' library(dplyr)
#' library(volker)
#'
#' volker::chatgpt |>
#'   labs_store() |>
#'   mutate(sd_age = 2024 - sd_age) |>
#'   labs_restore() |>
#'   tab_metrics(sd_age)
#' @export
labs_restore <- function(data, cols = NULL) {

  codes <- attr(data,"codebook")

  if (is.data.frame(codes)) {
    data <- labs_apply(data, codes, {{ cols }}, items = TRUE, values = TRUE)
  } else  {
    warning("No codebook found in the attributes.")
  }
  data
}

#' Set column and value labels
#'
#' `r lifecycle::badge("experimental")`
#'
#' You can either provide a data frame in \link{codebook} format to the codes-parameter
#' or provide named lists to the items- or values-parameter.
#'
#' When working with a codebook in the codes-parameter:
#'
#' - Change column labels by providing the columns item_name and item_label in the codebook.
#'   Set the items-parameter to TRUE (the default setting).
#' - Change value labels by providing the columns value_name and value_label in the codebook.
#'   To tell which columns should be changed, you can either use the item_name column in the codebook
#'   or use the cols-parameter.
#'   For factor values, the levels and their order are retrieved from the value_label column.
#'   For coded values, labels are retrieved from both the columns value_name and value_label.
#'
#' When working with lists in the items- or values-parameter:
#'
#' - Change column labels by providing a named list to the items-parameter. The list contains labels named by the columns.
#'   Set the parameters codes and cols to NULL (their default value).
#' - Change value labels by providing a named list to the values-parameter. The list contains labels named by the values.
#'   Provide the column selection in the cols-parameter.
#'   Set the codes-parameter to NULL (its default value).
#'
#' @param data A tibble containing the dataset.
#' @param codes A tibble in \link{codebook} format.
#' @param cols A tidy column selection. Set to NULL (default) to apply to all columns
#'             found in the codebook.
#'             Restricting the columns is helpful when you  want to set value labels.
#'             In this case, provide a tibble with value_name and value_label columns
#'             and specify the columns that should be modified.
#' @param items If TRUE, column labels will be retrieved from the codes (the default).
#'              If FALSE, no column labels will be changed.
#'              Alternatively, a named list of column names with their labels.
#' @param values If TRUE, value labels will be retrieved from the codes (default).
#'               If FALSE, no value labels will be changed.
#'               Alternatively, a named list of value names with their labels.
#'               In this case, use the cols-Parameter to define which columns should be changed.
#' @return A tibble containing the dataset with new labels.
#' @examples
#' library(volker)
#'
#' # Set column labels using the items-parameter
#' volker::chatgpt %>%
#'   labs_apply(
#'    items = list(
#'      "cg_adoption_advantage_01" = "Allgemeine Vorteile",
#'      "cg_adoption_advantage_02" = "Finanzielle Vorteile",
#'      "cg_adoption_advantage_03" = "Vorteile bei der Arbeit",
#'      "cg_adoption_advantage_04" = "Macht mehr Spaß"
#'    )
#'  ) %>%
#'  tab_metrics(starts_with("cg_adoption_advantage_"))
#'
#' # Set value labels using the values-parameter
#'  volker::chatgpt %>%
#'    labs_apply(
#'      cols=starts_with("cg_adoption"),
#'      values = list(
#'        "1" = "Stimme überhaupt nicht zu",
#'        "2" = "Stimme nicht zu",
#'        "3" = "Unentschieden",
#'        "4" = "Stimme zu",
#'        "5" =  "Stimme voll und ganz zu"
#'      )
#'    ) %>%
#'    plot_metrics(starts_with("cg_adoption"))
#'
#' @importFrom rlang .data
#' @export
labs_apply <- function(data, codes = NULL, cols = NULL, items = TRUE, values = TRUE) {

  # Convert lists to data frames
  if (!is.null(items) && is.vector(items) && !is.null(names(items))) {
    items <- as.list(items)
  }

  if (!is.null(values) &&is.vector(values) && !is.null(names(values))) {
    values <- as.list(values)
  }


  if (is.list(items)) {

    codes <- data.frame(
        item_name = names(items),
        item_label = unlist(items, use.names = FALSE),
        stringsAsFactors = FALSE
      )

    items = TRUE
  }

  else if (is.list(values)) {
    codes <- data.frame(
      value_name = names(values),
      value_label = unlist(values, use.names = FALSE),
      stringsAsFactors = FALSE
    )

    values = TRUE
  }

  # Check
  if (is.null(codes) || (nrow(codes) ==0)) {
    return (data)
  }


  # Fix column names
  if (!"item_name" %in% colnames(codes) && (colnames(codes)[1] != "value_name")) {
    colnames(codes)[1] <- "item_name"
  }
  if (!"item_label" %in% colnames(codes) && (colnames(codes)[2] != "value_label")) {
    colnames(codes)[2] <- "item_label"
  }

  # Set column labels (= comment attributes)
  items <- items &&
    ("item_name" %in% colnames(codes)) &&
    ("item_label" %in% colnames(codes))

  if (items) {
    lastitem <- ""
    for (no in c(1:nrow(codes))) {
      item_name <- codes$item_name[no]
      if (item_name %in% colnames(data) & (item_name != lastitem)) {
        comment(data[[item_name]]) <- codes$item_label[no]
      }
      lastitem <- item_name
    }
  }

  # Set value labels
  values <- values &&
    ("value_name" %in% colnames(codes)) &&
    ("value_label" %in% colnames(codes))

  if (values) {
    cols_expr <- rlang::enquo(cols)
    if (rlang::quo_is_null(cols_expr)) {
      cols <- colnames(data)
    }
    else {
      cols <- colnames(dplyr::select(data, {{ cols }}))
    }


    for (col in cols) {
      value_rows <- codes

      if ("item_name" %in% colnames(value_rows)) {
        value_rows <- value_rows |>
          dplyr::filter(.data$item_name == col)
      }


      value_rows <- value_rows |>
        dplyr::select(tidyselect::any_of(c("value_name", "value_label","item_class"))) |>
        dplyr::filter(
          !is.null(.data$value_name), !is.null(.data$value_label),
          !is.na(.data$value_name), !is.na(.data$value_label)
        )

      if (nrow(value_rows) > 0) {
        value_factor <- (
            ("item_class" %in% colnames(value_rows)) &&
            any(value_rows$item_class == "factor", na.rm= TRUE)
        )

        # Factor order
        if (value_factor) {
          value_levels <- unique(value_rows$value_name)
          current_levels <- stats::na.omit(unique(data[[col]]))

          if (length(setdiff(current_levels, value_levels)) > 0) {
            warning(paste0(
              "Check your levels: Not all factor levels of ", col,
              " are present in the codebook. The old levels were kept.")
            )
          } else {
            # Reorder levels
            data[[col]] <- .factor_with_attr(data[[col]], levels=value_levels)
          }
        }

        # Labels with numeric and boolean names are stored directly as attributes in the column (label_nested is FALSE)
        # All other labels are stored as a list in the "label" attribute (label_nested is TRUE)
        else {

          label_attr <- list()
          label_nested <- TRUE

          for (vr in c(1:nrow(value_rows))) {
            value_name <- value_rows$value_name[vr]
            value_label <- value_rows$value_label[vr]

            # Numeric or boolean values
            if (grepl("^-?[0-9TF]+$", value_name)) {
              label_nested <- FALSE
            }
            label_attr[[as.character(value_name)]] <- value_label
          }

          if (length(label_attr) > 0) {
            if (!label_nested) {
              attr(data[[col]], "labels") <- NULL
              for (value_name in names(label_attr)) {
                value_label <- label_attr[[value_name]]
                attr(data[[col]], as.character(value_name)) <- NULL
                attr(data[[col]], as.character(value_name)) <- value_label
              }
            } else {
              attr(data[[col]], "labels") <- label_attr
            }
          }
        }
      }
    }
  }

  data
}


#' Remove all comments from the selected columns
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A tibble.
#' @param cols Tidyselect columns.
#' @param labels The attributes to remove. NULL to remove all attributes except levels and class.
#' @return A tibble with comments removed.
#' @examples
#' library(volker)
#' volker::chatgpt |>
#'   labs_clear()
#' @export
labs_clear <- function(data, cols, labels = NULL) {


  remove_attr <-  function(x, labels = NULL) {
    if (is.null(labels)) {
      labels <- setdiff(names(attributes(x)), c("class", "levels"))
    }

    for (label in labels) {
      attr(x, label) <- NULL
    }
    x
  }

  if (missing(cols)) {
    data <-  dplyr::mutate(data, dplyr::across(tidyselect::everything(), ~remove_attr(., labels)))
  } else {
    data <-  dplyr::mutate(data, dplyr::across({{ cols }}, ~remove_attr(., labels)))
  }
  data
}

#' Replace item value names in a column by their labels
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding item values.
#' @param codes The codebook to use: A tibble with the columns
#'              value_name and value_label.
#'              Can be created by the \link{codebook} function, e.g. by calling
#'              `codes <- codebook(data, myitemcolumn)`.
#' @param col_from The tidyselect column with source values, defaults to value_name.
#'               If the column is not found in the codebook, the first column is used.
#' @param col_to The tidyselect column with target values, defaults to value_label.
#'               If the column is not found in the codebook, the second column is used
#' @param na.missing By default, the column is converted to a factor with levels combined from the codebook and the data.
#'                Set na.missing to TRUE to set all levels not found in the codes to NA.
#' @return Tibble with new labels.
labs_replace <- function(data, col, codes, col_from="value_name", col_to="value_label", na.missing = FALSE) {

  # Column without quotes
  # TODO: could we just use "{{ col }}" with quotes in mutate below?
  # See example at https://rlang.r-lib.org/reference/topic-data-mask-ambiguity.html
  if (rlang::quo_is_symbol(rlang::enquo(col))) {
    col <- rlang::enquo(col)
  }
  # Column as character
  else {
    col <- rlang::sym(col)
  }

  # Get codebook columns
  if (rlang::quo_is_symbol(rlang::enquo(col_from))) {
    col_from <- rlang::enquo(col_from)
  } else {
    if (!(col_from %in% colnames(codes))) {
      colnames(codes)[1] <- col_from
    }
    col_from <- rlang::sym(col_from)
  }
  if (rlang::quo_is_symbol(rlang::enquo(col_to))) {
    col_to <- rlang::enquo(col_to)
  } else {
    if (!(col_to %in% colnames(codes))) {
      colnames(codes)[2] <- col_to
    }
    col_to <- rlang::sym(col_to)
  }

  codes <- dplyr::rename(codes,.from = !!col_from)
  codes <- dplyr::rename(codes,.to = !!col_to)

  # Store levels
  before_levels <- data |>
    dplyr::distinct(!!col) |>
    dplyr::arrange(!!col) |>
    dplyr::mutate(!!col := as.character(!!col)) |>
    dplyr::rename(.from = !!col)

  # Store title
  before_comment <- attr(data[[as_name(col)]], "comment", exact = TRUE)

  codes <- codes %>%
    dplyr::filter(as.character(.data$.from) %in% before_levels$.from) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of(c(".from", ".to")))) %>%
    stats::na.omit()


  if (nrow(codes) > 0) {

    # If any values were missing in the codes, add them
    # and order as before.
    if  (!na.missing && !all((before_levels$.from %in% codes$.from))) {
      codes <- before_levels |>
        dplyr::left_join(codes, by=".from") |>
        dplyr::mutate(.to = dplyr::coalesce(.data$.to, .data$.from))
    }

    data <- data %>%
      dplyr::mutate(.from = as.character(!!col)) %>%
      dplyr::left_join(codes, by = ".from") %>%
      dplyr::mutate(!!col := .data$.to)


    data <- dplyr::mutate(data, !!col := factor(!!col, levels=codes$.to))
    data <- dplyr::select(data, -tidyselect::all_of(c(".from", ".to")))

    # Restore title
    attr(data[[as_name(col)]], "comment") <- before_comment
  }

  data
}

#' Add missing residual labels in numeric columns that have at least one labeled value
#'
#' @keywords internal
#'
#' @param data A tibble
#' @return A tibble with added value labels
labs_impute <- function(data) {

  na.numbers <- cfg_get_na_numbers()
  if (length(na.numbers) < 1) {
    return(data)
  }

  codes <- data |>
    codebook() |>
    dplyr::filter(.data$item_class == "numeric") |>
    dplyr::filter(!is.na(.data$value_name))

  for (col in unique(codes$item_name)) {

    # Get numeric values from the codebook
    vals_codebook <- codes$value_name[codes$item_name == col]
    vals_codebook <- stats::na.omit(suppressWarnings(as.numeric(unique(vals_codebook))))

    # Get residual values from the dataset
    vals_data <- stats::na.omit(suppressWarnings(as.numeric(unique(data[[col]]))))
    vals_data <- vals_data[vals_data %in% na.numbers]

    if (length(vals_data > 0)) {
      vals_missing <- setdiff(vals_data, vals_codebook)
      for (val in vals_missing) {
        attr(data[[col]], as.character(val)) <- as.character(val)
      }
    }
  }

  data
}

#' Get a common title for a column selection
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols A tidy column selection.
#' @param default A character string used in case not prefix is found
#' @return A character string.
#' @importFrom rlang .data
get_title <- function(data, cols, default=NULL) {
  labels <- data %>%
    codebook({{ cols }}) |>
    tidyr::drop_na("item_label") |>
    dplyr::distinct(dplyr::across(tidyselect::all_of("item_label")))

  if (nrow(labels) > 0) {
    labels <- labels$item_label
  } else {
    labels <- dplyr::select(data, {{ cols }}) %>% colnames()
  }

  prefix <- get_prefix(labels)
  prefix <- trim_label(prefix)

  if ((prefix == "") & (length(labels) == 1)) {
    prefix <- labels[[1]]
  } else if (prefix == "") {
    prefix <- default
  }

  prefix
}

#' Get the numeric range from the labels
#'
#' Gets the range of all values in the selected columns
#' by the first successful of the following methods:
#'
#' - Inspect the limits column attribute.
#' - Lookup the value names in the codebook.
#' - Calculate the range from all values in the columns.
#'
#' @keywords internal
#'
#' @param data The labeled data frame.
#' @param cols A tidy variable selection.
#' @param negative Whether to include negative values.
#' @return A list or NULL.
#' @importFrom rlang .data
get_limits <- function(data, cols, negative = TRUE) {

  # First, try to get limits from the column attributes
  values <- data %>%
    dplyr::select({{ cols }}) %>%
    lapply(attr, "limits") %>%
    unlist()

  # Second, try to get limits from the column labels
  na.numbers <- cfg_get_na_numbers()

  if (is.null(values)) {
    values <- codebook(data, {{ cols }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of("value_name"))) %>%
      filter(!(.data$value_name %in% na.numbers)) |>
      dplyr::pull(.data$value_name)
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
    return(range(values, na.rm = TRUE))
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
#' @param data The dataframe.
#' @param cols The tidy selection.
#' @param extract Whether to extract numeric values from characters.
#' @return 0 = an undirected scale, -1 = descending values, 1 = ascending values.
#' @importFrom rlang .data
get_direction <- function(data, cols, extract = TRUE) {
  data <- dplyr::select(data, {{ cols }})

  # Get all values
  categories <- data %>%
    dplyr::mutate(dplyr::across(tidyselect::everything(), as.character)) %>%
    tidyr::pivot_longer(tidyselect::everything()) %>%
    dplyr::arrange(.data$value) %>%
    dplyr::mutate(value = ifelse(extract, regmatches(.data$value, regexpr("[0-9-]+", .data$value)), .data$value)) |>
    dplyr::distinct(dplyr::across(tidyselect::all_of("value"))) %>%
    dplyr::pull(.data$value)

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
#' @param x Character vector.
#' @param ignore.case Whether case matters (default).
#' @param trim Whether non alphabetic characters should be trimmed.
#' @param delimiters A list of prefix delimiters.
#'                   If any of the delimiters is present in the extracted prefix,
#'                   the part after is removed from the prefix.
#'                   Consider the following two items as an example:
#'                   \code{c("Usage: in private context", "Usage: in work context")}.
#'                   The common prefix would be \preformatted{"Usage: in "}, but it makes
#'                   more sense to break it after the colon.
#' @return The longest common prefix of the strings.
get_prefix <- function(x, ignore.case = FALSE, trim = FALSE, delimiters= c(":","\n")) {

  x <- as.character(x)
  if (ignore.case) {
    x <- toupper(x)
  }
  x <- stats::na.omit(x)

  if (length(x) == 0) {
    return ("")
  }

  if (length(x) == 1) {
    return (x)
  }

  prefix <- ""
  nc <- nchar(x, type = "char")
  for (i in 1:min(nc)) {
    ss <- substr(x, 1, i)
    if (any(ss != ss[1])) {
      prefix <- substr(x[1], 1, i - 1)
      break
    }
  }

  #prefix <- trim_label(substr(x[1], 1, i))

  # Break at delimiters
  for (delimiter in delimiters) {
    pos <- regexpr(delimiter, prefix, fixed = TRUE)
    if (pos[1] > 0) {
      prefix <- substr(prefix, 1, pos[1] - 1)
    }
  }

  if (trim) {
    prefix <- trim_label(prefix)
  }

  prefix
}

length(c("asdasd","sdf"))

#' Wrap a string
#'
#' @keywords internal
#'
#' @param x A character vector.
#' @param width The number of chars after which to break.
#' @return A character vector with wrapped strings.
wrap_label <- function(x, width = 40) {
  # Vectorize
  if (length(x) > 1) {
    return(sapply(x, wrap_label, width))
  }

  # Keep NA
  else if (is.na(x)) {
    return (x)
  }

  # Wrap at word boundaries
  words <- unlist(strsplit(as.character(x), VLKR_WRAP_SEPARATOR, perl = TRUE))

  wrapped <- ""
  line <- ""

  for (word in words) {
    if (nchar(line) + nchar(word) + 1 > width) {
      wrapped <- paste(wrapped, line, sep = "\n")
      line <- word
    } else {
      if (nchar(line) > 0) {
        line <- paste(line, word, sep = " ")
      } else {
        line <- word
      }
    }
  }
  trimws(paste(wrapped, line, sep = "\n"))
}


#' Truncate labels
#'
#' Truncate labels that exceed a specified maximum length.
#'
#' @keywords internal
#'
#' @param x A character vector.
#' @param max_length Maximum length, default is 20. The ellipsis "..." is appended to shortened labels.
#' @return A character vector with truncated labels.
trunc_labels <- function(x, max_length = 20) {
  ifelse(nchar(x) > max_length, paste0(substr(x, 1, max_length), "..."), x)
}

#' Remove trailing zeros and trailing or leading
#' whitespaces, colons,
#' hyphens and underscores
#'
#' @keywords internal
#'
#' @param x A character value.
#' @return The trimmed character value.
trim_label <- function(x) {
  x <- sub("[: ,_-]*$", "", x)
  x <- sub("^[: ,_-]*", "", x)
  x
}

#' Remove a prefix from a character vector or a factor
#'
#' If the resulting character values would be empty,
#' the prefix is returned. At the end, all items
#' in the vector are trimmed using \link{trim_label}.
#'
#' If x is a factor, the order of factor levels is retained.
#'
#' @keywords internal
#'
#' @param x A character or factor vector.
#' @param prefix The prefix. Set to TRUE to first extract the prefix.
#' @return The trimmed character or factor vector.
trim_prefix <- function(x, prefix=TRUE) {

  if (is.factor(x)) {
    levels_x <- levels(x)
  } else {
    levels_x <- c()
  }
  x <- as.character(x)

  if (!is.na(prefix) && (prefix == TRUE)) {
    prefix <- get_prefix(x, trim = TRUE)
  }

  if (!is.na(prefix) && (prefix != "")) {

    levels_x <- sub(prefix, "", levels_x, fixed = TRUE)
    levels_x <- ifelse(levels_x == "", prefix, levels_x)

    x <- sub(prefix, "", x, fixed = TRUE)
    x = ifelse(x == "", prefix, x)
  }

  levels_x <- trim_label(levels_x)
  x <- trim_label(x)

  if (length(levels_x) > 0) {
    x <- factor(x, levels = unique(levels_x))
  }

  return(x)
}


#' Prepare the scale attribute values
#'
#' @keywords internal
#'
#' @param data A tibble with a scale attribute.
#' @return A named list or NULL.
#' @importFrom rlang .data
prepare_scale <- function(data) {
  if (!is.null(data)) {
    data <- data %>%
      dplyr::mutate(value_name = suppressWarnings(as.numeric(.data$value_name))) %>%
      dplyr::filter(.data$value_name >= 0) %>%
      stats::na.omit()

    scale <- stats::setNames(
      as.character(data$value_label),
      as.character(data$value_name)
    )

    return(scale)
  }
  return(NULL)
}

#' Wrap labels in plot scales
#'
#' @keywords internal
#'
#' @param x The label vector.
#' @param scale A named label vector to select elements that should be wrapped.
#'              Prevents numbers from being wrapped.
#' @return A vevtor of wrapped labels.
label_scale <- function(x, scale) {
  ifelse(
    x %in% names(scale),
    wrap_label(scale[as.character(x)], width = dplyr::coalesce(getOption("vlkr.wrap.scale"), VLKR_PLOT_SCALEWRAP)),
    x
  )
}




#' Angle labels
#'
#' Calculate angle for label adjustment based on character length.
#'
#' @keywords internal
#'
#' @param labels Vector of labels to check. The values are converted to characters.
#' @param threshold Length threshold beyond which the angle is applied.
#'                  Default is 20.
#' @param angle The angle to apply if any label exceeds the threshold.
#'            Default is 45.
#' @return A single angle value.
get_angle <- function(labels, threshold = 20, angle = 45) {
  # Check if any label exceeds the threshold and return the angle accordingly
  if (any(nchar(as.character(labels)) > threshold)) {
    return(angle)
  } else {
    return(0)
  }
}
