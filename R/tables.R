#' Output a frequency table
#'
#' The type of frequency table depends on the number of selected columns:
#' - One column: see \link{tab_counts_one}
#' - Multiple columns: see \link{tab_counts_items}
#' - One column and one grouping column: see \link{tab_counts_one_grouped}
#' - Multiple columns and one grouping column: see \link{tab_counts_items_grouped}
#'
#'
#' @param data A data frame
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with()
#' @param col_group Optional, a grouping column. The column name without quotes.
#' @param ... Other parameters passed to the appropriate table function
#' @return A tibble
#' @export
tab_counts <- function(data, cols, col_group=NULL, ...) {
  # Check
  check_is_dataframe(data)

  # Find columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  col_group_eval <- tidyselect::eval_select(expr = enquo(col_group), data = data)
  is_items <- length(cols_eval) > 1
  is_grouped <- length(col_group_eval)== 1

  # Single variables
  if (!is_items && !is_grouped) {
    tab_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped) {
    tab_counts_one_grouped(data, {{ cols }}, {{ col_group }}, ...)
  }

  # Items
  else if (is_items && !is_grouped) {
    tab_counts_items(data, cols , ...)
  }
  else if (is_items && is_grouped) {
    tab_counts_items_grouped(data, {{ cols }}, {{ col_group }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not supported by volker functions.")
  }

}

#' Output a table with distribution parameters
#'
#' The table type depends on the number of selected columns:
#' - One column: see \link{tab_metrics_one}
#' - Multiple columns: see \link{tab_metrics_items}
#' - One column and one grouping column: see \link{tab_metrics_one_grouped}
#' - Multiple columns and one grouping column: see \link{tab_metrics_items_grouped}
#'
#'
#' @param data A data frame
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param col_group Optional, a grouping column (without quotes).
#' @param ... Other parameters passed to the appropriate table function
#' @return A tibble
#' @export
tab_metrics <- function(data, cols, col_group=NULL, ...) {
  # Check
  check_is_dataframe(data)

  # Find columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  col_group_eval <- tidyselect::eval_select(expr = enquo(col_group), data = data)
  is_items <- length(cols_eval) > 1
  is_grouped <- length(col_group_eval)== 1

  # Single variables
  if (!is_items && !is_grouped) {
    tab_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped) {
    tab_metrics_one_grouped(data, {{ cols }}, {{ col_group }}, ...)
  }

  # Items
  else if (is_items && !is_grouped) {
    tab_metrics_items(data, cols , ...)
  }
  else if (is_items && is_grouped) {
    tab_metrics_items_grouped(data, {{ cols }}, {{ col_group }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not supported by volker functions.")
  }

}

#' Output a frequency table for the values in one column
#'
#' @param data A tibble
#' @param col The column holding values to count
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .formatted Set to FALSE to prevent calculating percents from proportions
#' @export
tab_counts_one <- function(data, col, .labels = T, .formatted = T) {
  # Check parameters
  check_is_dataframe(data)

  result <- data %>%
    dplyr::count({{ col }}) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(p = n / sum(n))

  # Get variable caption from the attributes
  if (.labels) {
    label <- get_col_label(data, {{ col }})
    result <- dplyr::rename(result, {{ label }} := {{ col }})
  }

  if (.formatted) {
    result <- dplyr::mutate(result, p = paste0(round(p * 100, 0), "%"))
  }

  # Totals
  result_total <- tibble(
    "Total",
    sum(!is.na(dplyr::select(data, {{ col }}))),
    ifelse(.formatted, "100%", 1)
  )
  colnames(result_total) <- colnames(result)

  # Missings
  result_missing <- tibble(
    "Missing",
    sum(is.na(dplyr::select(data, {{ col }}))),
    ifelse(.formatted, "", NA)
  )
  colnames(result_missing) <- colnames(result)

  # Build table
  result <- bind_rows(result, result_total, result_missing)

  .to_vlk_tab(result)
}

#' Output frequencies cross tabulated with a grouping column
#'
#' @param data A tibble
#' @param col The column holding factor values
#' @param col_group The column holding groups to compare
#' @param values The values to output: n (frequency) or p (percentage).
#' @param prop The basis of percent calculation: total (the default), cols, or rows
#' @param missings Include missing values (default FALSE)
#' @param .formatted Set to FALSE to prevent calculating percents from proportions
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
tab_counts_one_grouped <- function(data, col, col_group, values = c("n", "p"), prop = "total", missings = F, .formatted = T, .labels = T) {

  # Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ col_group }})

  # Remove missings
  if (!missings) {
    data <- data %>%
      tidyr::drop_na({{ col }}, {{ col_group }})
  }

  #
  # 1. Count
  #
  grouped <- data %>%
    dplyr::count({{ col }}, {{ col_group }}) %>%
    dplyr::mutate(
      {{ col_group }} := tidyr::replace_na(as.character({{ col_group }}), "Missing"),
      {{ col }} := tidyr::replace_na(as.character({{ col }}), "Missing")
    )

  #
  # 2. N
  #
  rows_n <- grouped %>%
    dplyr::select({{ col_group }}, {{ col }}, n) %>%
    tidyr::pivot_wider(
      names_from = {{ col }},
      values_from = n,
      values_fill = list(n = 0)
    )

  # Total column
  total_col_n <- data %>%
    dplyr::count({{ col_group }}) %>%
    dplyr::mutate(
      {{ col_group }} := tidyr::replace_na(as.character({{ col_group }}), "Missing")
    ) %>%
    dplyr::select({{ col_group }}, Total = n)

  # Total row
  total_row_n <- grouped %>%
    dplyr::group_by({{ col }}) %>%
    dplyr::summarise(n = sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate({{ col_group }} := "Total") %>%
    tidyr::pivot_wider(
      names_from = {{ col }},
      values_from = n,
      values_fill = list(n = 0)
    )

  # Total
  total_n <- data %>%
    dplyr::count() %>%
    mutate({{ col_group }} := "Total") %>%
    dplyr::select({{ col_group }}, Total = n)

  # Join
  result_n <-
    dplyr::full_join(
      total_col_n,
      rows_n,
      by = as.character(rlang::get_expr(rlang::enquo(col_group)))
    ) %>%
    dplyr::bind_rows(
      left_join(
        total_n,
        total_row_n,
        by = as.character(rlang::get_expr(rlang::enquo(col_group)))
      )
    )

  #
  # 3. P
  #
  if (prop == "cols") {
    rows_p <- grouped %>%
      dplyr::group_by({{ col }}) %>%
      dplyr::mutate(p = n / sum(n)) %>%
      dplyr::ungroup()

    total_col_p <- total_col_n %>%
      dplyr::mutate(Total = Total / sum(Total))

    total_row_p <- total_row_n %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~1))
  } else if (prop == "rows") {
    rows_p <- grouped %>%
      dplyr::group_by({{ col_group }}) %>%
      dplyr::mutate(p = n / sum(n)) %>%
      dplyr::ungroup()

    total_col_p <- total_col_n %>%
      dplyr::mutate(Total = 1)

    total_row_p <- total_row_n %>%
      dplyr::mutate(across(tidyselect::where(is.numeric), ~ .x / total_n$Total))
  } else {
    rows_p <- grouped %>%
      dplyr::mutate(p = n / sum(n))

    total_col_p <- total_col_n %>%
      dplyr::mutate(Total = Total / sum(Total))

    total_row_p <- total_row_n %>%
      dplyr::mutate(across(tidyselect::where(is.numeric), ~ .x / total_n$Total))
  }

  rows_p <- rows_p %>%
    dplyr::select({{ col }}, {{ col_group }}, p) %>%
    tidyr::pivot_wider(
      names_from = {{ col }},
      values_from = p,
      values_fill = list(p = 0)
    )

  total_p <- tibble("Total" = 1) %>%
    mutate({{ col_group }} := "Total")

  # Join
  result_p <-
    dplyr::full_join(
      total_col_p,
      rows_p,
      by = as.character(rlang::get_expr(rlang::enquo(col_group)))
    ) %>%
    dplyr::bind_rows(
      left_join(
        total_p,
        total_row_p,
        by = as.character(rlang::get_expr(rlang::enquo(col_group)))
      )
    )

  # Round and add % sign
  if (.formatted) {
    result_p <- result_p %>%
      dplyr::mutate(across(where(is.numeric), ~ paste0(round(. * 100, 0), "%")))
  }


  # Zip
  if (("n" %in% values) && ("p" %in% values)) {
    result <- zip_tables(result_p, result_n, brackets = T)
  } else if ("p" %in% values) {
    result <- result_p
  } else {
    result <- result_n
  }

  # Get item label from the attributes
  if (.labels) {
    codes <- data %>%
      get_labels({{ col_group }}) %>%
      dplyr::distinct(item_name, item_label) %>%
      na.omit()

    if (nrow(codes) > 0) {
      label <- codes$item_label[1]
      result <- result %>%
        dplyr::rename({{ label }} := {{ col_group }})
    }
  }

  .to_vlk_tab(result, digits= 0)
}

#' Output frequencies for multiple variables
#'
#' TODO: Reorder boolean categories: first TRUE, then FALSE
#' TODO: Support single columns
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param values The values to output: n (frequency) or p (percentage)
#' @param missings Include missing values (default FALSE)
#' @param .formatted Set to FALSE to prevent calculating percents from proportions
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @param .quiet Set to true to suppress printing the output
#' @export
tab_counts_items <- function(data, cols, values = c("n", "p"), missings=F, .formatted = T, .labels = T) {
  # Check parameters
  check_is_dataframe(data)

  # Remove missings
  if (!missings) {
    data <- data %>%
      tidyr::drop_na({{ cols }})
  }


  # Calculate n and p
  result <- data %>%
    tidyr::drop_na({{ cols }}) %>%
    remove_labels(tidyselect::all_of(cols)) %>%
    tidyr::pivot_longer(
      tidyselect::all_of(cols),
      names_to = "item",
      values_to = "value"
    ) %>%
    dplyr::count(item, value) %>%
    dplyr::group_by(item) %>%
    dplyr::mutate(p = n / sum(n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = as.factor(value)) %>%
    dplyr::arrange(value)

  # Absolute frequency
  value <- "n"
  result_n <- result %>%
    dplyr::select(item, value, !!sym(value)) %>%
    tidyr::pivot_wider(
      names_from = value,
      values_from = !!sym(value),
      values_fill = setNames(list(0), value)
    ) %>%
    janitor::adorn_totals("col")

  # Relative frequency
  value <- "p"
  result_p <- result %>%
    dplyr::select(item, value, !!sym(value)) %>%
    tidyr::pivot_wider(
      names_from = value,
      values_from = !!sym(value),
      values_fill = setNames(list(0), value)
    ) %>%
    janitor::adorn_totals("col")

  # Add % sign
  if (.formatted) {
    result_p <- dplyr::mutate(result_p, across(where(is.numeric), ~ paste0(round(. * 100, 0), "%")))
  }

  # Add missings

  if (missings) {
    result_missing <-  data %>%
      remove_labels(tidyselect::all_of(cols)) %>%
      tidyr::pivot_longer(
        tidyselect::all_of(cols),
        names_to = "item",
        values_to = "value"
      ) %>%
      dplyr::mutate(value = is.na(value)) %>%
      dplyr::count(item, value) %>%
      mutate(value = factor(value,levels=c("TRUE","FALSE"))) %>%
      tidyr::pivot_wider(
        names_from = value,
        values_from = n,
        values_fill = 0,
        names_expand=T
      ) %>%
      dplyr::select(item, Missing = "TRUE")

    result_n <- left_join(result_n, result_missing, by="item")
    result_p <- mutate(result_p, Missing="")
  }
  # Combine n and p if requested
  if (("n" %in% values) && ("p" %in% values)) {
    result <- zip_tables(result_p, result_n, brackets = T, newline = F)
  } else if ("p" %in% values) {
    result <- result_p
  } else {
    result <- result_n
  }

  # Replace item labels
  if (.labels) {
    result <- replace_item_values(result, data, cols)
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item)
  if (prefix != "") {
    result <- dplyr::mutate(result, item = stringr::str_remove(item, prefix))
    result <- dplyr::mutate(result, item = ifelse(item == "", prefix, item))
  }

  # Replace category labels
  if (.labels) {
    labels_categories <- data %>%
      get_labels(!!cols) %>%
      dplyr::distinct(value_name, value_label) %>%
      na.omit()

    if (nrow(labels_categories) > 0) {
      colnames(result) <- sapply(
        colnames(result),
        function(x) {
          dplyr::coalesce(
            setNames(labels_categories$value_label, labels_categories$value_name)[x],
            x
          )
        }
      )
    }
  }


  # Rename first column
  if (prefix != "") {
    colnames(result)[1] <- sub("[ :,]+$", "", prefix)
  } else {
    result <- rename(result, Item = item)
  }

  .to_vlk_tab(result, digits= 0)
}


#' Compare the values in multiple items by a grouping column.
#'
#' TODO: implement -> focus one category and show n / p
#'
#' @param data A tibble
#' @param cols The item columns that hold the values to summarize
#' @param cols_group The columns holding groups to compare
#' @param values The output values, n or p or both
#' @param category The category within the items to focus
#' @param digits The digits to print
#' @param .negative If True (default), negative values are recoded to missing values
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @keywords internal
#' @export
tab_counts_items_grouped <- function(data, cols, cols_groups, values = c("n", "p"), category = NULL, digits = 1, .labels = T) {
  # Check parameters
  check_is_dataframe(data)
  stop("Not implemented yet")
}

#' Correlate the values in multiple items
#'
#' TODO: implement -> calculate npmi or similar for one value
#'
#' @param data A tibble
#' @param cols1 The item columns
#' @param cols2 The item columns
#' @param values The output values
#' @param category The category within the items to focus
#' @param digits The digits to print
#' @param .negative If True (default), negative values are recoded to missing values
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @keywords internal
#' @export
tab_counts_items_cor <- function(data, cols1, cols2, values = c("n", "p"), category = NULL, digits = 1, .labels = T) {
  # Check parameters
  check_is_dataframe(data)
  stop("Not implemented yet")
}

#' Output a five point summary table for the values in multiple columns
#'
#' @param data A tibble
#' @param col The columns holding metric values
#' @param digits The digits to print
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
tab_metrics_one <- function(data, col, digits = 1, .labels = T) {
  # Check parameters
  check_is_dataframe(data)

  result <- data %>%
    skim_metrics({{ col }}) %>%
    dplyr::select(
      "item" = skim_variable,
      min = numeric.min,
      q1 = numeric.q1,
      median = numeric.median,
      q3 = numeric.q3,
      max = numeric.max,
      m = numeric.mean,
      sd = numeric.sd,
      missing,
      n,
      items = numeric.items,
      alpha = numeric.alpha
    )

  # Remove items and alpha if not and index
  if (all(is.na(result$items)) || all(is.na(result$alpha))) {
    result$items <- NULL
    result$alpha <- NULL
  } else {
    result <- result %>%
      dplyr::mutate(across(c(items), ~ as.character(round(., 0)))) %>%
      dplyr::mutate(across(c(alpha), ~ as.character(round(., 2))))
  }

  result <- result %>%
    dplyr::mutate(across(c(missing, n), ~ as.character(round(., 0)))) %>%
    dplyr::mutate(across(c(min, q1, median, q3, max), ~ as.character(round(., digits)))) %>%
    dplyr::mutate(across(c(m, sd), ~ as.character(round(., digits)))) %>%
    # remove_labels(-item) %>%
    tidyr::pivot_longer(-item) %>%
    dplyr::select(-item, {{ col }} := name, value)

  # Get item label from the attributes
  if (.labels) {
    label <- get_col_label(data, {{ col }})
    result <- result %>%
      dplyr::rename({{ label }} := {{ col }})
  }

  .to_vlk_tab(result)
}


#' Output a five point summary for groups
#'
#' @param data A tibble
#' @param col The column holding metric values
#' @param col_group The column holding groups to compare
#' @param .negative If True (default), negative values are recoded to missing values
#' @param digits The digits to print
#' @param .labels If TRUE (default) extracts item labels from the attributes, see get_labels()
#' @export
tab_metrics_one_grouped <- function(data, col, col_group, .negative = F, digits = 1, .labels = T) {
  # Check parameters
  check_is_dataframe(data)

  # TODO: warn if any negative values were recoded
  # TODO: only for the metric column (col parameter)
  if (!.negative) {
    data <- dplyr::mutate(data, across(where(is.numeric), ~ if_else(. < 0, NA, .)))
  }

  result_grouped <- data %>%
    dplyr::group_by({{ col_group }}) %>%
    skim_metrics({{ col }}) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      {{ col_group }} := tidyr::replace_na(as.character({{ col_group }}), "Missing")
    ) %>%
    dplyr::select(-skim_variable, -skim_type)

  result_total <- data %>%
    skim_metrics({{ col }}) %>%
    dplyr::mutate({{ col_group }} := "Total")

  result <- bind_rows(
    result_grouped,
    result_total
  ) %>%
    dplyr::select(
      {{ col_group }},
      min = numeric.min,
      q1 = numeric.q1,
      median = numeric.median,
      q3 = numeric.q3,
      max = numeric.max,
      m = numeric.mean,
      sd = numeric.sd,
      missing,
      n,
      items = numeric.items,
      alpha = numeric.alpha
    )

  # Remove items and alpha if not and index
  if (all(is.na(result$items)) || all(is.na(result$alpha))) {
    result$items <- NULL
    result$alpha <- NULL
  } else {
    result <- result %>%
      dplyr::mutate(across(c(items), ~ as.character(round(., 0)))) %>%
      dplyr::mutate(across(c(alpha), ~ as.character(round(., 2))))
  }

  # Get item label from the attributes
  if (.labels) {
    codes <- data %>%
      get_labels({{ col_group }}) %>%
      dplyr::distinct(item_name, item_label) %>%
      na.omit()

    if (nrow(codes) > 0) {
      label <- codes$item_label[1]
      result <- result %>%
        dplyr::rename({{ label }} := {{ col_group }})
    }

    scale <- attr(pull(data, {{ col }}), "scale")
    if (is.null(scale)) {
      scale <- data %>%
        get_labels({{ col }}) %>%
        distinct(value_name, value_label)
    }
    attr(result, "scale")
  }

  # TODO: Add limits
  # attr(data[[newcol]],"limits")

  .to_vlk_tab(result, digits= digits)
}


#' Output a five point summary table for multiple items
#'
#' @param data A tibble
#' @param cols The columns holding metric values
#' @param digits The digits to print
#' @param .negative If True (default), negative values are recoded to missing values
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
tab_metrics_items <- function(data, cols, digits = 1, .negative = F, .labels = T) {
  # Check parameters
  check_is_dataframe(data)

  cols <- enquo(cols)

  result <- data %>%
    dplyr::select(!!cols)

  # TODO: warn if any negative values were recoded
  if (!.negative) {
    result <- dplyr::mutate(result, across(where(is.numeric), ~ if_else(. < 0, NA, .)))
  }

  result <- result %>%
    skim_metrics()

  result <- result %>%
    dplyr::select(
      "item" = skim_variable,
      min = numeric.min,
      q1 = numeric.q1,
      median = numeric.median,
      q3 = numeric.q3,
      max = numeric.max,
      m = numeric.mean,
      sd = numeric.sd,
      missing,
      n,
      items = numeric.items,
      alpha = numeric.alpha
    )

  # Remove items and alpha if not and index
  if (all(is.na(result$items)) || all(is.na(result$alpha))) {
    result$items <- NULL
    result$alpha <- NULL
  } else {
    result <- result %>%
      dplyr::mutate(across(c(items), ~ as.character(round(., 0)))) %>%
      dplyr::mutate(across(c(alpha), ~ as.character(round(., 2))))
  }

  # Get item labels from the attributes
  if (.labels) {
    result <- replace_item_values(result, data, cols)
    attr(result, "limits") <- get_limits(data, !!cols, .negative)

    attr(result, "scale") <- get_labels(data, !!cols) %>%
      distinct(value_name, value_label)
  }

  # Remove common item prefix and title
  # TODO: remove common postfix
  prefix <- get_prefix(result$item)
  if (prefix != "") {
    result <- dplyr::mutate(result, item = stringr::str_remove(item, stringr::fixed(prefix)))
    result <- dplyr::mutate(result, item = ifelse(item == "", prefix, item))
  }


  # Rename first column
  if (prefix != "") {
    colnames(result)[1] <- sub("[ :,]+$", "", prefix)
  } else {
    result <- rename(result, Item = item)
  }

  .to_vlk_tab(result, digits= digits)
}



#' Output the means for groups in one or multiple columns
#'
#' TODO: handle completely missing data in single groups
#'
#' @param data A tibble
#' @param cols The item columns that hold the values to summarize
#' @param cols_group The columns holding groups to compare
#' @param values The output metrics, mean or sd
#' @param digits The digits to print
#' @param .negative If True (default), negative values are recoded to missing values
#' @param .labels If True (default) extracts item labels from the attributes, see get_labels()
#' @export
tab_metrics_items_grouped <- function(data, cols, cols_groups, values = c("mean", "sd"), digits = 1, .negative = F, .labels = T) {
  # Check parameters
  check_is_dataframe(data)

  # Get positions of group cols
  cols_groups <- tidyselect::eval_select(expr = enquo(cols_groups), data = data)
  cols <- enquo(cols)

  # TODO: warn if any negative values were recoded
  # TODO: only selected columns
  if (!.negative) {
    data <- dplyr::mutate(data, across(where(is.numeric), ~ if_else(. < 0, NA, .)))
  }

  # Total means
  value <- "numeric.mean"
  total_mean <- data %>%
    dplyr::select(!!cols) %>%
    skim_metrics() %>%
    dplyr::select(skim_variable, Total = !!sym(value))

  # Total sd
  value <- "numeric.sd"
  total_sd <- data %>%
    dplyr::select(!!cols) %>%
    skim_metrics() %>%
    dplyr::select(skim_variable, Total = !!sym(value))

  # Grouped means
  value <- "numeric.mean"
  grouped_mean <- map(
    cols_groups,
    function(col) {
      col <- names(data)[col]

      data %>%
        dplyr::filter(!is.na(!!sym(col))) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::select(!!sym(col), !!cols) %>%
        skim_metrics() %>%
        dplyr::ungroup() %>%
        dplyr::select(skim_variable, !!sym(col), !!sym(value)) %>%
        tidyr::pivot_wider(
          names_from = !!sym(col),
          values_from = !!sym(value)
        )
    }
  ) %>%
    purrr::reduce(
      dplyr::inner_join,
      by = "skim_variable"
    )

  # Grouped sd
  value <- "numeric.sd"
  grouped_sd <- map(
    cols_groups,
    function(col) {
      col <- names(data)[col]

      data %>%
        dplyr::filter(!is.na(!!sym(col))) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::select(!!sym(col), !!cols) %>%
        skim_metrics() %>%
        dplyr::ungroup() %>%
        dplyr::select(skim_variable, !!sym(col), !!sym(value)) %>%
        tidyr::pivot_wider(
          names_from = !!sym(col),
          values_from = !!sym(value)
        )
    }
  ) %>%
    purrr::reduce(
      dplyr::inner_join,
      by = "skim_variable"
    )

  # Significance of lm
  # TODO
  # grouped_p <- map(
  #   cols_groups,
  #   function(col) {
  #     col <- names(data)[col]
  #
  #     data %>%
  #       dplyr::filter(!is.na(!!sym(col))) %>%
  #       dplyr::group_by(!!sym(col)) %>%
  #       dplyr::select(!!sym(col),!!cols) %>%
  #       skim_metrics() %>%
  #       dplyr::ungroup() %>%
  #       dplyr::select(skim_variable, !!sym(col), !!sym(value)) %>%
  #       tidyr::pivot_wider(
  #         names_from = !!sym(col),
  #         values_from = !!sym(value)
  #       )
  #   }
  # ) %>%
  #   purrr::reduce(
  #     dplyr::inner_join,
  #     by="skim_variable"
  #   )

  result_mean <- dplyr::inner_join(total_mean, grouped_mean, by = "skim_variable") %>%
    dplyr::rename(item = skim_variable)

  result_sd <- dplyr::inner_join(total_sd, grouped_sd, by = "skim_variable") %>%
    dplyr::rename(item = skim_variable)


  # Zip
  if (("mean" %in% values) && ("sd" %in% values)) {
    result_mean <- mutate(result_mean, across(where(is.numeric), ~ format(round(., digits), nsmall = digits)))
    result_sd <- mutate(result_sd, across(where(is.numeric), ~ format(round(., digits), nsmall = digits)))
    result <- zip_tables(result_mean, result_sd, brackets = T)
  } else if ("sd" %in% values) {
    result <- result_sd
  } else {
    result <- result_mean
  }

  # Add labels
  if (.labels) {
    result <- replace_item_values(result, data, cols)
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item)
  if (prefix != "") {
    result <- dplyr::mutate(result, item = stringr::str_remove(item, prefix))
    result <- dplyr::mutate(result, item = ifelse(item == "", prefix, item))
  }

  # Rename first column
  if (prefix != "") {
    # colnames(result)[1] <-  sub("[ :,]+$", "",  prefix)
    colnames(result)[1] <- trim_label(prefix)
  } else {
    result <- rename(result, Item = item)
  }


  .to_vlk_tab(result, digits= digits)
}


#' Output a correlation table
#'
#' @param data A tibble
#' @param cols1 The source items
#' @param cols2 The target items or NULL to calculate correlations within the source items
#' @param method The output metrics, p = Pearson's R, s = Spearman's rho
#' @param significant Only show significant values
#' @param digits The digits to print
#' @export
tab_metrics_items_cor <- function(data, cols1, cols2, method = "p", significant = F, digits = 2) {
  # Check parameters
  check_is_dataframe(data)

  # # Get positions of cols
  # cols1 <- tidyselect::eval_select(
  #   expr = enquo(cols1),
  #   data = data[unique(names(data))],
  #   allow_rename = FALSE,
  #   error_call = rlang::error_call
  # )

  # Same or different items?
  # if (missing(cols2)) {
  #   cols2 <- cols1
  # }

  # else {
  #
  #   cols2 <- tidyselect::eval_select(
  #     expr = enquo(cols2),
  #     data = data[unique(names(data))],
  #     allow_rename = FALSE,
  #     error_call = rlang::error_call
  #   )
  # }


  cols1 <- tidyselect::eval_select(expr = enquo(cols1), data = data)

  if (missing(cols2)) {
    cols2 <- cols1
  } else {
    cols2 <- tidyselect::eval_select(expr = enquo(cols2), data = data)
  }

  result <- expand.grid(x = cols1, y = cols2, stringsAsFactors = FALSE) %>%
    dplyr::mutate(x_name = names(x), y_name = names(y)) %>%
    dplyr::mutate(
      test = purrr::map2(
        .$x, .$y,
        function(x, y) cor.test(data[[x]], data[[y]], method = method)
      ),
      p = map(test, function(x) x$p.value),
      value = map(test, function(x) as.numeric(x$estimate)),
      stars = get_stars(p)
    ) %>%
    dplyr::select(item = x_name, target = y_name, value, p, stars)


  # Add labels
  # codes <- data %>%
  #   dplyr::select(!!cols) %>%
  #   get_labels() %>%
  #   dplyr::distinct(item, label)
  #
  # if (nrow(codes) > 0) {
  #   result <- result %>%
  #     dplyr::left_join(codes, by=c("item")) %>%
  #     dplyr::mutate(item = dplyr::coalesce(label, item)) %>%
  #     dplyr::select(-label)
  # }


  # Remove common item prefix
  prefix <- get_prefix(result$item)
  if (prefix != "") {
    result <- dplyr::mutate(result, item = stringr::str_remove(item, prefix))
    result <- dplyr::mutate(result, item = ifelse(item == "", prefix, item))
  }

  prefix <- get_prefix(result$target)
  if (prefix != "") {
    result <- dplyr::mutate(result, target = stringr::str_remove(target, prefix))
    result <- dplyr::mutate(result, target = ifelse(target == "", prefix, target))
  }

  # Create table
  result <- result %>%
    mutate(value = paste0(round(unlist(value), digits), stars)) %>%
    mutate(value = ifelse(significant & (p >= 0.1), "", value)) %>%
    select(item, target, value)

  result <- result %>%
    tidyr::pivot_wider(names_from = "target", values_from = "value") %>%
    rename(Item = item)

  .to_vlk_tab(result, digits= digits)
}

#' Add vlkr_tbl class
#'
#' Additionally, removes the skim_df class if present
#'
#' @param data A tibble
#' @param digits Set the plot digits. If NULL (default), no digits are set.
#' @retur A tibble of class vlkr_tbl
.to_vlk_tab <- function(data, digits=NULL) {

  if (!is.null(digits)) {
    attr(data, "digits") <- digits
  }

  class(data) <- c("vlkr_tbl", setdiff(class(data), "skim_df"))
  data
}

#' Knit volker tables
#'
#' @param df Data frame
#' @return Formatted table
knit_table <- function(df, ...) {
  # TODO: Embed "digits" in the vlkr_options list
  digits <- attr(df, "digits", exact = T)

  if (is.null(digits)) {
    digits <- getOption("digits")
  }


  if (knitr::is_html_output()) {
    # Replace \n by <br>
    df <- df %>%
      dplyr::mutate(across(dplyr::where(is.character), ~ gsub("\n", "<br>", .))) %>%
      knitr::kable(
        "html",
        escape = F,
        align = c("l", rep("r", ncol(df) - 1)),
        digits = digits,
        ...
      ) %>%
      kableExtra::kable_styling()
  } else if (knitr::is_latex_output()) {
    df <- df %>%
      dplyr::mutate_all(kableExtra::linebreak) %>%
      knitr::kable(
        "latex",
        booktabs = T,
        escape = F,
        align = c("l", rep("r", ncol(df) - 1)),
        digits = digits,
        ...
      )
  } else {
    df <- df %>%
      knitr::kable("pipe", align = c("l", rep("r", ncol(df) - 1)), digits = digits, ...)
  }

  df
}


#' Printing method for volker tables.
#'
#' @param obj The volker table
#' @export
print.vlkr_tbl <- function(obj) {
  obj <- knit_table(obj)

  if (knitr::is_html_output()) {
    obj <- knitr::asis_output(obj)
    knitr::knit_print(obj)
  } else {
    print(obj)
  }
}

