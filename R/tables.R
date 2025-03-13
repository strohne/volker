#' Output a frequency table
#'
#' @description
#' The type of frequency table depends on the number of selected columns:
#' - One categorical column: see \link{tab_counts_one}
#' - Multiple categorical columns: see \link{tab_counts_items}
#'
#' Cross tabulations:
#'
#' - One categorical column and one grouping column: see \link{tab_counts_one_grouped}
#' - Multiple categorical columns and one grouping column: see \link{tab_counts_items_grouped}
#' - Multiple categorical columns and multiple grouping columns: see \link{tab_counts_items_grouped_items} (not yet implemented)
#'
#' By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - One categorical column and one metric column: see \link{tab_counts_one_cor}
#' - Multiple categorical columns and one metric column: see \link{tab_counts_items_cor}
#' - Multiple categorical columns and multiple metric columns: \link{tab_counts_items_cor_items} (not yet implemented)
#'
#' Parameters that may be passed to specific count functions:
#' - **ci**: Add confidence intervals to proportions.
#' - **percent**: Frequency tables show percentages by default. Set to FALSE to get raw proportions.
#' - **prop**: For cross tables you can choose between total, row or column percentages.
#' - **values**: The values to output: n (frequency) or p (percentage) or both (the default).
#' - **category**: When you have multiple categories in a column, you can focus one of the categories to simplify the plots.
#'                 By default, if a column has only TRUE and FALSE values, the outputs focus the TRUE category.
#' - **labels**: Labels are extracted from the column attributes.
#'              Set to FALSE to output bare column names and values.
#'
#'
#' `r lifecycle::badge("experimental")`
#'
#'
#' @param data A data frame.
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param cross Optional, a grouping column. The column name without quotes.
#' @param metric When crossing variables, the cross column parameter can contain categorical or metric values.
#'            By default, the cross column selection is treated as categorical data.
#'            Set metric to TRUE, to treat it as metric and calculate correlations.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Other parameters passed to the appropriate table function.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_counts(data, sd_gender)
#'
#' @export
tab_counts <- function(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...) {
  # Check
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data, clean)
  }

  # Find columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)
  is_items <- length(cols_eval) > 1
  is_grouped <- length(cross_eval)== 1
  is_multi <- length(cross_eval) > 1
  is_metric <- metric != FALSE

  # Single variables
  if (!is_items && !is_grouped && !is_multi) {
    tab_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    tab_counts_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }
  else if (!is_items && is_grouped && is_metric) {
    tab_counts_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !is_grouped && !is_multi) {
    tab_counts_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_metric ) {
    tab_counts_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && is_grouped && is_metric) {
    tab_counts_items_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not supported by volker functions.")
  }

}

#' Output a table with distribution parameters
#'
#' @description
#' The table type depends on the number of selected columns:
#' - One metric column: see \link{tab_metrics_one}
#' - Multiple metric columns: see \link{tab_metrics_items}
#'
#' Group comparisons:
#'
#' - One metric column and one grouping column: see \link{tab_metrics_one_grouped}
#' - Multiple metric columns and one grouping column: see \link{tab_metrics_items_grouped}
#' - Multiple metric columns and multiple grouping columns: see \link{tab_metrics_items_grouped_items} (not yet implemented)
#'
#' By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - Two metric columns: see \link{tab_metrics_one_cor}
#' - Multiple metric columns and one metric column: see \link{tab_metrics_items_cor}
#' - Two metric column selections: see \link{tab_metrics_items_cor_items}
#'
#' Parameters that may be passed to specific metric functions:
#' - **ci**: Add confidence intervals for means or correlation coefficients.
#' - **values**: The output metrics, mean (m), the standard deviation (sd) or both (the default).
#' - **digits**: Tables containing means and standard deviations by default round values to one digit.
#'               Increase the number to show more digits
#' - **method**: By default, correlations are calculated using Pearson’s R.
#'               You can choose Spearman’s Rho with the methods-parameter.
#' - **labels**: Labels are extracted from the column attributes.
#'               Set to FALSE to output bare column names and values.
#'
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A data frame.
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param cross Optional, a grouping column (without quotes).
#' @param metric When crossing variables, the cross column parameter can contain categorical or metric values.
#'            By default, the cross column selection is treated as categorical data.
#'            Set metric to TRUE, to treat it as metric and calculate correlations.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Other parameters passed to the appropriate table function.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics(data, sd_age)
#'
#' @export
tab_metrics <- function(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...) {
  # Check
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data, clean)
  }

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)

  is_items <- length(cols_eval) > 1
  is_grouped <- length(cross_eval) == 1
  is_multi <- length(cross_eval) > 1
  is_metric <- metric != FALSE

  # Single variables
  if (!is_items && !is_grouped && !is_multi) {
    tab_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    tab_metrics_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }

  else if (!is_items && is_grouped && is_metric) {
    tab_metrics_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !is_grouped  && !is_multi) {
    tab_metrics_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_metric) {
    tab_metrics_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && is_grouped && is_metric) {
    tab_metrics_items_cor(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && !is_grouped && is_multi && is_metric) {
    tab_metrics_items_cor_items(data, {{ cols }}, {{ cross }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not supported by volker functions.")
  }

}

#' Output a frequency table for the values in one column
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding values to count.
#' @param ci Whether to compute 95% confidence intervals using \code{stats::\link[stats:prop.test]{prop.test}}.
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_counts_one(data, sd_gender)
#'
#' @importFrom rlang .data
#' @export
tab_counts_one <- function(data, col, ci = FALSE, percent = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, cols.categorical = {{ col }}, clean = clean)

  # 2. Count
  result <- data %>%
    dplyr::count({{ col }}) %>%
    tidyr::drop_na() %>%
    dplyr::mutate("{{ col }}" := as.character({{ col }})) %>%
    dplyr::mutate(p = .data$n / sum(.data$n))

  # 3. Confidence intervals
  # TODO: option to select from prop.test or binom.test
  if (ci) {
    n_total <- sum(result$n)
    result <- result |>
      dplyr::rowwise() |>
      dplyr::mutate(.test = list(stats::prop.test(.data$n, n_total))) |>
      dplyr::mutate(
        "ci low" = .data$.test$conf.int[1],
        "ci high" = .data$.test$conf.int[2]
      ) |>
      dplyr::select(-tidyselect::all_of(c(".test")))
  }

  # Get variable caption from the attributes
  if (labels) {
    result <- labs_replace(
      result, {{ col }},
      codebook(data, {{ col }}))

    label <- get_title(data, {{ col }})

    result <- dplyr::rename(result, {{ label }} := {{ col }})
  }

  if (percent) {
    result <- dplyr::mutate(
      result,
      dplyr::across(
        tidyselect::any_of(c("p","ci low","ci high")),
        ~ paste0(round(. * 100, 0), "%")
      )
    )
  }

  # totals
  result_total <- tibble::tibble(
    "total",
    sum(!is.na(dplyr::select(data, {{ col }}))),
    ifelse(percent, "100%", 1)
  )
  colnames(result_total) <- colnames(result)

  result <- dplyr::bind_rows(result, result_total)


  # Clean NA
  result <- dplyr::mutate(result, dplyr::across(tidyselect::any_of(c("ci low","ci high")), ~ ifelse(is.na(.) && percent,"",.)))

  digits <- ifelse(percent, 0, 2)

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits=digits)
}

#' Output frequencies cross tabulated with a grouping column
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param cross The column holding groups to split.
#' @param prop The basis of percent calculation: "total" (the default), "cols", or "rows".
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param values The values to output: n (frequency) or p (percentage) or both (the default).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_counts_one_grouped(data, adopter, sd_gender)
#'
#' @importFrom rlang .data
#' @export
tab_counts_one_grouped <- function(data, col, cross, prop = "total", percent = TRUE, values = c("n", "p"), labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, cols.categorical = c({{ col }}, {{ cross }}), clean = clean)

  check_is_param(prop, c("total", "cols", "rows"))
  check_is_param(values, c("n", "p"), allowmultiple = TRUE)

  # 2. Get labels for values
  if (labels) {
    data <- labs_replace(
      data, {{ cross }},
      codebook(data, {{ cross }}),
      "value_name", "value_label"
    )

    data <- labs_replace(
      data, {{ col }},
      codebook(data, {{ col }}),
      "value_name", "value_label"
    )
  }

  # 3. Count
  grouped <- data %>%
    dplyr::count({{ col }}, {{ cross }}) %>%
    dplyr::mutate(
      "{{ cross }}" := tidyr::replace_na(as.character({{ cross }}), "missing"),
      "{{ col }}" := tidyr::replace_na(as.character({{ col }}), "missing")
    )

  #
  # 4. N
  #
  rows_n <- grouped %>%
    dplyr::select({{ col }}, {{ cross }}, "n") %>%
    tidyr::pivot_wider(
      names_from = {{ cross }},
      values_from = "n",
      values_fill = list(n = 0)
    )

  # total column
  total_col_n <- data %>%
    dplyr::count({{ col }}) %>%
    dplyr::mutate(
      "{{ col }}" := tidyr::replace_na(as.character({{ col }}), "missing")
    ) %>%
    dplyr::select({{ col }}, total = "n")

  # total row
  total_row_n <- grouped %>%
    dplyr::group_by({{ cross }}) %>%
    dplyr::summarise(n = sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("{{ col }}" := "total") %>%
    tidyr::pivot_wider(
      names_from = {{ cross }},
      values_from = "n",
      values_fill = list(n = 0)
    )

  # total
  total_n <- data %>%
    dplyr::count() %>%
    dplyr::mutate("{{ col }}" := "total") %>%
    dplyr::select({{ col }}, total = "n")

  # Join
  result_n <-
    dplyr::full_join(
      total_col_n,
      rows_n,
      by = as.character(rlang::get_expr(rlang::enquo(col)))
    ) %>%
    dplyr::bind_rows(
      dplyr::left_join(
        total_n,
        total_row_n,
        by = as.character(rlang::get_expr(rlang::enquo(col)))
      )
    )

  #
  # 5. P
  #
  if (prop == "cols") {
    rows_p <- grouped %>%
      dplyr::group_by({{ cross }}) %>%
      dplyr::mutate(p = .data$n / sum(.data$n)) %>%
      dplyr::ungroup()

    total_col_p <- total_col_n %>%
      dplyr::mutate(total = .data$total / sum(.data$total))

    total_row_p <- total_row_n %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~1))
  } else if (prop == "rows") {
    rows_p <- grouped %>%
      dplyr::group_by({{ col }}) %>%
      dplyr::mutate(p = .data$n / sum(.data$n)) %>%
      dplyr::ungroup()

    total_col_p <- total_col_n %>%
      dplyr::mutate(total = 1)

    total_row_p <- total_row_n %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ .x / total_n$total))
  } else {
    rows_p <- grouped %>%
      dplyr::mutate(p = .data$n / sum(.data$n))

    total_col_p <- total_col_n %>%
      dplyr::mutate(total = .data$total / sum(.data$total))

    total_row_p <- total_row_n %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ .x / total_n$total))
  }

  rows_p <- rows_p %>%
    dplyr::select({{ col }}, {{ cross }}, "p") %>%
    tidyr::pivot_wider(
      names_from = {{ cross }},
      values_from = "p",
      values_fill = list(p = 0)
    )

  total_p <- tibble::tibble("total" = 1) %>%
    dplyr::mutate("{{col }}" := "total")

  # Join
  result_p <-
    dplyr::full_join(
      total_col_p,
      rows_p,
      by = as.character(rlang::get_expr(rlang::enquo(col)))
    ) %>%
    dplyr::bind_rows(
      dplyr::left_join(
        total_p,
        total_row_p,
        by = as.character(rlang::get_expr(rlang::enquo(col)))
      )
    )

  # Round and add % sign
  if (percent) {
    result_p <- result_p %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ paste0(round(. * 100, 0), "%")))
  }


  # 6. Zip
  if (("n" %in% values) && ("p" %in% values)) {
    result <- zip_tables(result_p, result_n, brackets = TRUE)
  } else if ("p" %in% values) {
    result <- result_p
  } else {
    result <- result_n
  }

  # 7. Get item label from the attributes
  if (labels) {
      title <- get_title(data, {{ col }})
      result <- dplyr::rename(result, {{ title }} := {{ col }})
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits=0)
}


#' Count values by a metric column that will be split into groups
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param cross The metric column that will be split into groups at the median.
#' @param prop The basis of percent calculation: "total" (the default), "cols", or "rows".
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param values The values to output: n (frequency) or p (percentage) or both (the default).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_counts_one_cor(data, adopter, sd_age)
#'
#' @importFrom rlang .data
#' @export
tab_counts_one_cor <- function(data, col, cross, prop = "total", percent = TRUE, values = c("n", "p"), labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, cols.categorical = {{ col }}, cols.numeric = {{ cross }}, clean = clean)

  check_is_param(prop, c("total", "cols", "rows"))
  check_is_param(values, c("n", "p"), allowmultiple = TRUE)

  # 2. Split into groups
  data <- .tab_split(data, {{ cross }}, labels = labels)

  # 3. Output
  result <- tab_counts_one_grouped(
    data, {{ col }}, {{ cross }},
    prop = prop, percent = percent, values = values, labels = labels, clean = clean,
    ...
  )

  result <- .attr_transfer(result, data, "missings")
  result <- .attr_transfer(result, data[[rlang::as_string(rlang::ensym(cross))]], "split")

  result

}


#' Output frequencies for multiple variables
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param ci Whether to compute 95% confidence intervals.
#' @param percent Set to FALSE to prevent calculating percents from proportions.
#' @param values The values to output: n (frequency) or p (percentage) or both (the default).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_counts_items(data, starts_with("cg_adoption_"))
#'
#' @export
#' @importFrom rlang .data
tab_counts_items <- function(data, cols, ci = FALSE, percent = TRUE, values = c("n", "p"), labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, cols.categorical = {{ cols }}, clean = clean)

  check_is_param(values, c("n", "p"), allowmultiple = TRUE)

  # 2. Calculate n and p
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cols_names <- colnames(dplyr::select(data, tidyselect::all_of(cols_eval)))

  result <- data %>%
    labs_clear({{ cols }}) %>%
    tidyr::pivot_longer(
      {{ cols }},
      names_to = "item",
      values_to = "value",
      values_drop_na = TRUE
    ) %>%
    dplyr::count(dplyr::across(tidyselect::all_of(c("item", "value")))) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of("item"))) %>%
    dplyr::mutate(p = .data$n / sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$value) |>
    dplyr::mutate(value = as.factor(.data$value)) %>%
    dplyr::mutate(item = factor(.data$item, levels=cols_names)) |>
    dplyr::arrange(.data$item)

  # Absolute frequency
  value <- "n"
  result_n <- result %>%
    dplyr::select("item", "value", !!sym(value)) %>%
    tidyr::pivot_wider(
      names_from = value,
      values_from = !!sym(value),
      values_fill = stats::setNames(list(0), value),
      names_expand = TRUE
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total = sum(dplyr::c_across(-1), na.rm=TRUE)) %>%
    dplyr::ungroup()

  # Relative frequency
  value <- "p"
  result_p <- result %>%
    dplyr::select("item", "value", !!sym(value)) %>%
    tidyr::pivot_wider(
      names_from = value,
      values_from = !!sym(value),
      values_fill = stats::setNames(list(0), value),
      names_expand = TRUE
    ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(total = sum(dplyr::c_across(-1), na.rm=TRUE)) %>%
    dplyr::ungroup()


  # Add % sign
  if (percent) {
    result_p <- dplyr::mutate(result_p, dplyr::across(tidyselect::where(is.numeric), ~ paste0(round(. * 100, 0), "%")))
  }


  # Confidence intervals
  if (ci) {
    result_ci <- result |>
      dplyr::group_by(dplyr::across(tidyselect::all_of("item"))) %>%
      dplyr::mutate(.test = purrr::map(.data$n, function(x) stats::prop.test(x, sum(.data$n)))) |>
      dplyr::mutate(
        "ci low" =purrr::map_dbl(.data$.test, function(x) x$conf.int[1]),
        "ci high" =purrr::map_dbl(.data$.test, function(x) x$conf.int[2]),
      ) |>
      dplyr::select(-tidyselect::all_of(c(".test"))) |>
      dplyr::ungroup()

    # Add % sign
    if (percent) {
      result_ci <- dplyr::mutate(result_ci, dplyr::across(tidyselect::all_of(c("p","ci low","ci high")), ~ paste0(round(. * 100, 0), "%")))
    }
  }

  # Combine n and p if requested
  if (("n" %in% values) && ("p" %in% values)) {
    result <- zip_tables(result_p, result_n, brackets = TRUE, newline = FALSE)
  } else if ("p" %in% values) {
    result <- result_p
  } else {
    result <- result_n
  }

  # Replace item labels
  # TODO: use codebook to determine the column order
  if (labels) {
    result <- labs_replace(
      result, "item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
    )

    if (ci) {
      result_ci <- labs_replace(
        result_ci, "item",
        codebook(data, {{ cols }}),
        "item_name", "item_label"
      )
    }
  }

  # Remove common item prefix
  # TODO: make dry
  prefix <- get_prefix(result$item, trim = TRUE)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))

  # Rename first columns
  if (prefix == "") {
    prefix <- "Item"
  }

  colnames(result)[1] <- prefix

  if (ci) {
    result_ci <- dplyr::mutate(result_ci, item = trim_prefix(.data$item, prefix))
    colnames(result_ci)[1] <- prefix
  }

  # Replace category labels
  if (labels) {
    labels_categories <- data %>%
      codebook({{ cols }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label")))) %>%
      stats::na.omit()

    if (nrow(labels_categories) > 0) {
      colnames(result) <- sapply(
        colnames(result),
        function(x) {
          dplyr::coalesce(
            stats::setNames(labels_categories$value_label, labels_categories$value_name)[x],
            x
          )
        }
      )
    }
  }

  if (ci) {
    result <- list(
      count = .to_vlkr_tab(result),
      ci = .to_vlkr_tab(result_ci, digits=2)
    )
    result <- .attr_transfer(result, data, "missings")
    result <- .to_vlkr_list(result)
  } else {
    result <- .attr_transfer(result, data, "missings")
    result <- .to_vlkr_tab(result, digits= 0)
  }

  return(result)
}


#' Compare the values in multiple items by a grouping column
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The column holding groups to compare.
#' @param category Summarizing multiple items (the cols parameter) by group requires a focus category.
#'                By default, for logical column types, only TRUE values are counted.
#'                For other column types, the first category is counted.
#'                Accepts both character and numeric vectors to override default counting behavior.
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param values The values to output: n (frequency) or p (percentage) or both (the default).
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#' tab_counts_items_grouped(
#'   data, starts_with("cg_adoption_"), adopter,
#'   category=c("agree", "strongly agree")
#' )
#'
#' @export
#' @importFrom rlang .data
tab_counts_items_grouped <- function(data, cols, cross, category = NULL, percent = TRUE, values = c("n", "p"), title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, cols.categorical = c({{ cols}}, {{ cross}}), clean = clean)

  check_is_param(values, c("n", "p"), allowmultiple = TRUE)

  # 2. Evaluate columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cols_names <- colnames(dplyr::select(data, tidyselect::all_of(cols_eval)))

  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)
  cross_names <- colnames(dplyr::select(data, tidyselect::all_of(cross_eval)))

  # 3. Pivot
  pivoted <- data %>%
    labs_clear({{ cols }}) %>%
    tidyr::pivot_longer(
      {{ cols }},
      names_to = "item",
      values_to = ".value_name"
    ) |>

    dplyr::mutate(item = factor(.data$item, levels=cols_names)) |>
    dplyr::arrange(.data$item)

  # Get item labels
  if (labels) {
    pivoted <- labs_replace(
      pivoted, "item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
    )
  }


  # Add item value labels
  codebook_df <- codebook(data, {{ cols }})

  pivoted <- pivoted %>%
    dplyr::mutate(
      .value_label = ifelse(
        .data$.value_name %in% codebook_df$value_name,
        codebook_df$value_label[match(.data$.value_name, codebook_df$value_name)],
        as.character(.data$.value_name)
      )
    )

  #  Set cross variable value labels
  if (labels) {
    pivoted <- labs_replace(
      pivoted, {{ cross }},
      codebook(data, {{ cross }}),
      "value_name", "value_label"
    )
  }

  # Focus TRUE category or the first category
  if (is.null(category)) {
    value_names <- sort(unique(as.character(pivoted$.value_name)))
    if ((length(value_names) == 2) && ("TRUE" %in% value_names)) {
      base_category <- "TRUE"
    } else {
      base_category <- value_names[1]
    }
  } else {
    base_category <- as.character(category)

    if (
      !all(
        (base_category %in% as.character(pivoted$.value_name)) |
        (base_category %in% pivoted$.value_label)
      )
    ) {
      stop("One or more specified categories do not exist in the data.")
    }
  }

  # Get category labels if names are provided (e.g. for numeric values)
  base_labels <- base_category
  if (is.null(category) || all(base_labels %in% pivoted$.value_name)) {
    base_labels <- get_labels(codebook_df, base_category)
  }

  # Recode
  pivoted <- pivoted %>%
    mutate(.category = (.data$.value_name %in% base_category) | (.data$.value_label %in% base_category)) |>
    dplyr::mutate(.cross = {{ cross }})

  #
  # Count
  #

  grouped <- pivoted %>%

    # Count
    dplyr::count(dplyr::across(tidyselect::all_of(c("item", ".cross", ".category")))) %>%
    tidyr::complete(.data$item, .data$.cross, .data$.category, fill=list(n=0)) |>

    # Percent
    dplyr::group_by(dplyr::across(tidyselect::all_of(c("item",".cross"))))%>%
    dplyr::mutate(p = (.data$n / sum(.data$n))) %>%
    dplyr::mutate(p = ifelse(is.na(.data$p), 0, .data$p)) %>%
    dplyr::ungroup() |>


    # Filter category
    dplyr::filter(.data$.category == TRUE) %>%
    dplyr::select(-tidyselect::any_of(".category"))

  totals <- pivoted %>%

    # Count
    dplyr::count(dplyr::across(tidyselect::all_of(c("item", ".category")))) %>%
    tidyr::complete(.data$item, .data$.category, fill=list(n=0)) |>

    # Percent
    dplyr::group_by(dplyr::across(tidyselect::all_of(c("item"))))%>%
    dplyr::mutate(p = (.data$n / sum(.data$n))) %>%
    dplyr::mutate(p = ifelse(is.na(.data$p), 0, .data$p)) %>%
    dplyr::ungroup() |>


    # Filter category
    dplyr::filter(.data$.category == TRUE) %>%
    dplyr::select(-tidyselect::any_of(".category"))


  #
  # N
  #

  rows_n <- grouped %>%
    dplyr::select(tidyselect::all_of(c("item", ".cross", "n"))) %>%
    tidyr::pivot_wider(
      names_from = ".cross",
      values_from = "n",
      values_fill = list(n = 0)
    )

  totals_n <- totals |>
    dplyr::select(tidyselect::all_of(c("item", "n"))) %>%
    dplyr::rename(total = "n")

  result_n <- dplyr::left_join(totals_n, rows_n, by = "item")

  #
  # P
  #

  rows_p <- grouped %>%
    dplyr::select(tidyselect::all_of(c("item", ".cross", "p"))) %>%
    tidyr::pivot_wider(
      names_from = ".cross",
      values_from = "p",
      values_fill = list(p = 0)
    )

  totals_p <- totals |>
    dplyr::select(tidyselect::all_of(c("item", "p"))) %>%
    dplyr::rename(total = "p")

  result_p <- dplyr::left_join(totals_p, rows_p, by = "item")


  # Add % sign
  if (percent) {
    result_p <- dplyr::mutate(result_p, dplyr::across(tidyselect::where(is.numeric), ~ paste0(round(. * 100, 0), "%")))
  }

  #
  # Combine n and p if requested
  #

  if (("n" %in% values) && ("p" %in% values)) {
    result <- zip_tables(result_p, result_n, brackets = TRUE, newline = FALSE)
  } else if ("p" %in% values) {
    result <- result_p
  } else {
    result <- result_n
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item, trim = TRUE)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))

  # Rename first columns
  colnames(result)[1] <- ifelse(prefix == "", "Item", prefix)

  # Add baseline
  attr(result, "focus") <- base_labels
  result <- .attr_transfer(result, data, "missings")

  .to_vlkr_tab(result)

}


#' Correlation of categorical items with categorical items
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross Tidyselect item variables (e.g. starts_with...).
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
tab_counts_items_grouped_items <- function(data, cols, cross, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Compare the values in multiple items by a metric column that will be split into groups
#'
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross A metric column that will be split into groups at the median value.
#' @param category Summarizing multiple items (the cols parameter) by group requires a focus category.
#'                By default, for logical column types, only TRUE values are counted.
#'                For other column types, the first category is counted.
#'                Accepts both character and numeric vectors to override default counting behavior.
#' @param split Not implemented yet.
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param values The values to output: n (frequency) or p (percentage) or both (the default).
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#' tab_counts_items_cor(
#'   data, starts_with("cg_adoption_"), sd_age,
#'   category=c("agree", "strongly agree")
#' )
#'
#' @export
#' @importFrom rlang .data
tab_counts_items_cor <- function(data, cols, cross, category = NULL, split = NULL, percent = TRUE, values = c("n", "p"), title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, cols.categorical = {{ cols }}, cols.numeric = {{ cross }}, clean = clean)

  check_is_param(values, c("n", "p"), allowmultiple = TRUE)

  # 2. Split into groups
  data <- .tab_split(data, {{ cross }}, labels = labels)

  # 3. Output
  result <- tab_counts_items_grouped(data, {{ cols }}, {{ cross }}, category = category, percent = percent, values = values, title = title, labels = labels, clean = clean, ...)

  result <- .attr_transfer(result, data, "missings")
  result <- .attr_transfer(result, data[[rlang::as_string(rlang::ensym(cross))]], "split")

  result
}


#' Correlation of categorical items with metric items
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross Tidyselect item variables (e.g. starts_with...).
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
tab_counts_items_cor_items <- function(data, cols, cross,  title = TRUE, labels = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}


#' Output a five point summary table for the values in multiple columns
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The columns holding metric values.
#' @param ci Whether to calculate 95% confidence intervals of the mean.
#' @param digits The number of digits to print.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_one(data, sd_age)
#'
#' @export
#' @importFrom rlang .data
tab_metrics_one <- function(data, col, ci = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, cols.numeric = {{ col }}, clean = clean)

  # 2. Calculate values
  result <- data %>%
    skim_metrics({{ col }}) %>%
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))

  if (ci) {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd", "ci low" = "ci.low","ci high" = "ci.high","n", "items","alpha"))
    )
  } else {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","n","items","alpha"))
    )
  }

  # |>
  #   dplyr::select(
  #     "item" = "skim_variable",
  #     min = "numeric.min",
  #     q1 = "numeric.q1",
  #     median = "numeric.median",
  #     q3 = "numeric.q3",
  #     max = "numeric.max",
  #     m = "numeric.mean",
  #     sd = "numeric.sd",
  #     "missing",
  #     "n",
  #     items = "numeric.items",
  #     alpha = "numeric.alpha"
  #   )

  # 3. Remove items and alpha if not and index
  if (all(is.na(result$items)) || all(is.na(result$alpha))) {
    result$items <- NULL
    result$alpha <- NULL
  } else {
    result <- result %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of("items"), ~ as.character(round(., 0)))) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of("alpha"), ~ as.character(round(., 2))))
  }

  result <- result %>%
    # TODO: can we leave digits calculation to .to_vlkr_tab?
    #       So that the resulting data frame contains all digits?
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("n")), ~ as.character(round(., 0)))) %>%
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("min", "q1", "median", "q3", "max")), ~ as.character(round(., digits)))) %>%
    dplyr::mutate(dplyr::across(tidyselect::any_of(c("mean", "sd", "ci low", "ci high")), ~ as.character(round(., digits)))) %>%
    # labs_clear(-item) %>%
    tidyr::pivot_longer(-tidyselect::all_of("item")) %>%
    dplyr::select(-tidyselect::all_of("item"), {{ col }} := "name", "value")

  # 4. Get item label from the attributes
  if (labels) {
    label <- get_title(data, {{ col }})
    result <- dplyr::rename(result, {{ label }} := {{ col }})
  }
  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result)
}


#' Output a five point summary for groups
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param cross The column holding groups to compare.
#' @param ci Whether to output 95% confidence intervals.
#' @param digits The number of digits to print.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_one_grouped(data, sd_age, sd_gender)
#'
#' @export
#' @importFrom rlang .data
tab_metrics_one_grouped <- function(data, col, cross, ci = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, cols.categorical = {{ cross }}, cols.numeric = {{ col }}, clean = clean)

  # 2. Calculate values
  result_grouped <- data %>%
    dplyr::group_by({{ cross }}) %>%
    skim_metrics({{ col }}) %>%
    # dplyr::ungroup() %>%
    # dplyr::mutate(
    #   {{ cross }} := tidyr::replace_na(as.character({{ cross }}), "missing")
    # ) %>%
    dplyr::select(-tidyselect::all_of(c("skim_variable","skim_type"))) |>
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))

  if (labels) {
    result_grouped <- labs_replace(result_grouped,  {{ cross }}, codebook(data, {{ cross }}))
  }

  result_total <- data %>%
    skim_metrics({{ col }}) %>%
    dplyr::mutate({{ cross }} := "total") |>
    dplyr::select(-tidyselect::all_of(c("skim_variable","skim_type"))) |>
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))


  result <- dplyr::bind_rows(
    result_grouped,
    result_total
  )

  if (ci) {
    result <- dplyr::select(
      result,  {{ cross }},
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd", "ci low" = "ci.low", "ci high" = "ci.high","n","items","alpha"))
    )
  } else {
    result <- dplyr::select(
      result,{{ cross }},
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","n","items","alpha"))
    )
  }

  # Remove items and alpha if not an index
  if (all(is.na(result$items)) || all(is.na(result$alpha))) {
    result$items <- NULL
    result$alpha <- NULL
  } else {
    result <- result %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of("items"), ~ as.character(round(., 0)))) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of("alpha"), ~ as.character(round(., 2))))
  }

  # Get item label from the attributes
  if (labels) {

    # Cross variable's title
    codes <- data %>%
      codebook({{ cross }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("item_name", "item_label")))) %>%
      stats::na.omit()

    if (nrow(codes) > 0) {
      label <- codes$item_label[1]
      result <- result %>%
        dplyr::rename({{ label }} := {{ cross }})
    }


    # Metric scale labeling
    scale <- attr(dplyr::pull(data, {{ col }}), "scale")
    if (is.null(scale)) {
      scale <- data %>%
        codebook({{ col }}) %>%
        dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))
    }
    attr(result, "scale")
  }

  # TODO: Add limits
  # attr(data[[newcol]],"limits")
  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits= digits)
}

#' Correlate two columns
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The first column holding metric values.
#' @param cross The second column holding metric values.
#' @param method The output metrics, TRUE or pearson = Pearson's R, spearman = Spearman's rho
#' @param ci Whether to output confidence intervals.
#' @param digits The number of digits to print.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_one_cor(data, use_private, sd_age)
#'
#' @export
#' @importFrom rlang .data
tab_metrics_one_cor <- function(data, col, cross, method = "pearson", ci = FALSE, digits = 2, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, cols.numeric = c({{ col }}, {{ cross }}), clean = clean)

  check_is_param(method, c("spearman", "pearson"))

  # 2. Get columns
  cols_eval <- tidyselect::eval_select(expr = enquo(col), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)

  # 3. Calculate correlation
  result <- .effect_correlations(data, {{ col }}, {{ cross}}, method = method, labels = labels)

  if (method=="spearman") {
    values <- c("item1", "item2", "n", "Spearman's rho")
  } else {
    values <- c("item1", "item2", "n", "Pearson's r")
  }
  if (ci) {
    values <- c(values, "ci low", "ci high")
  }

  result <- dplyr::select(result, tidyselect::all_of(values))

  # 4. Remove common item prefix
  prefix <- get_prefix(c(result$item1, result$item2))
  result <- dplyr::mutate(result, item1 = trim_prefix(.data$item1, prefix))
  result <- dplyr::mutate(result, item2 = trim_prefix(.data$item2, prefix))

  prefix <- ifelse(prefix == "", "Item", prefix)
  if (prefix == "") {
    title <- NULL
  } else {
    title <- prefix
  }

  result <- result %>%
    dplyr::rename("Item 1" = tidyselect::all_of("item1")) |>
    dplyr::rename("Item 2" = tidyselect::all_of("item2"))

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits = digits, caption = title)
}

#' Output a five point summary table for multiple items
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The columns holding metric values.
#' @param ci Whether to compute confidence intervals of the mean.
#' @param digits The number of digits to print.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_items(data, starts_with("cg_adoption_"))
#'
#' @export
#' @importFrom rlang .data
tab_metrics_items <- function(data, cols, ci = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, cols.numeric = {{ cols }}, clean = clean)

  # 2. Calculate
  result <- data %>%
    dplyr::select({{ cols }}) |>
    skim_metrics() |>
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))

  if (ci) {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd", "ci low" = "ci.low", "ci high" = "ci.high","n","items","alpha"))
    )
  } else {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","n","items","alpha"))
    )
  }

  # 3. Remove items and alpha if not and index
  if (all(is.na(result$items)) || all(is.na(result$alpha))) {
    result$items <- NULL
    result$alpha <- NULL
  } else {
    result <- result %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of("items"), ~ as.character(round(., 0)))) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of("alpha"), ~ as.character(round(., 2))))
  }

  # 4. Get item labels from the attributes
  if (labels) {
    result <- labs_replace(
      result, "item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
    )
    attr(result, "limits") <- get_limits(data, {{ cols }})

    attr(result, "scale") <- codebook(data, {{ cols }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))
  }

  # 5. Remove common item prefix and title
  # TODO: remove common postfix
  prefix <- get_prefix(result$item, trim=TRUE)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))


  # 6. Rename first column
  if (prefix != "") {
    colnames(result)[1] <- prefix
  } else {
    result <- dplyr::rename(result, Item = tidyselect::all_of("item"))
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits= digits)
}

#' Output the means for groups in one or multiple columns
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The item columns that hold the values to summarize.
#' @param cross The column holding groups to compare.
#' @param digits The number of digits to print.
#' @param values The output metrics, mean (m), the standard deviation (sd) or both (the default).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_items_grouped(data, starts_with("cg_adoption_"), sd_gender)
#'
#' @export
#' @importFrom rlang .data
tab_metrics_items_grouped <- function(data, cols, cross, digits = 1, values = c("m", "sd"), labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }},  cols.categorical = {{ cross }}, cols.numeric = {{ cols }}, clean = clean)

  check_is_param(values, c("m", "sd"), allowmultiple = TRUE)

  # Means and SDs
  result_mean <- skim_grouped(data, {{ cols }}, {{ cross }}, "numeric.mean", labels)
  result_sd <- skim_grouped(data, {{ cols }}, {{ cross }}, "numeric.sd", labels)

  total_n <- data |>
    dplyr::count({{ cross }}) |>
    (\(.) dplyr::bind_rows(.,dplyr::summarise(., {{ cross }} := "total", n = sum(.$n))))()  |>
    tidyr::pivot_wider(names_from = {{ cross }}, values_from = "n") |>
    dplyr::mutate(item = "n")

  # Significance of lm
  # TODO
  # grouped_p <- purrr::map(
  #   col_group,
  #   function(col) {
  #     col <- names(data)[col]
  #
  #     data %>%
  #       dplyr::filter(!is.na(!!sym(col))) %>%
  #       dplyr::group_by(!!sym(col)) %>%
  #       dplyr::select(!!sym(col),{{ cols }}) %>%
  #       skim_metrics() %>%
  #       dplyr::ungroup() %>%
  #       dplyr::select("skim_variable", !!sym(col), !!sym(value)) %>%
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

  # 7. Zip
  if (("m" %in% values) && ("sd" %in% values)) {

    # TODO: What about the resulting data frame, should it really contain rounded values?
    #       Maybe let zipping and rounding to the print function and return a list of data frames instead
    result_mean <- dplyr::mutate(result_mean, dplyr::across(tidyselect::where(is.numeric), ~ format(round(., digits), nsmall = digits)))
    result_sd <- dplyr::mutate(result_sd, dplyr::across(tidyselect::where(is.numeric), ~ format(round(., digits), nsmall = digits)))
    total_n <- dplyr::mutate(total_n, dplyr::across(tidyselect::where(is.numeric), ~ as.character(.)))

    result <- zip_tables(result_mean, result_sd, brackets = TRUE)
  } else if ("sd" %in% values) {
    result <- result_sd
  } else {
    result <- result_mean
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item, trim=TRUE)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))

  # Add total row
  result <- dplyr::bind_rows(result, total_n)

  # Rename first column
  if (prefix != "") {
    colnames(result)[1] <- prefix
  } else {
    result <- dplyr::rename(result, Item = tidyselect::all_of("item"))
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits= digits)
}

#' Correlation of metric items with categorical items
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross Tidyselect item variables (e.g. starts_with...)
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A volker tibble.
#' @importFrom rlang .data
tab_metrics_items_grouped_items <- function(data, cols, cross, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Output a correlation table for item battery and one metric variable
#'
#' @description
#' `r lifecycle::badge("experimental")`

#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The source columns.
#' @param cross The target columns or NULL to calculate correlations within the source columns.
#' @param method The output metrics, pearson = Pearson's R, spearman = Spearman's rho.
#' @param digits The number of digits to print.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_items_cor(
#'   data,
#'   starts_with("cg_adoption_adv"),
#'   sd_age,
#'   metric = TRUE
#' )
#'
#' @importFrom rlang .data
#' @export
tab_metrics_items_cor <- function(data, cols, cross, method = "pearson", digits = 2, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, cols.numeric = c({{ cols }}, {{ cross}}), clean = clean)

  check_is_param(method, c("spearman", "pearson"))

  # 2. Calculate correlation
  result <- .effect_correlations(data, {{ cols }}, {{ cross}}, method = method, labels = labels)

  # Remove common item prefix
  prefix1 <- get_prefix(result$item1)
  prefix2 <- get_prefix(result$item2)
  result <- dplyr::mutate(result, item1 = trim_prefix(.data$item1, prefix1))
  if (prefix1 == prefix2) {
    result <- dplyr::mutate(result, item2 = trim_prefix(.data$item2, prefix2))
    title <- prefix1
  } else {

    if ((prefix1 != "") && (prefix2 != "")) {
      title <- paste0(prefix1, " - ", prefix2)
    } else {
      title = ""
    }
  }

  if(title == "")  {
    title <- NULL
  }

  # Create matrix
  method <- ifelse(method=="spearman", "Spearman's rho", "Pearson's r")
  result <- result %>%
    dplyr::select(tidyselect::all_of(c("item1", "item2", method))) |>
    tidyr::pivot_wider(names_from = "item2", values_from = !!sym(method))

  prefix1 <- ifelse(prefix1 == "", "Item", prefix1)

  result <- dplyr::rename(result, {{ prefix1 }} := "item1")

  result <- .attr_transfer(result, data, c("missings", "cases"))
  .to_vlkr_tab(result, digits = digits, caption = title)
}

#' Output a correlation table for item battery and item battery
#'
#' @description
#' `r lifecycle::badge("experimental")`

#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The source columns.
#' @param cross The target columns or NULL to calculate correlations within the source columns.
#' @param method The output metrics, pearson = Pearson's R, spearman = Spearman's rho.
#' @param digits The number of digits to print.
#' @param ci Whether to calculate 95% confidence intervals of the correlation coefficient.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_items_cor_items(
#'   data,
#'   starts_with("cg_adoption_adv"),
#'   starts_with("use"),
#'   metric = TRUE
#' )
#'
#' @importFrom rlang .data
#' @export
tab_metrics_items_cor_items <- function(data, cols, cross, method = "pearson", digits = 2, ci = FALSE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, cols.numeric = c({{ cols }}, {{ cross }}), clean = clean)

  check_is_param(method, c("spearman", "pearson"))

  # 2. Calculate correlations
  result <- .effect_correlations(data, {{ cols }}, {{ cross }}, method = method, labels = labels)

  result_cols <- c("item1", "item2")

  if(method == "spearman") {
    result_cols <- c(result_cols, "Spearman's rho")
  }
  else if (method == "pearson") {
    result_cols <- c(result_cols, "Pearson's r")
    if (ci) {
      result_cols <- c(result_cols, "ci low", "ci high")
    }
  }


  result <- dplyr::select(result, tidyselect::all_of(result_cols))

  # 4. Labels
  prefix1 <- get_prefix(result$item1)
  prefix2 <- get_prefix(result$item2)

  result <- result %>%
    dplyr::mutate(item1 = trim_prefix(.data$item1, prefix1)) |>
    dplyr::mutate(item2 = trim_prefix(.data$item2, prefix2))

  if ((prefix1 == prefix2) && (prefix1 != "")) {
    prefix1 <- paste0("Item 1: ", prefix1)
    prefix2 <- paste0("Item 2: ", prefix2)
  }

  # Rename first column
  if (prefix1 != "") {
    colnames(result)[1] <- prefix1
  }

  # Rename second column
  if (prefix2 != "") {
    colnames(result)[2] <- prefix2
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits = 2)
}

#' Split a metric column into categories based on the median
#'
#' @keywords internal
#'
#' @param data A data frame containing the column to be split.
#' @param col The column to split.
#' @param labels Logical; if `TRUE` (default), use custom labels for the split categories based
#'               on the column title. If `FALSE`, use the column name directly.
#'
#' @return A data frame with the specified column converted into categorical labels based on its median value.
#'         The split threshold (median) is stored as an attribute of the column.
.tab_split <- function(data, col, labels = TRUE) {
  cross_name <- rlang::as_string(rlang::ensym(col))
  cross_label <- ifelse(labels, get_title(data, {{ col }}), cross_name)
  cross_median <- stats::median(data[[cross_name]], na.rm = TRUE)

  cross_levels <- as.list(paste0(c("Low ", "High "), cross_label))
  names(cross_levels) <- paste0(c("low: ", "high: "), cross_name)

  data <- data |>
    mutate("{{ col }}" := ifelse({{ col }} < cross_median, names(cross_levels)[1], names(cross_levels)[2])) |>
    labs_apply(cols = {{ col }}, values = cross_levels)

  attr(data[[ cross_name ]], "label")  <- cross_label
  attr(data[[ cross_name ]], "split") <- paste0(cross_label, " split at median ", round(cross_median, 1))

  data

}


#' Add vlkr_tbl class
#'
#' Additionally, removes the skim_df class if present.
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param digits Set the plot digits. If NULL (default), no digits are set.
#' @param caption The caption printed above the table.
#' @param baseline A base line printed below the table.
#' @return A volker tibble.
.to_vlkr_tab <- function(data, digits = NULL, caption = NULL, baseline = NULL) {

  if (!is.null(digits)) {
    attr(data, "digits") <- digits
  }

  if (!is.null(caption)) {
    attr(data, "caption") <- caption
  }

  if (!is.null(baseline)) {
    attr(data, "baseline") <- baseline
  }

  class(data) <- c("vlkr_tbl", setdiff(class(data), "skim_df"))
  data
}

#' Knit volker tables
#'
#' @keywords internal
#'
#' @param df Data frame.
#' @return Formatted  table produced by \link[knitr:kable]{kable}.
.knit_table <- function(df, ...) {
  options(knitr.kable.NA = '')

  digits <- attr(df, "digits", exact = TRUE)
  if (is.null(digits)) {
    digits <- getOption("vlkr.digits", VLKR_NORMAL_DIGITS)
  }

  baseline <- attr(df, "baseline", exact=TRUE)
  if (is.null(baseline)) {
    baseline <- get_baseline(df)
  } else if (baseline == FALSE) {
    baseline <- NULL
  }

  # Round
  if (".digits" %in% colnames(df)) {
    df <- df |>
      dplyr::rowwise() |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) round(x, .data$.digits))) |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) as.character(x)))

    df$.digits <- NULL
  }

  if (!is.null(digits) && digits > 1) {
    numberformat = list(nsmall = digits)
  } else {
    numberformat <- NULL
  }

  if (knitr::is_html_output()) {
    # Replace \n by <br>
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.character), .knit_prepare)) %>%
      knitr::kable(
        "html",
        escape = FALSE,
        align = c("l", rep("r", ncol(df) - 1)),
        digits = digits,
        #format.args = numberformat,
        ...
      ) %>%
      kableExtra::kable_styling(
        position = "left",
        full_width = FALSE
      )
  } else if (knitr::is_latex_output()) {
    df <- df %>%
      dplyr::mutate(dplyr::across(
        dplyr::where(is.character),
        ~ .knit_prepare(., wrap = dplyr::coalesce(getOption("vlkr.wrap.labels"), VLKR_PLOT_LABELWRAP))
      )) %>%
      knitr::kable(
        "latex",
        #booktabs = TRUE,
        escape = FALSE,
        #align = c("l", rep("r", ncol(df) - 1)),
        digits = digits,
        col.names = .knit_prepare(colnames(df), wrap = dplyr::coalesce(getOption("vlkr.wrap.legend"), VLKR_PLOT_LEGENDWRAP)),
        #format.args = numberformat,
        ...
      )

  } else if (!is.null(knitr::pandoc_to())) {
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.character), .knit_prepare)) %>%
      knitr::kable(
        align = c("l", rep("r", ncol(df) - 1)),
        digits = digits,
        ...
      )
  } else {
    df <- df %>%
      knitr::kable(
        align = c("l", rep("r", ncol(df) - 1)),
        digits = digits,
        ...
      )
  }

  if (!is.null(baseline)) {
    attr(df, "baseline") <- baseline
  }

  df
}


#' Prepare markdown content for table rendering
#'
#' @keywords internal
#'
#' @param x Markdown text.
#' @param wrap Wrap text after the given number of characters.
#' @return Markdown text with line breaks and escaped special characters.
.knit_prepare <- function(x, wrap = FALSE) {

  if (knitr::is_html_output()) {
    x <- gsub("\n", "<br>", x, fixed=TRUE)
  } else {
    x <- gsub("\n", " ", x, fixed=TRUE)
  }

  x <- gsub("*", "\\*", x, fixed=TRUE)

  if (knitr::is_latex_output()){
    x <- gsub("%", "\\%", x, fixed=TRUE)
    x <- gsub("&", "\\&", x, fixed=TRUE)
    x <- gsub("$", "\\$", x, fixed=TRUE)
    x <- gsub("_", "\\_", x, fixed=TRUE)

    if (is.numeric(wrap)) {
      x <- kableExtra::linebreak(wrap_label(x, wrap))
    }
  }
  x
}

#' Printing method for volker tables.
#'
#' @keywords internal
#'
#' @param x The volker table.
#' @param ... Further parameters passed to print().
#' @return No return value.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tb <- tab_metrics(data, sd_age)
#' print(tb)
#'
#' @export
print.vlkr_tbl <- function(x, ...) {
  x <- .knit_table(x)
  baseline <- attr(x, "baseline", exact=TRUE)

  if (!is.null(knitr::pandoc_to())) {

    if (!is.null(baseline)) {
      x <- paste0(x,"  \n  ", baseline)
    }

    x <- paste0(x, collapse= "\n")
    knitr::knit_print(knitr::asis_output(x))

  } else {
    caption <- attr(x, "caption", exact=TRUE)
    if (!is.null(caption)) {
      cat("\n",caption, sep="")
    }

    print(x, ...)

    if (!is.null(baseline)) {
      cat("\n", baseline, "\n\n", sep="")
    }

  }
}

