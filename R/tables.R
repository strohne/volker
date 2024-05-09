#' Output a frequency table
#'
#' @description
#' The type of frequency table depends on the number of selected columns:
#' - One column: see \link{tab_counts_one}
#' - Multiple columns: see \link{tab_counts_items}
#' - One column and one grouping column: see \link{tab_counts_one_grouped}
#' - Multiple columns and one grouping column: see \link{tab_counts_items_grouped}
#'
#' By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - One column and one metric column: see \link{tab_counts_one_cor} (not yet implemented)
#' - Multiple columns and one metric column: see \link{tab_counts_items_cor} (not yet implemented)
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
    data <- data_clean(data)
  }

  # Find columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)
  is_items <- length(cols_eval) > 1
  is_grouped <- length(cross_eval)== 1
  is_multi <- length(cross_eval) > 1
  is_metric <- metric != FALSE

  # Single variables
  if (!is_items && !(is_grouped || is_multi)) {
    tab_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    tab_counts_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !(is_grouped ||is_multi)) {
    tab_counts_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped &&  !is_metric) {
    tab_counts_items_grouped(data, {{ cols }}, {{ cross }},  ...)
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
#' - One column: see \link{tab_metrics_one}
#' - Multiple columns: see \link{tab_metrics_items}
#' - One column and one grouping column: see \link{tab_metrics_one_grouped}
#' - Multiple columns and one grouping column: see \link{tab_metrics_items_grouped}
#'
#' By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - Two metric columns: see \link{tab_metrics_one_cor}
#' - Multiple columns: see \link{tab_metrics_items_cor} (experimental)
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
    data <- data_clean(data)
  }

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)

  is_items <- length(cols_eval) > 1
  is_grouped <- length(cross_eval) == 1
  is_multi <- length(cross_eval) > 1
  is_metric <- metric != FALSE

  # Single variables
  if (!is_items && !(is_grouped ||is_multi)) {
    tab_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    tab_metrics_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }

  else if (!is_items && is_grouped && is_metric) {
    tab_metrics_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !(is_grouped || is_multi) && !is_metric) {
    tab_metrics_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_metric) {
    tab_metrics_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && (is_grouped || is_multi) && is_metric) {
    tab_metrics_items_cor(data, {{ cols }}, {{ cross }}, ...)
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
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, {{ col }})

  # 4. Count
  result <- data %>%
    dplyr::count({{ col }}) %>%
    tidyr::drop_na() %>%
    dplyr::mutate("{{ col }}" := as.character({{ col }})) %>%
    dplyr::mutate(p = .data$n / sum(.data$n))

  # 5. Confidence intervals
  # TODO: option to select from prop.test or binom.test
  if (ci) {
    n_total <- sum(result$n)
    result <- result |>
      dplyr::rowwise() |>
      dplyr::mutate(.test = list(stats::prop.test(.data$n, n_total))) |>
      dplyr::mutate(
        ci.low = .data$.test$conf.int[1],
        ci.high = .data$.test$conf.int[2]
      ) |>
      dplyr::select(-tidyselect::all_of(c(".test")))
  }

  # Get variable caption from the attributes
  if (labels) {
    result <- labs_replace(result, {{ col }}, codebook(data, {{ col }}))
    label <- get_title(data, {{ col }})
    result <- dplyr::rename(result, {{ label }} := {{ col }})

  }

  if (percent) {
    result <- dplyr::mutate(result, dplyr::across(tidyselect::any_of(c("p","ci.low","ci.high")), ~ paste0(round(. * 100, 0), "%")))
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
  result <- dplyr::mutate(result, dplyr::across(tidyselect::any_of(c("ci.low","ci.high")), ~ ifelse(is.na(.) && percent,"",.)))

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
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ col }}, {{ cross }}))

  #
  # 4. Count
  #
  grouped <- data %>%
    dplyr::count({{ col }}, {{ cross }}) %>%
    dplyr::mutate(
      "{{ cross }}" := tidyr::replace_na(as.character({{ cross }}), "missing"),
      "{{ col }}" := tidyr::replace_na(as.character({{ col }}), "missing")
    )

  #
  # 2. N
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
  # 3. P
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


  # Zip
  if (("n" %in% values) && ("p" %in% values)) {
    result <- zip_tables(result_p, result_n, brackets = TRUE)
  } else if ("p" %in% values) {
    result <- result_p
  } else {
    result <- result_n
  }

  # Get item label from the attributes
  if (labels) {
    codes <- data %>%
      codebook({{ col }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("item_name", "item_label")))) %>%
      stats::na.omit()

    if (nrow(codes) > 0) {
      label <- codes$item_label[1]
      result <- result %>%
        dplyr::rename({{ label }} := {{ col }})
    }
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits=0)
}


#' Correlate categorical groups with one metric column
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The item columns that hold the values to summarize.
#' @param cross The column to correlate.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
tab_counts_one_cor <- function(data, cols, cross, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
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
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, {{ cols }})

  # 4. Calculate n and p
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
      values_fill = stats::setNames(list(0), value)
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
      values_fill = stats::setNames(list(0), value)
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
        ci.low =purrr::map_dbl(.data$.test, function(x) x$conf.int[1]),
        ci.high =purrr::map_dbl(.data$.test, function(x) x$conf.int[2]),
      ) |>
      dplyr::select(-tidyselect::all_of(c(".test"))) |>
      dplyr::ungroup()

    # Add % sign
    if (percent) {
      result_ci <- dplyr::mutate(result_ci, dplyr::across(tidyselect::all_of(c("p","ci.low","ci.high")), ~ paste0(round(. * 100, 0), "%")))
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
  prefix <- get_prefix(result$item, trim=T)
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
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols The item columns that hold the values to summarize.
#' @param cross The column holding groups to compare.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
tab_counts_items_grouped <- function(data, cols, cross, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Correlate the values in multiple items
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols The source columns.
#' @param cross The target columns or NULL to calculate correlations within the source columns.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @importFrom rlang .data
tab_counts_items_cor <- function(data, cols, cross, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Output a five point summary table for the values in multiple columns
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The columns holding metric values.
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
tab_metrics_one <- function(data, col, negative = FALSE, ci = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Check
  check_is_dataframe(data)
  check_has_column(data, {{col}})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, {{ col }})

  # 4. Remove negatives
  if (!negative) {
    data <- data_rm_negatives(data, {{ col }})
  }

  # Calculate values
  result <- data %>%
    skim_metrics({{ col }}) %>%
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))

  if (ci) {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","ci.low","ci.high","n", "items","alpha"))
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

  # Remove items and alpha if not and index
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
    dplyr::mutate(dplyr::across(tidyselect::any_of(c("mean", "sd", "ci.low", "ci.high")), ~ as.character(round(., digits)))) %>%
    # labs_clear(-item) %>%
    tidyr::pivot_longer(-tidyselect::all_of("item")) %>%
    dplyr::select(-tidyselect::all_of("item"), {{ col }} := "name", "value")

  # Get item label from the attributes
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
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
tab_metrics_one_grouped <- function(data, col, cross, negative = FALSE, ci = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove negatives
  if (!negative) {
    data <- data_rm_negatives(data,  {{ col }})
  }

  # 4. Remove missings
  data <- data_rm_missings(data, c({{ col }}, {{ cross }}))

  # 5. Calculate values
  result_grouped <- data %>%
    dplyr::group_by({{ cross }}) %>%
    skim_metrics({{ col }}) %>%
    # dplyr::ungroup() %>%
    # dplyr::mutate(
    #   {{ cross }} := tidyr::replace_na(as.character({{ cross }}), "missing")
    # ) %>%
    dplyr::select(-tidyselect::all_of(c("skim_variable","skim_type"))) |>
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))

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
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","ci.low","ci.high","n","items","alpha"))
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
    codes <- data %>%
      codebook({{ cross }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("item_name", "item_label")))) %>%
      stats::na.omit()

    if (nrow(codes) > 0) {
      label <- codes$item_label[1]
      result <- result %>%
        dplyr::rename({{ label }} := {{ cross }})
    }

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
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param method The output metrics, TRUE or pearson = Pearson's R, spearman = Spearman's rho
#' @param ci Whether to output confidence intervals
#' @param digits The number of digits to print.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_one_cor(data, use_private, sd_age)
#'
#' @export
#' @importFrom rlang .data
tab_metrics_one_cor <- function(data, col, cross, negative = FALSE, method = "pearson", ci = FALSE, digits = 2, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ col }}, {{ cross }}))

  # 4. Remove negatives
  if (!negative) {
    data <- data_rm_negatives(data, c({{ col }}, {{ cross }}))
  }

  # 5. Get columns
  cols_eval <- tidyselect::eval_select(expr = enquo(col), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)

  # Calculate correlation
  method <- ifelse(method == "spearman", "spearman", "pearson")
  result <- .effect_correlations(data, {{ col }}, {{ cross}}, method = method, labels = labels)

  if (method=="spearman") {
    values <- c("item1", "item2", "n", "Spearman's rho")
  } else {
    values <- c("item1", "item2", "n", "Pearson's r")
  }
  if (ci) {
    values <- c(values, "ci.low", "ci.high")
  }

  result <- dplyr::select(result, tidyselect::all_of(values))

  # Remove common item prefix
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
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
tab_metrics_items <- function(data, cols, negative = FALSE, ci = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, {{ cols }})

  # 4. Remove negatives
  if (!negative) {
    data <- data_rm_negatives(data, {{ cols }})
  }

  # 5. Calculate
  result <- data %>%
    dplyr::select({{ cols }}) |>
    skim_metrics() |>
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))

  if (ci) {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","ci.low","ci.high","n","items","alpha"))
    )
  } else {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","n","items","alpha"))
    )
  }

  # Remove items and alpha if not and index
  if (all(is.na(result$items)) || all(is.na(result$alpha))) {
    result$items <- NULL
    result$alpha <- NULL
  } else {
    result <- result %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of("items"), ~ as.character(round(., 0)))) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of("alpha"), ~ as.character(round(., 2))))
  }

  # Get item labels from the attributes
  if (labels) {
    result <- labs_replace(
      result, "item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
    )
    attr(result, "limits") <- get_limits(data, {{ cols }}, negative)

    attr(result, "scale") <- codebook(data, {{ cols }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))
  }

  # Remove common item prefix and title
  # TODO: remove common postfix
  prefix <- get_prefix(result$item, trim=TRUE)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))


  # Rename first column
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
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
tab_metrics_items_grouped <- function(data, cols, cross, negative = FALSE, digits = 1, values = c("m", "sd"), labels = TRUE, clean = TRUE, ...) {
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}, {{ cross }}))

  # 4. Remove negatives
  if (!negative) {
    data <- data_rm_negatives(data, {{ cols }})
  }

  # Get positions of group cols
  cross <- tidyselect::eval_select(expr = enquo(cross), data = data)

  # total means
  value <- "numeric.mean"
  total_mean <- data %>%
    dplyr::select({{ cols }}) %>%
    skim_metrics() %>%
    dplyr::select("skim_variable", total = !!sym(value))

  # total sd
  value <- "numeric.sd"
  total_sd <- data %>%
    dplyr::select({{ cols }}) %>%
    skim_metrics() %>%
    dplyr::select("skim_variable", total = !!sym(value))

  # Grouped means
  value <- "numeric.mean"
  grouped_mean <- purrr::map(
    cross,
    function(col) {
      col <- names(data)[col]

      data %>%
        dplyr::filter(!is.na(!!sym(col))) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::select(!!sym(col), {{ cols }}) %>%
        skim_metrics() %>%
        dplyr::ungroup() %>%
        dplyr::select("skim_variable", !!sym(col), !!sym(value)) %>%
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
  grouped_sd <- purrr::map(
    cross,
    function(col) {
      col <- names(data)[col]

      data %>%
        dplyr::filter(!is.na(!!sym(col))) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::select(!!sym(col), {{ cols }}) %>%
        skim_metrics() %>%
        dplyr::ungroup() %>%
        dplyr::select("skim_variable", !!sym(col), !!sym(value)) %>%
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

  result_mean <- dplyr::inner_join(total_mean, grouped_mean, by = "skim_variable") %>%
    dplyr::rename(item = tidyselect::all_of("skim_variable"))

  result_sd <- dplyr::inner_join(total_sd, grouped_sd, by = "skim_variable") %>%
    dplyr::rename(item = tidyselect::all_of("skim_variable"))


  # Zip
  if (("m" %in% values) && ("sd" %in% values)) {
    # TODO: What about the resulting data frame, should it really contain rounded values?
    #       Maybe let zipping and rounding to the print function and return a list of data frames instead
    result_mean <- dplyr::mutate(result_mean, dplyr::across(tidyselect::where(is.numeric), ~ format(round(., digits), nsmall = digits)))
    result_sd <- dplyr::mutate(result_sd, dplyr::across(tidyselect::where(is.numeric), ~ format(round(., digits), nsmall = digits)))
    result <- zip_tables(result_mean, result_sd, brackets = TRUE)
  } else if ("sd" %in% values) {
    result <- result_sd
  } else {
    result <- result_mean
  }

  # Add labels
  if (labels) {
    result <- labs_replace(
      result, "item",
      codebook(data, {{ cols }}),
      "item_name","item_label"
    )
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item, trim=TRUE)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))

  # Rename first column
  if (prefix != "") {
    colnames(result)[1] <- prefix
  } else {
    result <- dplyr::rename(result, Item = tidyselect::all_of("item"))
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits= digits)
}


#' Output a correlation table
#'
#' @description
#' `r lifecycle::badge("experimental")`

#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The source columns.
#' @param cross The target columns or NULL to calculate correlations within the source columns.
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
#' tab_metrics_items_cor(data, starts_with("cg_adoption_adv"), starts_with("use_"))
#'
#' @importFrom rlang .data
#' @export
tab_metrics_items_cor <- function(data, cols, cross, negative = FALSE, method = "pearson", digits = 2, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}, {{ cross }}))

  # 4. Remove negatives
  if (!negative) {
    data <- data_rm_negatives(data, c({{ cols }}, {{ cross }}))
  }

  # 5. Calculate correlation
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

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits = digits, caption = title)
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
#' @return Formatted  table produced by \link{kable}.
knit_table <- function(df, ...) {
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
      dplyr::mutate(dplyr::across(dplyr::where(is.character), knit_prepare)) %>%
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
      dplyr::mutate_all(kableExtra::linebreak) %>%
      knitr::kable(
        "latex",
        booktabs = TRUE,
        escape = FALSE,
        align = c("l", rep("r", ncol(df) - 1)),
        digits = digits,
        #format.args = numberformat,
        ...
      )
  } else {
    df <- df %>%
      knitr::kable("pipe", align = c("l", rep("r", ncol(df) - 1)), digits = digits, ...)
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
#' @return Markdown text with line breaks and escaped special characters.
knit_prepare <- function(x) {
  x <- gsub("\n", "<br>", x, fixed=TRUE)
  x <- gsub("*", "\\*", x, fixed=TRUE)
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
  x <- knit_table(x)
  baseline <- attr(x, "baseline", exact=TRUE)

  if (knitr::is_html_output()) {

    if (!is.null(baseline)) {
      x <- paste0(x,"  \n  ", baseline)
    }

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

