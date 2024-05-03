#' Output a frequency table
#'
#' The type of frequency table depends on the number of selected columns:
#' - One column: see \link{tab_counts_one}
#' - Multiple columns: see \link{tab_counts_items}
#' - One column and one grouping column: see \link{tab_counts_one_grouped}
#' - Multiple columns and one grouping column: see \link{tab_counts_items_grouped}
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

  # Find columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)
  is_items <- length(cols_eval) > 1
  is_grouped <- length(cross_eval)== 1
  is_cor <- metric != FALSE

  # Single variables
  if (!is_items && !is_grouped) {
    tab_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_cor) {
    tab_counts_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !is_grouped) {
    tab_counts_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped &&  !is_cor) {
    tab_counts_items_grouped(data, {{ cols }}, {{ cross }},  ...)
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

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)

  is_items <- length(cols_eval) > 1
  is_grouped <- length(cross_eval) == 1
  is_cor <- metric != FALSE

  # Single variables
  if (!is_items && !is_grouped && !is_cor) {
    tab_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_cor) {
    tab_metrics_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }

  else if (!is_items && is_grouped && is_cor) {
    tab_metrics_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !is_grouped && !is_cor) {
    tab_metrics_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_cor) {
    tab_metrics_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not supported by volker functions.")
  }

}

#' Output a frequency table for the values in one column
#'
#' TODO: option to choose from prop.test or binom.test
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding values to count.
#' @param ci Whether to compute 95% confidence intervals.
#' @param missings Include missing values in the output (default TRUE).
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
tab_counts_one <- function(data, col, ci = FALSE, missings = TRUE, percent = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  if (!missings) {
    data <- data_rm_missings(data, {{ col }})
  }

  # 4. Count
  result <- data %>%
    dplyr::count({{ col }}) %>%
    tidyr::drop_na() %>%
    dplyr::mutate("{{ col }}" := as.character({{ col }})) %>%
    dplyr::mutate(p = .data$n / sum(.data$n))

  # 5. Confidence intervals
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


  # Totals
  result_total <- tibble::tibble(
    "Total",
    sum(!is.na(dplyr::select(data, {{ col }}))),
    ifelse(percent, "100%", 1)
  )
  colnames(result_total) <- colnames(result)

  result <- dplyr::bind_rows(result, result_total)

  # Missings
  if (missings) {
    result_missing <- tibble::tibble(
      "Missing",
      sum(is.na(dplyr::select(data, {{ col }}))),
      ifelse(percent, "", NA)
    )
    colnames(result_missing) <- colnames(result)
    result <- dplyr::bind_rows(result, result_missing)

  }

  # Clean NA
  result <- dplyr::mutate(result, dplyr::across(tidyselect::any_of(c("ci.low","ci.high")), ~ ifelse(is.na(.) && percent,"",.)))

  digits <- ifelse(percent, 0, 2)
  .to_vlkr_tab(result, digits=digits)
}

#' Output frequencies cross tabulated with a grouping column
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param cross The column holding groups to split.
#' @param missings Include missing values in the output (default FALSE).
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param prop The basis of percent calculation: "total" (the default), "cols", or "rows".
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
tab_counts_one_grouped <- function(data, col, cross, missings = FALSE, percent = TRUE, prop = "total", values = c("n", "p"), labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
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
  # 1. Count
  #
  grouped <- data %>%
    dplyr::count({{ col }}, {{ cross }}) %>%
    dplyr::mutate(
      "{{ cross }}" := tidyr::replace_na(as.character({{ cross }}), "Missing"),
      "{{ col }}" := tidyr::replace_na(as.character({{ col }}), "Missing")
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

  # Total column
  total_col_n <- data %>%
    dplyr::count({{ col }}) %>%
    dplyr::mutate(
      "{{ col }}" := tidyr::replace_na(as.character({{ col }}), "Missing")
    ) %>%
    dplyr::select({{ col }}, Total = "n")

  # Total row
  total_row_n <- grouped %>%
    dplyr::group_by({{ cross }}) %>%
    dplyr::summarise(n = sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("{{ col }}" := "Total") %>%
    tidyr::pivot_wider(
      names_from = {{ cross }},
      values_from = "n",
      values_fill = list(n = 0)
    )

  # Total
  total_n <- data %>%
    dplyr::count() %>%
    dplyr::mutate("{{ col }}" := "Total") %>%
    dplyr::select({{ col }}, Total = "n")

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
      dplyr::mutate(Total = .data$Total / sum(.data$Total))

    total_row_p <- total_row_n %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~1))
  } else if (prop == "rows") {
    rows_p <- grouped %>%
      dplyr::group_by({{ col }}) %>%
      dplyr::mutate(p = .data$n / sum(.data$n)) %>%
      dplyr::ungroup()

    total_col_p <- total_col_n %>%
      dplyr::mutate(Total = 1)

    total_row_p <- total_row_n %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ .x / total_n$Total))
  } else {
    rows_p <- grouped %>%
      dplyr::mutate(p = .data$n / sum(.data$n))

    total_col_p <- total_col_n %>%
      dplyr::mutate(Total = .data$Total / sum(.data$Total))

    total_row_p <- total_row_n %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~ .x / total_n$Total))
  }

  rows_p <- rows_p %>%
    dplyr::select({{ col }}, {{ cross }}, "p") %>%
    tidyr::pivot_wider(
      names_from = {{ cross }},
      values_from = "p",
      values_fill = list(p = 0)
    )

  total_p <- tibble::tibble("Total" = 1) %>%
    dplyr::mutate("{{col }}" := "Total")

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

  .to_vlkr_tab(result, digits=0)
}


#' Correlate categorical groups with one metric column
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
  stop("Not implemented yet")
}

#' Output frequencies for multiple variables
#'
#' TODO: Reorder boolean categories: first TRUE, then FALSE
#' TODO: Support single columns
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param ci Whether to compute 95% confidence intervals.
#' @param missings Include missing values (default FALSE).
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
tab_counts_items <- function(data, cols, ci = FALSE, missings = FALSE, percent = TRUE, values = c("n", "p"), labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Calculate n and p
  result <- data %>%
    tidyr::drop_na({{ cols }}) %>%
    labs_clear({{ cols }}) %>%
    tidyr::pivot_longer(
      {{ cols }},
      names_to = "item",
      values_to = "value"
    ) %>%
    dplyr::count(dplyr::across(tidyselect::all_of(c("item", "value")))) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of("item"))) %>%
    dplyr::mutate(p = .data$n / sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = as.factor(.data$value)) %>%
    dplyr::arrange(.data$value)

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
    dplyr::mutate(Total = sum(dplyr::c_across(-1), na.rm=TRUE)) %>%
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
    dplyr::mutate(Total = sum(dplyr::c_across(-1), na.rm=TRUE)) %>%
    dplyr::ungroup()


  # Add % sign
  if (percent) {
    result_p <- dplyr::mutate(result_p, dplyr::across(tidyselect::where(is.numeric), ~ paste0(round(. * 100, 0), "%")))
  }


  # Confidence intervals
  result_ci <- result |>
    dplyr::group_by(dplyr::across(tidyselect::all_of("item"))) %>%
    dplyr::mutate(.test = purrr::map(.data$n, function(x) stats::prop.test(x, sum(.data$n)))) |>
    dplyr::mutate(
      ci.low =purrr::map_dbl(.data$.test, function(x) x$conf.int[1]),
      ci.high =purrr::map_dbl(.data$.test, function(x) x$conf.int[2]),
    ) |>
    dplyr::select(-tidyselect::all_of(c(".test", "n"))) |>
    dplyr::ungroup()

  # Add % sign
  if (percent) {
    result_ci <- dplyr::mutate(result_ci, dplyr::across(tidyselect::where(is.numeric), ~ paste0(round(. * 100, 0), "%")))
  }

  # Add missings
  if (missings) {
    result_missing <-  data %>%
      labs_clear({{ cols }}) %>%
      tidyr::pivot_longer(
        {{ cols }},
        names_to = "item",
        values_to = "value"
      ) %>%
      dplyr::mutate(value = is.na(.data$value)) %>%
      dplyr::count(dplyr::across(tidyselect::all_of(c("item", "value")))) %>%
      dplyr::mutate(value = factor(.data$value,levels=c("TRUE","FALSE"))) %>%
      tidyr::pivot_wider(
        names_from = value,
        values_from = "n",
        values_fill = 0,
        names_expand=TRUE
      ) %>%
      dplyr::select("item", Missing = "TRUE")

    result_n <- dplyr::left_join(result_n, result_missing, by="item")
    result_p <-dplyr::mutate(result_p, Missing="")
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

    result_ci <- labs_replace(
      result_ci, "item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
    )
  }

  # Remove common item prefix
  # TODO: make dry
  prefix <- get_prefix(result$item, trim=T)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))
  result_ci <- dplyr::mutate(result_ci, item = trim_prefix(.data$item, prefix))

  # Rename first columns
  if (prefix == "") {
    prefix <- "Item"
  }
  colnames(result)[1] <- prefix
  colnames(result_ci)[1] <- prefix

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
      count = .to_vlkr_tab(result, caption = "Frequencies"),
      ci = .to_vlkr_tab(result_ci, digits=2, caption = "Confidence intervals")
    )
    result <- .to_vlkr_list(result)
  } else {
    result <- .to_vlkr_tab(result, digits= 0)
  }

  result

}


#' Compare the values in multiple items by a grouping column
#'
#' TODO: implement -> focus one category and show n / p
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
  stop("Not implemented yet")
}

#' Correlate the values in multiple items
#'
#' TODO: implement -> calculate npmi or similar for one value
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
  stop("Not implemented yet")
}

#' Output a five point summary table for the values in multiple columns
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The columns holding metric values.
#' @param ci Whether to calculate 95% confidence intervals of the mean.
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
tab_metrics_one <- function(data, col, ci = FALSE, negative = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{col}})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Recode negative values to NA
  if (!negative) {
    data <- data_rm_negatives(data, {{ col }})
  }

  # 3. Remove missings
  #data <- data_rm_missings(data, {{ col }})

  # Calculate values
  result <- data %>%
    skim_metrics({{ col }}) %>%
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))

  if (ci) {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","ci.low","ci.high","missing","n","items","alpha"))
    )
  } else {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","missing","n","items","alpha"))
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
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("missing", "n")), ~ as.character(round(., 0)))) %>%
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

  .to_vlkr_tab(result)
}


#' Output a five point summary for groups
#'
#' TODO: handle missings parameter
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param cross The column holding groups to compare.
#' @param ci Whether to output 95% confidence intervals.
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
tab_metrics_one_grouped <- function(data, col, cross, ci = FALSE, negative = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  #data <- data_rm_missings(data, {{ col }})
  #data <- data_rm_missings(data, {{ cross }})

  # 4. Remove negative values
  if (!negative) {
    data <- data_rm_negatives(data,  {{ col }})
  }

  # 5. Calculate values
  result_grouped <- data %>%
    dplyr::group_by({{ cross }}) %>%
    skim_metrics({{ col }}) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      {{ cross }} := tidyr::replace_na(as.character({{ cross }}), "Missing")
    ) %>%
    dplyr::select(-tidyselect::all_of(c("skim_variable","skim_type"))) |>
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))

  result_total <- data %>%
    skim_metrics({{ col }}) %>%
    dplyr::mutate({{ cross }} := "Total") |>
    dplyr::select(-tidyselect::all_of(c("skim_variable","skim_type"))) |>
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))


  result <- dplyr::bind_rows(
    result_grouped,
    result_total
  )

  if (ci) {
    result <- dplyr::select(
      result,  {{ cross }},
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","ci.low","ci.high","missing","n","items","alpha"))
    )
  } else {
    result <- dplyr::select(
      result,{{ cross }},
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","missing","n","items","alpha"))
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

  .to_vlkr_tab(result, digits= digits)
}

#' Correlate two columns
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The first column holding metric values.
#' @param cross The second column holding metric values.
#' @param method The output metrics, TRUE or p = Pearson's R, s = Spearman's rho
#' @param ci Whether to output confidence intervals
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_one_cor(data, starts_with("cg_adoption_"), sd_gender)
#'
#' @export
#' @importFrom rlang .data
tab_metrics_one_cor <- function(data, col, cross, method = "p", ci = FALSE, negative = FALSE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove negatives
  if (! negative) {
    data <- data_rm_negatives(data, {{ col }})
    data <- data_rm_negatives(data, {{ cross }})
  }

  # 4. Remove missings
  data <- data_rm_missings(data, {{ col }})
  data <- data_rm_missings(data, {{ cross }})

  # 6. Get columns
  cols_eval <- tidyselect::eval_select(expr = enquo(col), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)


  # Calculate correlation
  method <- ifelse(method == "s", "s", "p")
  result <- expand.grid(
    x = cols_eval, y = cross_eval, stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(x_name = names(.data$x), y_name = names(.data$y)) %>%
    dplyr::mutate(
      .test = purrr::map2(
        .data$x, .data$y,
        function(x, y) stats::cor.test(data[[x]], data[[y]], method = method, exact = method != "s")
      ),
      n = nrow(data),
      r = purrr::map(.data$.test, function(x) round(as.numeric(x$estimate),2)),
      ci.low = purrr::map(.data$.test, function(x) round(as.numeric(x$conf.int[1]), 2)),
      ci.high = purrr::map(.data$.test, function(x) round(as.numeric(x$conf.int[2]), 2))
    ) %>%
    dplyr::select(-tidyselect::all_of(c("x", "y",".test"))) |>
    dplyr::select(item1 = "x_name", item2 = "y_name", tidyselect::everything())


  if (!ci) {
    result <- dplyr::select(result, -tidyselect::all_of(c("ci.low","ci.high")))
  }


  # Get variable caption from the attributes
  if (labels) {
    result <- labs_replace(result, "item1", codebook(data, {{ col }}), col_from="item_name", col_to="item_label")
    result <- labs_replace(result, "item2", codebook(data, {{ cross }}), col_from="item_name", col_to="item_label" )
  }


  # Remove common item prefix
  prefix <- get_prefix(c(result$item1, result$item2))
  result <- dplyr::mutate(result, item1 = trim_prefix(.data$item1, prefix))
  result <- dplyr::mutate(result, item2 = trim_prefix(.data$item2, prefix))

  if (prefix == "") {
    prefix <- "Item"
  }

  result <- result %>%
    dplyr::rename("Item 1" = tidyselect::all_of("item1")) |>
    dplyr::rename("Item 2" = tidyselect::all_of("item2"))

  title <- ifelse(prefix == "", NULL, prefix)

  # TODO: print caption
  .to_vlkr_tab(result, digits= 2, caption=title)
}

#' Output a five point summary table for multiple items
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols The columns holding metric values
#' @param ci Whether to compute confidence intervals of the mean
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param digits The number of digits to print
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_items(data, starts_with("cg_adoption_"))
#'
#' @export
#' @importFrom rlang .data
tab_metrics_items <- function(data, cols, ci = FALSE, negative = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Recode negative values to NA
  if (!negative) {
    data <- data_rm_negatives(data, {{ cols }})
  }

  # # 3. Remove missings
  # if (!missings) {
  #   data <- data_rm_missings(data, {{ cols }})
  # }


  result <- data %>%
    dplyr::select({{ cols }}) |>
    skim_metrics() |>
    dplyr::rename_with(function(x) sub("numeric.", "", x, fixed = TRUE))

  if (ci) {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","ci.low","ci.high","missing","n","items","alpha"))
    )
  } else {
    result <- dplyr::select(
      result, "item" = "skim_variable",
      tidyselect::all_of(c("min","q1","median","q3","max","mean","sd","missing","n","items","alpha"))
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

  .to_vlkr_tab(result, digits= digits)
}

#' Output the means for groups in one or multiple columns
#'
#' TODO: handle completely missing data in single groups
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols The item columns that hold the values to summarize
#' @param cross The column holding groups to compare
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param digits The number of digits to print.
#' @param values The output metrics, mean (m), the standard deviation (sd) or both (the default).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_items_grouped(data, starts_with("cg_adoption_"), sd_gender)
#'
#' @export
#' @importFrom rlang .data
tab_metrics_items_grouped <- function(data, cols, cross, negative = FALSE, digits = 1, values = c("m", "sd"), labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}, {{ cross }}))

  # Remove negative values
  if (!negative) {
    data <- data_rm_negatives(data, {{ cols }})
  }

  # Get positions of group cols
  cross <- tidyselect::eval_select(expr = enquo(cross), data = data)

  # Total means
  value <- "numeric.mean"
  total_mean <- data %>%
    dplyr::select({{ cols }}) %>%
    skim_metrics() %>%
    dplyr::select("skim_variable", Total = !!sym(value))

  # Total sd
  value <- "numeric.sd"
  total_sd <- data %>%
    dplyr::select({{ cols }}) %>%
    skim_metrics() %>%
    dplyr::select("skim_variable", Total = !!sym(value))

  # Grouped means
  value <- "numeric.mean"
  grouped_mean <- map(
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
  grouped_sd <- map(
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
  # grouped_p <- map(
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


  .to_vlkr_tab(result, digits= digits)
}


#' Output a correlation table
#'
#' @description
#' `r lifecycle::badge("experimental")`
#'
#' TODO: do we need effects parameter here?
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols The source columns
#' @param cross The target columns or NULL to calculate correlations within the source columns
#' @param method The output metrics, p = Pearson's R, s = Spearman's rho
#' @param effects Add significance stars and only show significant values
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_items_cor(data, starts_with("cg_adoption_adv"), starts_with("use_"))
#'
#' @importFrom rlang .data
#' @export
tab_metrics_items_cor <- function(data, cols, cross, method = "p", effects = FALSE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  # TODO: output a warning
  # data <- data %>%
  #   tidyr::drop_na({{ cols }}, {{ cols_cor }})

  # Prepare parameters
  cols <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cross <- tidyselect::eval_select(expr = enquo(cross), data = data)


  result <- expand.grid(x = cols, y = cross, stringsAsFactors = FALSE) %>%
    dplyr::mutate(x_name = names(.data$x), y_name = names(.data$y)) %>%
    dplyr::mutate(
      test = purrr::map2(
        .data$x, .data$y,
        function(x, y) stats::cor.test(data[[x]], data[[y]], method = method)
      ),
      value = purrr::map(.data$test, function(x) round(as.numeric(x$estimate),2)),
      stars = purrr::map(.data$test, function(x) get_stars(x$p.value)),
      p = purrr::map(.data$test, function(x) round(x$p.value,2))
    ) %>%
    dplyr::select(item = "x_name", target = "y_name", "value", "p", "stars")


  # Remove common item prefix
  result <- dplyr::mutate(result, item = trim_prefix(.data$item))
  result <- dplyr::mutate(result, target = trim_prefix(.data$target))

  return (result)
  # Create table
  result <- result %>%
    dplyr::mutate(value = round(unlist(.data$value), 2))

  if (effects == TRUE) {
    result <- result %>%
      dplyr::mutate(value = paste0(unlist(.data$value), .data$stars)) %>%
      dplyr::mutate(value = ifelse(.data$p >= 0.1, "", .data$value))
  }

  result <- dplyr::select(result, "item", "target", "value")

  result <- result %>%
    tidyr::pivot_wider(names_from = "target", values_from = "value") %>%
    dplyr::rename(Item = tidyselect::all_of("item"))

  .to_vlkr_tab(result, digits= 2)
}

#' Add vlkr_tbl class
#'
#' Additionally, removes the skim_df class if present
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param digits Set the plot digits. If NULL (default), no digits are set.
#' @return A volker tibble
.to_vlkr_tab <- function(data, digits = NULL, caption=NULL) {
  if (!is.null(digits)) {
    attr(data, "digits") <- digits
  }

  if (!is.null(caption)) {
    attr(data, "caption") <- caption
  }

  class(data) <- c("vlkr_tbl", setdiff(class(data), "skim_df"))
  data
}

#' Knit volker tables
#'
#' @keywords internal
#'
#' @param df Data frame
#' @return Formatted  table produced by \link{kable}
knit_table <- function(df, ...) {

  options(knitr.kable.NA = '')

  # TODO: Embed "digits" in the vlkr_options list
  digits <- attr(df, "digits", exact = TRUE)

  if (is.null(digits)) {
    digits <- getOption("digits")
  }

  # Round
  if (".digits" %in% colnames(df)) {
    df <- df |>
      dplyr::rowwise() |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) round(x, .data$.digits))) |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), \(x) as.character(x)))

    df$.digits <- NULL
    digits <- getOption("digits")
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
        ...
      ) %>%
      kableExtra::kable_styling()
  } else if (knitr::is_latex_output()) {
    df <- df %>%
      dplyr::mutate_all(kableExtra::linebreak) %>%
      knitr::kable(
        "latex",
        booktabs = TRUE,
        escape = FALSE,
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


#' Prepare markdown content for table rendering
#'
#' @keywords internal
#'
#' @param x Markdown text
#' @return Markdown text with line breaks and excaped special characters
knit_prepare <- function(x) {
  x <- gsub("\n", "<br>", x, fixed=TRUE)
  x <- gsub("*", "\\*", x, fixed=TRUE)
  x
}

#' Printing method for volker tables.
#'
#' @keywords internal
#'
#' @param x The volker table
#' @param ... Further parameters passed to print()
#' @return No return value
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

  if (knitr::is_html_output()) {
    x <- knitr::asis_output(x)
    knitr::knit_print(x)
  } else {
    caption <- attr(x, "caption", exact=TRUE)
    if (!is.null(caption)) {
      cat("\n",caption)
    }
    print(x, ...)
  }
}

