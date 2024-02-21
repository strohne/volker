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
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Other parameters passed to the appropriate table function
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_counts(data, sd_gender)
#'
#' @export
tab_counts <- function(data, cols, col_group=NULL, clean= TRUE, ...) {
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
    tab_counts_items(data, {{ cols }} , ...)
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
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Other parameters passed to the appropriate table function
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics(data, sd_age)
#'
#' @export
tab_metrics <- function(data, cols, col_group=NULL, clean= TRUE, ...) {
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
    tab_metrics_items(data, {{ cols }} , ...)
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
#' @param missings Include missing values in the output (default TRUE)
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_counts_one(data, sd_gender)
#'
#' @importFrom rlang .data
#' @export
tab_counts_one <- function(data, col, missings = TRUE, percent = TRUE, labels = TRUE, clean= TRUE, ...) {
  # 1. Checks
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  # TODO: output a warning
  if (!missings) {
    data <- data %>%
      tidyr::drop_na({{ col }})
  }

  result <- data %>%
    dplyr::count({{ col }}) %>%
    tidyr::drop_na() %>%
    dplyr::mutate("{{ col }}" := as.character({{ col }})) %>%
    dplyr::mutate(p = .data$n / sum(.data$n))

  # Get variable caption from the attributes
  if (labels) {
    result <- labs_replace_values(result, {{ col }}, codebook(data, {{ col }}))
    label <- get_title(data, {{ col }})
    result <- dplyr::rename(result, {{ label }} := {{ col }})

  }

  if (percent) {
    result <- dplyr::mutate(result, p = paste0(round(.data$p * 100, 0), "%"))
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

  .to_vlkr_tab(result, digits=0)
}

#' Output frequencies cross tabulated with a grouping column
#'
#' @param data A tibble
#' @param col The column holding factor values
#' @param col_group The column holding groups to compare
#' @param missings Include missing values in the output (default FALSE)
#' @param prop The basis of percent calculation: "total" (the default), "cols", or "rows".
#' @param values The values to output: n (frequency) or p (percentage) or both (the default).
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_counts(data, adopter, sd_gender)
#'
#' @importFrom rlang .data
#' @export
tab_counts_one_grouped <- function(data, col, col_group, missings = FALSE, prop = "total", values = c("n", "p"), percent = TRUE, labels = TRUE, clean= TRUE, ...) {

  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ col_group }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  # TODO: output a warning
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
      "{{ col_group }}" := tidyr::replace_na(as.character({{ col_group }}), "Missing"),
      "{{ col }}" := tidyr::replace_na(as.character({{ col }}), "Missing")
    )

  #
  # 2. N
  #
  rows_n <- grouped %>%
    dplyr::select({{ col_group }}, {{ col }}, "n") %>%
    tidyr::pivot_wider(
      names_from = {{ col }},
      values_from = "n",
      values_fill = list(n = 0)
    )

  # Total column
  total_col_n <- data %>%
    dplyr::count({{ col_group }}) %>%
    dplyr::mutate(
      "{{ col_group }}" := tidyr::replace_na(as.character({{ col_group }}), "Missing")
    ) %>%
    dplyr::select({{ col_group }}, Total = "n")

  # Total row
  total_row_n <- grouped %>%
    dplyr::group_by({{ col }}) %>%
    dplyr::summarise(n = sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate("{{ col_group }}" := "Total") %>%
    tidyr::pivot_wider(
      names_from = {{ col }},
      values_from = "n",
      values_fill = list(n = 0)
    )

  # Total
  total_n <- data %>%
    dplyr::count() %>%
    dplyr::mutate("{{ col_group }}" := "Total") %>%
    dplyr::select({{ col_group }}, Total = "n")

  # Join
  result_n <-
    dplyr::full_join(
      total_col_n,
      rows_n,
      by = as.character(rlang::get_expr(rlang::enquo(col_group)))
    ) %>%
    dplyr::bind_rows(
      dplyr::left_join(
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
      dplyr::mutate(p = .data$n / sum(.data$n)) %>%
      dplyr::ungroup()

    total_col_p <- total_col_n %>%
      dplyr::mutate(Total = .data$Total / sum(.data$Total))

    total_row_p <- total_row_n %>%
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), ~1))
  } else if (prop == "rows") {
    rows_p <- grouped %>%
      dplyr::group_by({{ col_group }}) %>%
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
    dplyr::select({{ col }}, {{ col_group }}, "p") %>%
    tidyr::pivot_wider(
      names_from = {{ col }},
      values_from = "p",
      values_fill = list(p = 0)
    )

  total_p <- tibble::tibble("Total" = 1) %>%
    dplyr::mutate("{{ col_group }}" := "Total")

  # Join
  result_p <-
    dplyr::full_join(
      total_col_p,
      rows_p,
      by = as.character(rlang::get_expr(rlang::enquo(col_group)))
    ) %>%
    dplyr::bind_rows(
      dplyr::left_join(
        total_p,
        total_row_p,
        by = as.character(rlang::get_expr(rlang::enquo(col_group)))
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
      codebook({{ col_group }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("item_name", "item_label")))) %>%
      stats::na.omit()

    if (nrow(codes) > 0) {
      label <- codes$item_label[1]
      result <- result %>%
        dplyr::rename({{ label }} := {{ col_group }})
    }
  }

  .to_vlkr_tab(result, digits=0)
}

#' Output frequencies for multiple variables
#'
#' TODO: Reorder boolean categories: first TRUE, then FALSE
#' TODO: Support single columns
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param missings Include missing values (default FALSE)
#' @param values The values to output: n (frequency) or p (percentage) or both (the default)
#' @param percent Set to FALSE to prevent calculating percents from proportions
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_counts_items(data, starts_with("cg_adoption_"))
#'
#' @export
#' @importFrom rlang .data
tab_counts_items <- function(data, cols, missings= FALSE, values = c("n", "p"), percent = TRUE, labels = TRUE, clean= TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  # TODO: Output a warning
  if (!missings) {
    data <- data %>%
      tidyr::drop_na({{ cols }})
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
    janitor::adorn_totals("col")

  # Relative frequency
  value <- "p"
  result_p <- result %>%
    dplyr::select("item", "value", !!sym(value)) %>%
    tidyr::pivot_wider(
      names_from = value,
      values_from = !!sym(value),
      values_fill = stats::setNames(list(0), value)
    ) %>%
    janitor::adorn_totals("col")

  # Add % sign
  if (percent) {
    result_p <- dplyr::mutate(result_p, dplyr::across(tidyselect::where(is.numeric), ~ paste0(round(. * 100, 0), "%")))
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
    result <- labs_replace_names(result, "item", codebook(data, {{ cols }}))
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item)
  if (prefix != "") {
    result <- dplyr::mutate(result, item = stringr::str_remove(.data$item, prefix))
    result <- dplyr::mutate(result, item = ifelse(.data$item == "", prefix, .data$item))
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


  # Rename first column
  if (prefix != "") {
    colnames(result)[1] <- sub("[ :,]+$", "", prefix)
  } else {
    result <- dplyr::rename(result, Item = tidyselect::all_of("item"))
  }

  .to_vlkr_tab(result, digits= 0)
}


#' Compare the values in multiple items by a grouping column.
#'
#' TODO: implement -> focus one category and show n / p
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols The item columns that hold the values to summarize
#' @param col_group The column holding groups to compare
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble
#' @importFrom rlang .data
tab_counts_items_grouped <- function(data, cols, col_group, clean= TRUE, ...) {

  stop("Not implemented yet")

    # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

}

#' Correlate the values in multiple items
#'
#' TODO: implement -> calculate npmi or similar for one value
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols The source columns
#' @param cols_cor The target columns or NULL to calculate correlations within the source columns
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @importFrom rlang .data
tab_counts_items_cor <- function(data, cols, cols_cor, clean= TRUE, ...) {

  stop("Not implemented yet")

  # 1.Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

}

#' Output a five point summary table for the values in multiple columns
#'
#' @param data A tibble
#' @param col The columns holding metric values
#' @param digits The number of digits to print.
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_one(data, sd_age)
#'
#' @export
#' @importFrom rlang .data
tab_metrics_one <- function(data, col, negative= FALSE, digits = 1, labels = TRUE, clean= TRUE, ...) {

  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{col}})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # # Remove missings
  # # TODO: output a warning
  # data <- data %>%
  #   tidyr::drop_na({{ col }})

  # Remove negative values
  # TODO: warn if any negative values were recoded
  if (!negative) {
    data <- dplyr::mutate(data, dplyr::across({{ col }}, ~ dplyr::if_else(. < 0, NA, .)))
  }

  result <- data %>%
    skim_metrics({{ col }}) %>%
    dplyr::select(
      "item" = "skim_variable",
      min = "numeric.min",
      q1 = "numeric.q1",
      median = "numeric.median",
      q3 = "numeric.q3",
      max = "numeric.max",
      m = "numeric.mean",
      sd = "numeric.sd",
      "missing",
      "n",
      items = "numeric.items",
      alpha = "numeric.alpha"
    )

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
    dplyr::mutate(dplyr::across(tidyselect::all_of(c("m", "sd")), ~ as.character(round(., digits)))) %>%
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
#' @param data A tibble
#' @param col The column holding metric values
#' @param col_group The column holding groups to compare
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
#' tab_metrics_one_grouped(data, sd_age, sd_gender)
#'
#' @export
#' @importFrom rlang .data
tab_metrics_one_grouped <- function(data, col, col_group, negative = FALSE, digits = 1, labels = TRUE, clean= TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  # TODO: output a warning
  data <- data %>%
    tidyr::drop_na({{ col_group }})

  # Remove negative values
  # TODO: warn if any negative values were recoded
  if (!negative) {
    data <- dplyr::mutate(data, dplyr::across({{ col }}, ~ dplyr::if_else(. < 0, NA, .)))
  }

  result_grouped <- data %>%
    dplyr::group_by({{ col_group }}) %>%
    skim_metrics({{ col }}) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      {{ col_group }} := tidyr::replace_na(as.character({{ col_group }}), "Missing")
    ) %>%
    dplyr::select(-tidyselect::all_of(c("skim_variable","skim_type")))

  result_total <- data %>%
    skim_metrics({{ col }}) %>%
    dplyr::mutate({{ col_group }} := "Total")

  result <- dplyr::bind_rows(
    result_grouped,
    result_total
  ) %>%
    dplyr::select(
      {{ col_group }},
      min = "numeric.min",
      q1 = "numeric.q1",
      median = "numeric.median",
      q3 = "numeric.q3",
      max = "numeric.max",
      m = "numeric.mean",
      sd = "numeric.sd",
      "missing",
      "n",
      items = "numeric.items",
      alpha = "numeric.alpha"
    )

  # Remove items and alpha if not and index
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
      codebook({{ col_group }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("item_name", "item_label")))) %>%
      stats::na.omit()

    if (nrow(codes) > 0) {
      label <- codes$item_label[1]
      result <- result %>%
        dplyr::rename({{ label }} := {{ col_group }})
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


#' Output a five point summary table for multiple items
#'
#' @param data A tibble
#' @param cols The columns holding metric values
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
tab_metrics_items <- function(data, cols, negative = FALSE, digits = 1, labels = TRUE, clean= TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  # TODO: output a warning
  # data <- data %>%
  #   tidyr::drop_na({{ cols }})

  result <- data %>%
    dplyr::select({{ cols }})

  # Remove negative values
  # TODO: warn if any negative values were recoded
  if (!negative) {
    result <- dplyr::mutate(result, dplyr::across(tidyselect::where(is.numeric), ~ ifelse(. < 0, NA, .)))
  }

  result <- result %>%
    skim_metrics()

  result <- result %>%
    dplyr::select(
      item = "skim_variable",
      min = "numeric.min",
      q1 = "numeric.q1",
      median = "numeric.median",
      q3 = "numeric.q3",
      max = "numeric.max",
      m = "numeric.mean",
      sd = "numeric.sd",
      "missing",
      "n",
      items = "numeric.items",
      alpha = "numeric.alpha"
    )

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
    result <- labs_replace_names(result, "item", codebook(data, {{ cols }}))
    attr(result, "limits") <- get_limits(data, {{ cols }}, negative)

    attr(result, "scale") <- codebook(data, {{ cols }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))
  }

  # Remove common item prefix and title
  # TODO: remove common postfix
  prefix <- get_prefix(result$item)
  if (prefix != "") {
    result <- dplyr::mutate(result, item = stringr::str_remove(.data$item, stringr::fixed(prefix)))
    result <- dplyr::mutate(result, item = ifelse(.data$item == "", prefix, .data$item))
  }


  # Rename first column
  if (prefix != "") {
    colnames(result)[1] <- sub("[ :,]+$", "", prefix)
  } else {
    result <- dplyr::rename(result, Item = tidyselect::all_of("item"))
  }

  .to_vlkr_tab(result, digits= digits)
}

#' Output the means for groups in one or multiple columns
#'
#' TODO: handle completely missing data in single groups
#'
#' @param data A tibble
#' @param cols The item columns that hold the values to summarize
#' @param col_group The column holding groups to compare
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param values The output metrics, mean (m), the standard deviation (sd) or both (the default).
#' @param digits The number of digits to print.
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
tab_metrics_items_grouped <- function(data, cols, col_group, negative = FALSE, values = c("m", "sd"), digits = 1, labels = TRUE, clean= TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  # TODO: output a warning
  data <- data %>%
    tidyr::drop_na({{ cols }}, {{ col_group }})

  # Remove negative values
  # TODO: warn if any negative values were recoded
  if (!negative) {
    data <- dplyr::mutate(data, dplyr::across({{ cols }}, ~ dplyr::if_else(. < 0, NA, .)))
  }

  # Get positions of group cols
  col_group <- tidyselect::eval_select(expr = enquo(col_group), data = data)

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
    col_group,
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
    col_group,
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
    result <- labs_replace_names(result, "item", codebook(data, {{ cols }}))
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item)
  if (prefix != "") {
    result <- dplyr::mutate(result, item = stringr::str_remove(.data$item, prefix))
    result <- dplyr::mutate(result, item = ifelse(.data$item == "", prefix, .data$item))
  }

  # Rename first column
  if (prefix != "") {
    # colnames(result)[1] <-  sub("[ :,]+$", "",  prefix)
    colnames(result)[1] <- trim_label(prefix)
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
#' @param data A tibble
#' @param cols The source columns
#' @param cols_cor The target columns or NULL to calculate correlations within the source columns
#' @param method The output metrics, p = Pearson's R, s = Spearman's rho
#' @param significant Only show significant values
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_items_cor(data, starts_with("cg_adoption_adv"))
#'
#' @importFrom rlang .data
#' @export
tab_metrics_items_cor <- function(data, cols, cols_cor, method = "p", significant = FALSE, labels= TRUE, clean= TRUE, ...) {

  # 1. Checks
  check_is_dataframe(data)

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

  if (missing(cols_cor)) {
    cols_cor <- cols
  } else {
    cols_cor <- tidyselect::eval_select(expr = enquo(cols_cor), data = data)
  }


  result <- expand.grid(x = cols, y = cols_cor, stringsAsFactors = FALSE) %>%
    dplyr::mutate(x_name = names(.data$x), y_name = names(.data$y)) %>%
    dplyr::mutate(
      test = purrr::map2(
        .data$x, .data$y,
        function(x, y) stats::cor.test(data[[x]], data[[y]], method = method)
      ),
      p = map(.data$test, function(x) x$p.value),
      value = purrr::map(.data$test, function(x) as.numeric(x$estimate)),
      stars = get_stars(.data$p)
    ) %>%
    dplyr::select(item = "x_name", target = "y_name", "value", "p", "stars")


  # Remove common item prefix
  prefix <- get_prefix(result$item)
  if (prefix != "") {
    result <- dplyr::mutate(result, item = stringr::str_remove(.data$item, prefix))
    result <- dplyr::mutate(result, item = dplyr::if_else(.data$item == "", prefix, .data$item))
  }

  prefix <- get_prefix(result$target)
  if (prefix != "") {
    result <- dplyr::mutate(result, target = stringr::str_remove(.data$target, prefix))
    result <- dplyr::mutate(result, target = ifelse(.data$target == "", prefix, .data$target))
  }

  # Create table
  result <- result %>%
    dplyr::mutate(value = paste0(round(unlist(.data$value), 2), .data$stars)) %>%
    dplyr::mutate(value = ifelse(significant & (.data$p >= 0.1), "", .data$value)) %>%
    dplyr::select("item", "target", "value")

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
.to_vlkr_tab <- function(data, digits=NULL) {

  if (!is.null(digits)) {
    attr(data, "digits") <- digits
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
  # TODO: Embed "digits" in the vlkr_options list
  digits <- attr(df, "digits", exact = TRUE)

  if (is.null(digits)) {
    digits <- getOption("digits")
  }

  if (knitr::is_html_output()) {
    # Replace \n by <br>
    df <- df %>%
      dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ gsub("\n", "<br>", .))) %>%
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
    print(x, ...)
  }
}

