#' Output a frequency plot
#'
#' @description
#' The type of frequency plot depends on the number of selected columns:
#'
#' - One categorical column: see \link{plot_counts_one}
#' - Multiple categorical columns: see \link{plot_counts_items}
#'
#' Cross tabulations:
#'
#' - One categorical column and one grouping column: see \link{plot_counts_one_grouped}
#' - Multiple categorical columns and one grouping column: see \link{plot_counts_items_grouped}
#' - Two categorical column selections: see \link{plot_counts_items_grouped_items} (not yet implemented)
#'
#' By default, if you provide two column selections, the second selection is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - One categorical column and one metric column: see \link{plot_counts_one_cor}
#' - Multiple categorical columns and one metric column: see \link{plot_counts_items_cor}
#' - Multiple categorical columns and multiple metric columns: see \link{plot_counts_items_cor_items} (not yet implemented)
#'
#' Parameters that may be passed to the count functions
#' (see the respective function help):
#' - **ci**: Add confidence intervals to proportions.
#' - **ordered**: The values of the cross column can be nominal (0), ordered ascending (1), or ordered descending (-1).
#'                The colors are adjusted accordingly.
#' - **category**: When you have multiple categories in a column, you can focus one of the categories to simplify the plots.
#'                By default, if a column has only TRUE and FALSE values, the outputs focus the TRUE category.
#' - **prop**: For stacked bar charts, displaying row percentages instead of total percentages gives a direct visual comparison of groups.
#' - **limits**: The scale limits are automatically guessed by the package functions (work in progress).
#'              Use the limits-parameter to manually fix any misleading graphs.
#' - **title**: All plots usually get a title derived from the column attributes or column names.
#'              Set to FALSE to suppress the title or provide a title of your choice as a character value.
#' - **labels**: Labels are extracted from the column attributes.
#'               Set to FALSE to output bare column names and values.
#' - **numbers**: Set the numbers parameter to “n” (frequency), “p” (percentage) or c(“n”,“p”).
#'                To prevent cluttering and overlaps, numbers are only plotted on bars larger than 5%.
#'
#' `r lifecycle::badge("experimental")`
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
#' @param ... Other parameters passed to the appropriate plot function.
#' @return A ggplot2 plot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts(data, sd_gender)
#'
#' @export
plot_counts <- function(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...) {
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
  if (!is_items && !is_grouped && !is_multi) {
    plot_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    plot_counts_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }

  else if (!is_items && is_grouped && is_metric) {
    plot_counts_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !is_grouped && !is_multi) {
    plot_counts_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_metric) {
    plot_counts_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && is_grouped && is_metric) {
    plot_counts_items_cor(data, {{ cols }}, {{ cross }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not yet supported by volker functions.")
  }

}

#' Output a plot with distribution parameters such as the mean values
#'
#' @description
#' The plot type depends on the number of selected columns:
#'
#' - One metric column: see \link{plot_metrics_one}
#' - Multiple metric columns: see \link{plot_metrics_items}
#'
#' Group comparisons:
#'
#' - One metric column and one grouping column: see \link{plot_metrics_one_grouped}
#' - Multiple metric columns and one grouping column: see \link{plot_metrics_items_grouped}
#' - Multiple metric columns and multiple grouping columns: see \link{plot_metrics_items_grouped_items} (not yet implemented)
#'
#' By default, if you provide two column selections, the second selection is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - Two metric columns: see \link{plot_metrics_one_cor}
#' - Multiple metric columns and one metric column : see \link{plot_metrics_items_cor}
#' - Two metric column selections: see \link{plot_metrics_items_cor_items}
#'
#' Parameters that may be passed to the metric functions
#' (see the respective function help):
#' - **ci**: Plot confidence intervals for means or correlation coefficients.
#' - **box**: Visualise the distribution by adding boxplots.
#' - **log**: In scatter plots, you can use a logarithmic scale.
#'            Be aware, that zero values will be omitted because their log value is undefined.
#' - **method**: By default, correlations are calculated using Pearson’s R.
#'               You can choose Spearman’s Rho with the methods-parameter.
#' - **limits**: The scale limits are automatically guessed by the package functions (work in progress).
#'               Use the limits-parameter to manually fix any misleading graphs.
#' - **title**: All plots usually get a title derived from the column attributes or column names.
#'              Set to FALSE to suppress the title or provide a title of your choice as a character value.
#' - **labels**: Labels are extracted from the column attributes.
#'               Set to FALSE to output bare column names and values.
#' - **numbers**: Controls whether to display correlation coefficients on the plot.
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
#' @param ... Other parameters passed to the appropriate plot function.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics(data, sd_age)
#'
#' @export
plot_metrics <- function(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...) {
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
  is_grouped <- length(cross_eval) == 1
  is_multi <- length(cross_eval) > 1
  is_metric <- metric != FALSE

  # Single variables
  if (!is_items && !is_grouped && !is_multi) {
    plot_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    plot_metrics_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }
  else if (!is_items && is_grouped && is_metric) {
    plot_metrics_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !is_grouped && !is_multi) {
    plot_metrics_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_metric) {
    plot_metrics_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && is_grouped && is_metric) {
    plot_metrics_items_cor(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && !is_grouped && is_multi && is_metric) {
    plot_metrics_items_cor_items(data, {{ cols }}, {{ cross }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not yet supported by volker functions.")
  }

}

#' Plot the frequency of values in one column
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding values to count.
#' @param category The value FALSE will force to plot all categories.
#'                  A character value will focus a selected category.
#'                  When NULL, in case of boolean values, only the TRUE category is plotted.
#' @param ci Whether to plot error bars for 95% confidence intervals.
#' @param limits The scale limits, autoscaled by default.
#'               Set to \code{c(0,100)} to make a 100% plot.
#'               If the data is binary or focused on a single category, by default a 100% plot is created.
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts_one(data, sd_gender)
#'
#' @importFrom rlang .data
#' @export
plot_counts_one <- function(data, col, category = NULL, ci = FALSE, limits = NULL, numbers = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Data
  # Count data
  result <- data %>%
    dplyr::count({{ col }}) %>%
    dplyr::mutate(p = (.data$n / sum(.data$n)) * 100)

  # 3. Confidence intervals
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

  # 4. Numbers
  result <- result %>%
    dplyr::mutate(
      .values = dplyr::case_when(
        .data$p < VLKR_LOWPERCENT ~ "",
        all(numbers == "n") ~ as.character(.data$n),
        all(numbers == "p") ~ paste0(round(.data$p, 0), "%"),
        TRUE ~ paste0(.data$n, "\n", round(.data$p, 0), "%")
      )
    )

  # 5. Item labels
  result <- result |>
    dplyr::mutate("{{ col }}" := as.factor({{ col }}))

  if (labels) {
    result <- labs_replace(result, {{ col }}, codebook(data, {{ col }}))
  }

  # 6. Detect the scale (whether the categories are binary and direction)
  # TODO: make dry
  if (!is.null(category)) {
    result <- dplyr::filter(result, as.character({{ col }}) == as.character(category))
  }
  categories <- dplyr::pull(result, {{ col }}) |> unique() |> as.character()
  if ((length(categories) == 2) && (is.null(category)) && ("TRUE" %in% categories)) {
    result <- dplyr::filter(result, as.character({{ col }}) == "TRUE")
  }

  result <- dplyr::rename(result, item = {{ col }})
  result <- dplyr::mutate(result, value = factor("TRUE"))
  category = "TRUE"

  # 7. Title
  if (title == TRUE) {
    title <- get_title(data, {{ col }})
  } else if (title == FALSE) {
    title <- NULL
  }

  # 8. Base
  base_n <- nrow(data)
  result <- .attr_transfer(result, data, "missings")

  .plot_bars(
    result,
    category = category,
    ci = ci,
    scale = 0,
    limits=limits,
    numbers = numbers,
    base = paste0("n=", base_n),
    title = title
  )
}

#' Plot frequencies cross tabulated with a grouping column
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param cross The column holding groups to split.
#' @param ordered The values of the cross column can be nominal (0), ordered ascending (1), or descending (-1).
#'                By default (NULL), the ordering is automatically detected.
#'                An appropriate color scale should be chosen depending on the ordering.
#'                For unordered values, colors from VLKR_FILLDISCRETE are used.
#'                For ordered values, shades of the VLKR_FILLGRADIENT option are used.
#' @param category The value FALSE will force to plot all categories.
#'                  A character value will focus a selected category.
#'                  When NULL, in case of boolean values, only the TRUE category is plotted.
#' @param prop The basis of percent calculation: "total" (the default), "rows" or "cols".
#'             Plotting row or column percentages results in stacked bars that add up to 100%.
#'             Whether you set rows or cols determines which variable is in the legend (fill color)
#'             and which on the vertical scale.
#' @param limits The scale limits, autoscaled by default.
#'               Set to \code{c(0,100)} to make a 100 % plot.
#' @param numbers The numbers to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts_one_grouped(data, adopter, sd_gender)
#'
#' @export
#' @importFrom rlang .data
plot_counts_one_grouped <- function(data, col, cross, category = NULL, prop = "total", limits = NULL, ordered = NULL, numbers = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Swap columns
  if (prop == "cols") {
    col <- rlang::enquo(col)
    cross <- rlang::enquo(cross)
    col_temp <- col
    col <- cross
    cross <- col_temp
    #stop("To display column proportions, swap the first and the grouping column. Then set the prop parameter to \"rows\".")
  }

  # 3. Calculate data
  result <- data %>%
    dplyr::count({{ col }}, {{ cross }})

  # 4. Set labels
  # data <- data |>
  #   dplyr::mutate(data, "{{ col_group }}" := as.factor({{ col_group }}))

  result <- result %>%
    dplyr::mutate(item = factor({{ col }})) |>
    dplyr::mutate(value = as.factor({{ cross }}))

    #dplyr::mutate(value = factor({{ col }}, levels = categories))

  if ((prop == "rows") || (prop == "cols")) {
    result <- result %>%
      dplyr::group_by({{ col }}) %>%
      dplyr::mutate(p = (.data$n / sum(.data$n)) * 100) %>%
      dplyr::ungroup()
  } else {
    result <- result %>%
      dplyr::mutate(p = (.data$n / sum(.data$n)) * 100)
  }

  # Detect the scale (whether the categories are binary and direction)
  # TODO: make dry
  scale <-dplyr::coalesce(ordered, get_direction(data, {{ cross }}))

  categories <- dplyr::pull(result, {{ cross }}) |> unique() |> as.character()

  if ((length(categories) == 2) && (is.null(category)) && ("TRUE" %in% categories)) {
    category <- "TRUE"
  }

  if (labels) {
    result <- labs_replace(result, "value", codebook(data, {{ cross }}))
    result <- labs_replace(result, "item", codebook(data, {{ col }}))
  }

  lastcategory <- ifelse(scale > 0, categories[1], categories[length(categories)])

  #return(result)
  # Select numbers to print on the bars
  # ...omit the last category in scales, omit small bars
  result <- result %>%
    dplyr::mutate(
      .values = dplyr::case_when(
        (is.null(category)) & (scale != 0) & (lastcategory == .data$value) ~ "",
        .data$p < VLKR_LOWPERCENT ~ "",
        all(numbers == "n") ~ as.character(.data$n),
        all(numbers == "p") ~ paste0(round(.data$p, 0), "%"),
        TRUE ~ paste0(.data$n, "\n", round(.data$p, 0), "%")
      )
    )

  # Get title
  if (title == TRUE) {
    title <- get_title(data, {{ col }})
  } else if (title == FALSE) {
    title <- NULL
  }

  # Get base
  base_n <- nrow(data)

  result <- .attr_transfer(result, data, "missings")

  .plot_bars(
    result,
    category = category,
    scale = scale,
    numbers = numbers,
    base = paste0("n=", base_n),
    title = title
  )
}

#' Plot frequencies cross tabulated with a metric column that will be split into groups
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param cross A metric column that will be split into groups at the median.
#' @param ordered The values of the cross column can be nominal (0), ordered ascending (1), or descending (-1).
#'                By default (NULL), the ordering is automatically detected.
#'                An appropriate color scale should be chosen depending on the ordering.
#'                For unordered values, colors from VLKR_FILLDISCRETE are used.
#'                For ordered values, shades of the VLKR_FILLGRADIENT option are used.
#' @param category The value FALSE will force to plot all categories.
#'                  A character value will focus a selected category.
#'                  When NULL, in case of boolean values, only the TRUE category is plotted.
#' @param prop The basis of percent calculation: "total" (the default), "rows" or "cols".
#'             Plotting row or column percentages results in stacked bars that add up to 100%.
#'             Whether you set rows or cols determines which variable is in the legend (fill color)
#'             and which on the vertical scale.
#' @param limits The scale limits, autoscaled by default.
#'               Set to \code{c(0,100)} to make a 100 % plot.
#' @param numbers The numbers to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts_one_cor(data, adopter, sd_age)
#'
#' @export
#' @importFrom rlang .data
plot_counts_one_cor <- function(data, col, cross, category = NULL, prop = "total", limits = NULL, ordered = NULL, numbers = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Split into groups
  data <- .tab_split(data, {{ cross }}, labels = labels)

  # 3. Output
  result <- plot_counts_one_grouped(
    data, {{ col }}, {{ cross }},
    category = category, prop = prop, limits = limits, ordered = ordered, numbers = numbers, title = title, labels = labels, clean = clean,
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
#' @param category The value FALSE will force to plot all categories.
#'                  A character value will focus a selected category.
#'                  When NULL, in case of boolean values, only the TRUE category is plotted.
#' @param ordered Values can be nominal (0) or ordered ascending (1) descending (-1).
#'                By default (NULL), the ordering is automatically detected.
#'                An appropriate color scale should be choosen depending on the ordering.
#'                For unordered values, colors from VLKR_FILLDISCRETE are used.
#'                For ordered values, shades of the VLKR_FILLGRADIENT option are used.
#' @param ci Whether to plot error bars for 95% confidence intervals.
#' @param limits The scale limits, autoscaled by default.
#'               Set to \code{c(0,100)} to make a 100 % plot.
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts_items(data, starts_with("cg_adoption_"))
#'
#' @export
#' @importFrom rlang .data
plot_counts_items <- function(data, cols, category = NULL, ordered = NULL, ci = FALSE, limits = NULL, numbers = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Calculate data
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cols_names <- colnames(dplyr::select(data, tidyselect::all_of(cols_eval)))

  # n and p
  result <- data %>%
    labs_clear({{ cols }}) %>%
    tidyr::pivot_longer(
      {{ cols }},
      names_to = "item",
      values_to = "value"
    ) %>%
    dplyr::count(dplyr::across(tidyselect::all_of(c("item", "value")))) %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of("item"))) %>%
    dplyr::mutate(p = (.data$n / sum(.data$n)) * 100) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$value) |>
    dplyr::mutate(value = as.factor(.data$value)) %>%
    dplyr::mutate(item = factor(.data$item, levels=cols_names)) |>
    dplyr::arrange(.data$item)

  # Detect whether the categories are binary
  # TODO: use codebook to determine the order
  categories <- levels(result$value) |> as.character()

  if ((length(categories) == 2) && (is.null(category)) && ("TRUE" %in% categories)) {
    category <- "TRUE"
  }

  # Confidence intervals
  if (ci) {
    result <- result |>
      dplyr::group_by(dplyr::across(tidyselect::all_of("item"))) %>%
      dplyr::mutate(.test = purrr::map(.data$n, function(x) stats::prop.test(x, sum(.data$n)))) |>
      dplyr::mutate(
        ci.low =purrr::map_dbl(.data$.test, function(x) x$conf.int[1]),
        ci.high =purrr::map_dbl(.data$.test, function(x) x$conf.int[2]),
      ) |>
      dplyr::select(-tidyselect::all_of(c(".test"))) |>
      dplyr::ungroup()
  }

  # Numbers on the plot
  scale <- dplyr::coalesce(ordered, get_direction(data, {{ cols }}))
  lastcategory <- ifelse(scale > 0, categories[1], categories[length(categories)])

  result <- result %>%
    dplyr::mutate(value = factor(.data$value, levels = categories)) %>%
    dplyr::mutate(
      .values = dplyr::case_when(
        (is.null(category)) & (scale != 0) & (.data$value == lastcategory) ~ "",
        p < VLKR_LOWPERCENT ~ "",
        all(numbers == "n") ~ as.character(n),
        all(numbers == "p") ~ paste0(round(p, 0), "%"),
        TRUE ~ paste0(n, "\n", round(p, 0), "%")
      )
    )


  # Item and value labels
  if (labels) {
    result <- labs_replace(result, "item", codebook(data, {{ cols }}), "item_name", "item_label")
    result <- labs_replace(result, "value", codebook(data, {{ cols }}))
  }

  # Remove common item prefix
  result <- dplyr::mutate(result, item = trim_prefix(.data$item))

  # Order item levels
  result <- dplyr::mutate(result, item = factor(.data$item, levels=unique(.data$item)))

  # Title
  if (title == TRUE) {
    title <- get_title(data, {{ cols }})
  } else if (title == FALSE) {
    title <- NULL
  }

  # Base
  base_n <- nrow(data)
  result <- .attr_transfer(result, data, "missings")
  .plot_bars(
    result,
    category = category,
    ci = ci,
    scale = scale,
    limits=limits,
    numbers = numbers,
    base = paste0("n=", base_n, "; multiple responses possible"),
    title = title
  )
}

#' Plot percent shares of multiple items compared by groups
#'
#' @keywords internal
#'
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The column holding groups to compare.
#' @param category Summarizing multiple items (the cols parameter) by group requires a focus category.
#'                By default, for logical column types, only TRUE values are counted.
#'                For other column types, the first category is counted.
#'                To override the default behavior, provide a vector of values in the dataset or labels from the codebook.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#' plot_counts_items_grouped(
#'   data, starts_with("cg_adoption_"), adopter,
#'   category=c("agree","strongly agree")
#' )
#'
#' plot_counts_items_grouped(
#'   data, starts_with("cg_adoption_"), adopter,
#'   category=c(4,5)
#' )
#'
#' @export
#' @importFrom rlang .data
plot_counts_items_grouped <- function(data, cols, cross, category = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Calculate data
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cols_names <- colnames(dplyr::select(data, tidyselect::all_of(cols_eval)))

  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)
  cross_names <- colnames(dplyr::select(data, tidyselect::all_of(cross_eval)))

  # Pivot
  result <- data %>%
    labs_clear({{ cols }}) %>%
    tidyr::pivot_longer(
      {{ cols }},
      names_to = "item",
      values_to = ".value_name"
    ) |>
    dplyr::mutate(item = factor(.data$item, levels=cols_names)) |>
    dplyr::arrange(.data$item)

  # Add label column for category
  codebook_df <- codebook(data, {{ cols }})

  result <- result %>%
    dplyr::mutate(
      .value_label = ifelse(
        .data$.value_name %in% codebook_df$value_name,
        codebook_df$value_label[match(.data$.value_name, codebook_df$value_name)],
        as.character(.data$.value_name)
      )
    )

  #  Set labels: cross variable
  if (labels) {
    result <- labs_replace(
      result, {{ cross }},
      codebook(data, {{ cross }}),
      "value_name", "value_label"
    )
  }

  # Focus TRUE category or the first category
  if (is.null(category)) {
    value_names <- unique(as.character(result$.value_name))
    if ((length(value_names) == 2) && ("TRUE" %in% value_names)) {
      base_category <- "TRUE"
    } else {
      base_category <- value_names[1]
    }
  } else {
    base_category <- as.character(category)

    if (
      !all(
        (base_category %in% as.character(result$.value_name)) |
        (base_category %in% result$.value_label)
      )
    ) {
      stop("One or more specified categories do not exist in the data.")
    }
  }

  # Get category labels if names are provided (e.g. for numeric values)
  base_labels <- base_category
  if (is.null(category) || all(base_labels %in% result$.value_name)) {
    base_labels <- codebook_df %>%
      dplyr::filter(.data$value_name %in% base_category) %>%
      dplyr::pull(.data$value_label) %>%
      unique()
  }

  # Recode
  result <- result %>%
    mutate(.category = (.data$.value_name %in% base_category) | (.data$.value_label %in% base_category))

  # Count
  result <- result %>%
    dplyr::mutate(.cross = as.factor({{ cross }})) %>%
    dplyr::mutate(.category = as.factor(.data$.category)) %>%
    dplyr::count(dplyr::across(tidyselect::all_of(c("item", ".cross", ".category")))) %>%
    tidyr::complete(.data$item, .data$.cross, .data$.category, fill=list(n=0))

  # Result p
  result <- result %>%
    dplyr::group_by(dplyr::across(tidyselect::all_of(c("item",".cross")))) %>%
    dplyr::mutate(value = (.data$n / sum(.data$n))) %>%
    dplyr::mutate(value = ifelse(is.na(.data$value), 0, .data$value))

  result <- dplyr::filter(result, .data$.category == TRUE)

  # Factor result
  result <- result %>%
    dplyr::mutate(.cross = as.factor(.data$.cross)) %>%
    dplyr::mutate(item = factor(.data$item, levels=cols_names))

  # Replace item labels
  if (labels) {
    result <- labs_replace(
      result, "item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
    )
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item, trim=TRUE)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix)) %>%
            dplyr::ungroup()

  # Order item levels
  result <- dplyr::mutate(result, item = factor(.data$item, levels=unique(.data$item)))

  # Add title
  if (title == TRUE) {
    title <- trim_label(prefix)
  } else if (title == FALSE) {
    title <- NULL
  }

  # Get base
  base_n <- nrow(data)
  base_labels <- paste0(base_labels, collapse=", ")
  result <- .attr_transfer(result, data, "missings")

  # Plot
  .plot_lines(
    result,
    title = title,
    base = paste0(
      "n=", base_n,
      "; multiple responses possible",
      "; values=", base_labels
    )
  )
}


#' Correlation of categorical items with categorical items
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
#' @return A ggplot object.
#' @importFrom rlang .data
plot_counts_items_grouped_items <- function(data, cols, cross, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Plot percent shares of multiple items compared by a metric variable split into groups
#'
#' @keywords internal
#'
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross A metric column that will be split into groups at the median.
#' @param category Summarizing multiple items (the cols parameter) by group requires a focus category.
#'                By default, for logical column types, only TRUE values are counted.
#'                For other column types, the first category is counted.
#'                To override the default behavior, provide a vector of values in the dataset or labels from the codebook.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts_items_cor(
#'   data, starts_with("cg_adoption_"), sd_age,
#'   category=c("agree","strongly agree")
#' )
#'
#' plot_counts_items_cor(
#'   data, starts_with("cg_adoption_"), sd_age,
#'   category=c(4,5)
#' )
#'
#' @export
#' @importFrom rlang .data
plot_counts_items_cor <- function(data, cols, cross, category = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Split into groups
  data <- .tab_split(data, {{ cross }}, labels = labels)

  # 3. Output
  result <- plot_counts_items_grouped(
    data, {{ cols }}, {{ cross }},
    category = category, title = title, labels = labels, clean = clean,
    ...
  )

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
#' @return A ggplot object.
#' @importFrom rlang .data
plot_counts_items_cor_items <- function(data, cols, cross,  title = TRUE, labels = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Output a density plot for a single metric variable
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param ci Whether to plot the confidence interval.
#' @param box Whether to add a boxplot.
#' @param limits The scale limits. Set NULL to extract limits from the label.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_one(data, sd_age)
#'
#' @export
#' @importFrom rlang .data
plot_metrics_one <- function(data, col, ci = FALSE, box = FALSE, limits = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # Extract the maximum density value
  max_density <- .density_mode(data, {{ col }})

  # TODO: make configurable: density, boxplot or histogram
  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes({{ col }})) +
    ggplot2::geom_density()

  # Boxplot
  if (box) {
    pl <- pl + ggplot2::geom_boxplot(
      ggplot2::aes({{ col }}, y = max_density / 2),
      width = max_density / 3,
      fill="transparent", color=VLKR_COLOR_BOX_FOREGROUND
    )
  }

  # Confidence interval
  if (ci) {
    pl <- pl +
      ggplot2::stat_summary(
        fun.data = get_ci,
        ggplot2::aes(x= {{ col }}, y=max_density / 2),
        width= max_density / 20,
        orientation ="y",
        geom = "errorbar",
        color = VLKR_COLOR_CI
      )
  }

  mean_data <- tibble::tibble(
    x = mean(dplyr::pull(data, {{col}} ), na.rm = TRUE),
    y = max_density / 2
  )

  pl <- pl +

    ggplot2::geom_point(
      ggplot2::aes(x= .data$x, y = .data$y),
      data = mean_data,
      size=4,
      shape=VLKR_POINT_MEAN_SHAPE,
      color = "black"
    )

  # Get title
  if (title == TRUE) {
    title <- get_title(data, {{ col }})
  } else if (title == FALSE) {
    title <- NULL
  }
  if (!is.null(title)) {
    pl <- pl +
      ggplot2::ggtitle(label = title) +
      ggplot2::xlab(title)
  }

  # Set limits
  if (is.null(limits)) {
    limits <- get_limits(data, {{ col }})
  }
  if (!is.null(limits)) {
    pl <- pl +
      ggplot2::coord_cartesian(xlim = limits)
  }

  # Get base
  base_n <- data %>% nrow()
  pl <- pl + ggplot2::labs(caption = paste0("n=", base_n))

  # Pass row number and label length to the knit_plot() function
  # TODO: Don't set rows manually
  pl <- .attr_transfer(pl, data, "missings")
  .to_vlkr_plot(pl, rows=4, theme_options = list(axis.text.y = FALSE))
}

#' Output averages for multiple variables
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param cross The column holding groups to compare.
#' @param ci Whether to add error bars with 95% confidence intervals.
#' @param box Whether to add boxplots.
#' @param limits The scale limits. Set NULL to extract limits from the labels.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_one_grouped(data, sd_age, sd_gender)
#'
#' @export
#' @importFrom rlang .data
plot_metrics_one_grouped <- function(data, col, cross, ci = FALSE, box = FALSE, limits = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # Add n to labels
  # if (!is.null(numbers) && any(numbers == "n")) {
  #
  #   categories_n <- data |>
  #     dplyr::rename(value_name = {{ cross }}) |>
  #     dplyr::count(.data$value_name) |>
  #     dplyr::mutate(value_label = paste0(
  #       wrap_label(.data$value_name, width = 14), "\n",
  #       "(n = ", .data$n, ")")
  #     )
  #
  #   data <- data |>
  #     labs_replace( {{ cross }}, categories_n)
  # }

  # Get scale and limits
  if (is.null(limits)) {
    limits <- get_limits(data, {{ col }})
  }

  scale <- c()
  if (labels) {
    scale <- attr(dplyr::pull(data, {{ col }}), "scale")
    if (is.null(scale)) {
      scale <- data %>%
        codebook({{ col }}) %>%
        dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))
    }
    scale <- prepare_scale(scale)
  }


  # Add title
  if (title == TRUE) {
    title <- get_title(data, {{ col }})
  }
  else if (title == FALSE) {
    title <- NULL
  }

  # Rename
  data <- data %>%
    dplyr::mutate(item = as.factor({{ cross }})) |>
    dplyr::mutate(value = ({{ col }}))


  # Plot
  .plot_summary(
    data,
    scale = scale,
    ci = ci,
    box = box,
    limits = limits,
    base = paste0("n=", nrow(data)),
    title = title
  )

}

#' Correlate two items
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The first column holding metric values.
#' @param cross The second column holding metric values.
#' @param limits The scale limits, a list with x and y components, e.g. \code{list(x=c(0,100), y=c(20,100))}.
#'               Set NULL to extract limits from the labels.
#' @param log Whether to plot log scales.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_one_cor(data, use_private, sd_age)
#'
#' @export
#' @importFrom rlang .data
plot_metrics_one_cor <- function(data, col, cross, limits = NULL, log = FALSE, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Remove 0 values in log plots
  if (log) {
    data <- data_rm_zeros(data, c({{ col }}, {{ cross }}))
  }

  # 3. Get column positions
  cols_eval <- tidyselect::eval_select(expr = rlang::enquo(col), data = data)
  cross_eval <- tidyselect::eval_select(expr = rlang::enquo(cross), data = data)

  # Get first cols
  col1 <- colnames(data)[cols_eval[1]]
  col2 <- colnames(data)[cross_eval[1]]

  # Get variable caption from the attributes
  prefix <- ""
  if (labels) {
    labs <- codebook(data, c({{ col }} , {{ cross }}))  |>
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("item_name", "item_label")))) |>
      dplyr::mutate(item_name = factor(.data$item_name, levels=c(col1, col2))) |>
      dplyr::arrange(.data$item_name) |>
      dplyr::pull(.data$item_label)

    prefix <- get_prefix(labs)
    labs <- trim_prefix(labs, prefix)
  }

  # Order item levels
  # result <- dplyr::mutate(result, item = factor(.data$item, levels=unique(.data$item)))

  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x=.data[[col1]],
      y=.data[[col2]],)
    ) +
    ggplot2::geom_point(size=VLKR_POINT_SIZE, alpha=VLKR_POINT_ALPHA)

  # Scale and limits
  if (log == TRUE) {
    pl <- pl +
      ggplot2::scale_x_log10() +
      ggplot2::scale_y_log10()
  }

  # Set limits
  if (is.null(limits)) {
    limits <- list(
      x = get_limits(data, {{ col }}),
      y = get_limits(data, {{ cross }})
    )
  }

  if (!is.null(limits)) {
    pl <- pl +
      ggplot2::coord_cartesian(xlim = limits$x, ylim = limits$y)
  }

  # Set axis labels
  if (labels) {
    pl <- pl +
      ggplot2::labs(x = labs[1], y = labs[2])
  }

  # Add title
  if (title == TRUE) {
    title <- trim_label(prefix)
  } else if (title == FALSE) {
    title <- NULL
  }
  if (!is.null(title)) {
    pl <- pl + ggplot2::ggtitle(label = title)
  }

  # Add base
  base_n <- nrow(data)
  pl <- pl + ggplot2::labs(caption = paste0("n=", base_n))

  # Convert to vlkr_plot
  pl <- .attr_transfer(pl, data, "missings")
  .to_vlkr_plot(pl, rows=15,  theme_options = list(axis.title.x = TRUE, axis.title.y = TRUE))
}

#' Output averages for multiple variables
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param ci Whether to plot the 95% confidence interval of the mean.
#' @param box Whether to add boxplots.
#' @param limits The scale limits. Set NULL to extract limits from the labels. NOT IMPLEMENTED YET.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_items(data, starts_with("cg_adoption_"))
#'
#' @export
plot_metrics_items <- function(data, cols, ci = FALSE, box = FALSE, limits = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Pivot items
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cols_names <- colnames(dplyr::select(data, tidyselect::all_of(cols_eval)))

  result <- data %>%
    labs_clear({{ cols }}) %>%
    tidyr::pivot_longer(
      {{ cols }},
      names_to = "item",
      values_to = "value"
    ) |>
    dplyr::mutate(item = factor(.data$item, levels=cols_names)) |>
    dplyr::arrange(.data$item)

  # Replace item labels
  if (labels) {
    result <- labs_replace(
      result, "item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
    )
  }

  if (is.null(limits)) {
    limits <- get_limits(data, {{ cols }})
  }

  # Remove common item prefix and title
  # TODO: remove common postfix
  result <- dplyr::mutate(result, item = trim_prefix(.data$item))

  # Order item levels
  result <- dplyr::mutate(result, item = factor(.data$item, levels=unique(.data$item)))

  # Add scale labels
  scale <- c()
  if (labels) {
    scale <- data %>%
      codebook({{ cols }}) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label")))) %>%
      prepare_scale()
  }

  # Add title
  if (title == TRUE) {
    title <- get_title(data, {{ cols }})
  }
  else if (title == FALSE) {
    title <- NULL
  }


  # Add base
  base_n <- nrow(data)
  result <- .attr_transfer(result, data, "missings")
  .plot_summary(
    result,
    ci = ci,
    box = box,
    scale = scale,
    limits = limits,
    base = paste0("n=", base_n, "; multiple responses possible"),
    title = title
  )
}


#' Output averages for multiple variables compared by a grouping variable
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The column holding groups to compare.
#' @param limits The scale limits. Set NULL to extract limits from the labels.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_items_grouped(data, starts_with("cg_adoption_"), sd_gender)
#'
#' @export
#' @importFrom rlang .data
plot_metrics_items_grouped <- function(data, cols, cross, limits = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Calculate
  # Get positions of group cols
  cross <- tidyselect::eval_select(expr = rlang::enquo(cross), data = data)

  # Grouped means
  result <- purrr::map(
    cross,
    function(col) {
      col <- names(data)[col]

      data %>%
        dplyr::filter(!is.na(!!sym(col))) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::select(!!sym(col), {{ cols }}) %>%
        skim_metrics() %>%
        dplyr::ungroup() %>%
        dplyr::select(item = "skim_variable", .cross = !!sym(col), value = "numeric.mean") %>%
        tidyr::drop_na()
    }
  ) %>%
    purrr::reduce(
      dplyr::bind_rows
    )

  # Get limits
  if (is.null(limits)) {
    limits <- get_limits(data, {{ cols }})
  }

  # Replace item labels
  if (labels) {
    result <- labs_replace(
      result, "item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
      )
  }

  # Remove common item prefix
  prefix <- get_prefix(result$item, trim=TRUE)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))

  # Order item levels
  result <- dplyr::mutate(result, item = factor(.data$item, levels=unique(.data$item)))

  # Set the scale
  # TODO: get from attributes
  scale <- data %>%
    codebook({{ cols }}) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label")))) %>%
    prepare_scale()

  # Add title
  if (title == TRUE) {
    title <- trim_label(prefix)
  } else if (title == FALSE) {
    title <- NULL
  }

  # Get base
  base_n <- nrow(data)
  result <- .attr_transfer(result, data, "missings")

  # Plot
  .plot_lines(
    result,
    scale = scale,
    title = title,
    limits = limits,
    base = paste0("n=", base_n, "; multiple responses possible")

  )
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
#' @return A ggplot object.
#' @importFrom rlang .data
plot_metrics_items_grouped_items <- function(data, cols, cross, title = TRUE, labels = TRUE, clean = TRUE, ...) {
    warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Multiple items correlated with one metric variable
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The column to correlate.
#' @param ci Whether to plot confidence intervals of the correlation coefficent.
#' @param method The method of correlation calculation, pearson = Pearson's R, spearman = Spearman's rho.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_items_cor(data, starts_with("use_"), sd_age)
#'
#'@export
#'@importFrom rlang .data
plot_metrics_items_cor <- function(data, cols, cross, ci = FALSE, method = "pearson", title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Calculate correlations
  result <- .effect_correlations(data, {{ cols }}, {{ cross }}, method = method, labels = labels)

  # Remove common item prefix
  prefix1 <- get_prefix(result$item1)
  prefix2 <- get_title(data, {{ cross }})

  result <- dplyr::mutate(
    result,
    item1 = trim_prefix(.data$item1, prefix1)
  )

  # Adjust result for plot
  if (method == "pearson") {

    value_label <- "Pearson's r"

    result <- result %>%
      dplyr::rename(
        item = "item1",
        item2 = "item2",
        value = "Pearson's r",
        low = "ci low",
        high = "ci high"
      )


  } else if (method == "spearman") {

    value_label <- "Spearman's rho"

    result <- result %>%
      dplyr::rename(
        item = "item1",
        item2 = "item2",
        value = "Spearman's rho"
      )
  }

  # Get limits
  method_range <- range(result$value, na.rm = TRUE)

  if (all(method_range >= 0 & method_range <= 1)) {
    limits <- c(0, 1)
  } else {
    limits <- c(-1, 1)
  }

  # Add title
  if (title == TRUE) {
    if (prefix1 != prefix2) {
      title <- paste0(prefix1, " - ", prefix2)
    } else {
      title <- prefix1
    }
  } else if (title == FALSE) {
    title <- NULL
  }

  # Add base
  base_n <- nrow(data)

  # Missings
  result <- .attr_transfer(result, data, "missings")

  .plot_cor(
    result,
    ci = ci,
    limits = limits,
    base = paste0("n=", base_n),
    title = title,
    label = value_label
  )

}

#' Heatmap for correlations between multiple items
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross Tidyselect item variables to correlate (e.g. starts_with...).
#' @param method The method of correlation calculation, pearson = Pearson's R, spearman = Spearman's rho.
#' @param numbers Controls whether to display correlation coefficients on the plot.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_items_cor_items(data, starts_with("cg_adoption_adv"), starts_with("use_"))
#'
#'@export
#'@importFrom rlang .data
plot_metrics_items_cor_items <- function(data, cols, cross, method = "pearson", numbers = FALSE, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, clean = clean)

  if (nrow(data) == 0) {
    return(NULL)
  }

  # 2. Calculate correlation
  result <- .effect_correlations(data, {{ cols }}, {{ cross }}, method = method, labels = labels)

  # Remove common item prefix
  prefix1 <- get_prefix(result$item1)
  prefix2 <- get_prefix(result$item2)

  result <- dplyr::mutate(
    result,
    item1 = trim_prefix(.data$item1, prefix1),
    item2 = trim_prefix(.data$item2, prefix2)
  )

  value_col <- ifelse(method=="spearman", "Spearman's rho", "Pearson's r")
  value_range <- range(result[[value_col]], na.rm = TRUE)

  # Plot
  pl <- result %>%
    ggplot2::ggplot(ggplot2::aes(
      y = .data$item1,
      x = .data$item2,
      fill = !!sym(value_col)
      )
    ) +
    ggplot2::geom_tile()

  if (all(value_range >= 0 & value_range <= 1)) {
    # range 0 to 1
    pl <- pl + ggplot2::scale_fill_gradientn(
      colors = vlkr_colors_sequential(),
      limits = c(0,1))

  } else {

    # range -1 to 1
    pl <- pl + ggplot2::scale_fill_gradientn(
      colors = vlkr_colors_polarized(),
      limits = c(-1,1)
    )
  }

  if (numbers) {
    pl <- pl +
      ggplot2::geom_text(
        ggplot2::aes(label = !!sym(value_col), color = !!sym(value_col)),
        size = 3,
      ) +
      ggplot2::scale_color_gradientn(
        colors = VLKR_COLORPOLARIZED,
        limits = c(-1,1),
        guide = "none"
      )
  }

  # Add labels
  if (labels)
    pl <- pl + ggplot2::labs(
      x = prefix1,
      y = prefix2
    )

  # Add title
  if (title == TRUE) {
    if (prefix1 != prefix2) {
      title <- paste0(prefix1, " - ", prefix2)
    } else {
      title <- prefix1
    }
  } else if (title == FALSE) {
    title <- NULL
  }

  if (!is.null(title)) {
    pl <- pl + ggplot2::ggtitle(label = title)
  }

  # Wrap labels and adjust text size
  pl <- pl +
    ggplot2::scale_y_discrete(
      labels = scales::label_wrap(dplyr::coalesce(getOption("vlkr.wrap.labels"), VLKR_PLOT_LABELWRAP))
    ) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(
        angle = get_angle(result$item1),
        hjust = 1,
        size = ggplot2::theme_get()$axis.text.y$size,
        color = ggplot2::theme_get()$axis.text.y$color
      )
    ) +
    ggplot2::scale_x_discrete(labels = \(labels) trunc_labels(labels)) +
    ggplot2::coord_fixed()

  # Add base
  base_n <- nrow(data)
  pl <- pl + ggplot2::labs(caption = paste0("n=", base_n))

  # Convert to vlkr_plot
  pl <- .attr_transfer(pl, data, "missings")
  .to_vlkr_plot(pl, rows = dplyr::n_distinct(result$item1) + 3)

}


#' Helper function: plot grouped bar chart
#'
#' @keywords internal
#'
#' @param data Dataframe with the columns item, value, p, n.
#' @param category Category for filtering the dataframe.
#' @param ci Whether to plot error bars for 95% confidence intervals. Provide the columns ci.low and ci.high in data.
#' @param scale Direction of the scale: 0 = no direction for categories,
#'              -1 = descending or 1 = ascending values.
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title The plot title as character or NULL.
#' @param base The plot base as character or NULL.
#' @return A ggplot object.
#' @importFrom rlang .data
.plot_bars <- function(data, category = NULL, ci = FALSE, scale = NULL, limits = NULL, numbers = NULL, base = NULL, title = NULL) {

  data_missings <- attr(data, "missings", exact = TRUE)

  if (!is.null(category)) {
    data <- dplyr::filter(data, .data$value == category)
  }

  if (all(data$item == "TRUE") && is.null(limits)) {
    limits <- c(0, 1)
  }

  if (scale <= 0) {
    data <- data %>%
      dplyr::mutate(
        value = factor(.data$value, levels = rev(levels(.data$value)))
      )
  }

  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(x = .data$item, y = .data$p / 100, fill = .data$value, group = .data$value)) +
    ggplot2::geom_col()

  if (ci && !is.null(category)) {
    pl <- pl +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$ci.low, ymax = .data$ci.high),
        width = 0.1, color = VLKR_COLOR_CI
      )
  }

  pl <- pl +
    ggplot2::scale_y_continuous(labels = scales::percent, limits = limits) +
    ggplot2::scale_x_discrete(
      labels = scales::label_wrap(dplyr::coalesce(getOption("vlkr.wrap.labels"), VLKR_PLOT_LABELWRAP)),
      limits = rev
    ) +
    ggplot2::ylab("Share in percent") +
    ggplot2::coord_flip()

  # Select scales:
  # - Simplify binary plots
  # - Generate a color scale for ordinal scales by vlkr_colors_sequential()
  # - Use vlkr_colors_discrete() for other cases

  if (!is.null(category)) {
    pl <- pl +
      ggplot2::scale_fill_manual(
        values = vlkr_colors_discrete(1)
      ) +
      ggplot2::theme(
        legend.position = ifelse(category == "TRUE" | category == TRUE, "none","bottom"),
        legend.justification = "left"
      )
  } else if ((scale > 0) || (scale < 0)) {
    pl <- pl +
      ggplot2::scale_fill_manual(
        values = vlkr_colors_sequential(length(levels(as.factor(data$value)))),
        guide = ggplot2::guide_legend(reverse = TRUE)
      )
  } else {
    pl <- pl +
      ggplot2::scale_fill_manual(
        values = vlkr_colors_discrete(length(levels(as.factor(data$value)))),
        guide = ggplot2::guide_legend(reverse = TRUE)
      )
  }

  # Add numbers
  if (!is.null(numbers)) {
    pl <- pl +
      ggplot2::geom_text(ggplot2::aes(label = .data$.values), position = ggplot2::position_stack(vjust = 0.5), color = "white") #size = 3
  }

  # Add title
  if (!is.null(title)) {
    pl <- pl + ggplot2::ggtitle(label = title)
  }

  # Add base
  if (!is.null(base)) {
    pl <- pl + ggplot2::labs(caption = base)
  }

  # Convert to vlkr_plot
  attr(pl, "missings") <- data_missings

  .to_vlkr_plot(pl, theme_options = list(axis.text.y = !all(data$item == "TRUE")))
}


#' Helper function: plot grouped line chart by summarising values
#'
#' @keywords internal
#'
#' @param data Dataframe with the columns item, value.
#' @param ci Whether to plot confidence intervals of the means.
#' @param scale Passed to the label scale function.
#' @param box Whether to add boxplots.
#' @param title The plot title as character or NULL.
#' @param base The plot base as character or NULL.
#' @return A ggplot object.
#' @importFrom rlang .data
.plot_summary <- function(data, ci = FALSE, scale = NULL, base = NULL, box = FALSE, limits = NULL, title = NULL) {

  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(y=.data$item, x=.data$value, group=1))

  if (box) {
    pl <- pl + ggplot2::geom_boxplot(
      ggplot2::aes(group=.data$item),
      fill="transparent", color=VLKR_COLOR_BOX_BACKGROUND
    )
  }

  if (ci) {
    pl <- pl +
      ggplot2::stat_summary(
      geom = "errorbar",
      fun.data = get_ci,
      orientation ="y",
      width=0.2,
      #linewidth=1,
      colour = VLKR_COLOR_CI
    )
  }

  if (!box && !ci) {
    pl <- pl +
      ggplot2::stat_summary(fun = mean, geom="line")
  }

  pl <- pl + ggplot2::stat_summary(fun = mean, geom="point", size=4, shape=18)


  # Add limits
  if (!is.null(limits)) {
    pl <- pl +
      ggplot2::coord_cartesian(xlim = limits)
  }

  # Add scales
  if (length(scale) > 0) {
    pl <- pl +
      ggplot2::scale_x_continuous(labels = ~ label_scale(., scale) )
  } else {
    pl <- pl +
      ggplot2::scale_x_continuous()
  }

  pl <- pl +
    ggplot2::scale_y_discrete(
      labels = scales::label_wrap(dplyr::coalesce(getOption("vlkr.wrap.labels"), VLKR_PLOT_LABELWRAP)),
      limits = rev
    )

  #
  #   if (!is.null(numbers)) {
  #     pl <- pl +
  #       geom_text(
  #         # aes(label=paste0("⌀", round(m,1))),
  #         aes(label = round(m, 1)),
  #         #position = position_stack(vjust = 0.5),
  #         hjust = -1,
  #         vjust=0.5,
  #         size = 3,
  #         color = "black"
  #       )
  #   }

  # Add title
  if (!is.null(title)) {
    pl <- pl + ggplot2::ggtitle(label = title)
  }

  # Add base
  if (!is.null(base)) {
    pl <- pl + ggplot2::labs(caption = base)
  }

  # Maximum label length
  maxlab  <- data %>%
    dplyr::pull(.data$item) |>
    as.character() |>
    nchar() |>
    max(na.rm= TRUE)

  # Convert to vlkr_plot
  # Pass row number and label length to the knit_plot() function
  pl <- .attr_transfer(pl, data, "missings")
  .to_vlkr_plot(pl, maxlab=maxlab)
}

#' Helper function: plot grouped line chart
#'
#' @keywords internal
#'
#' @param data Dataframe with the columns item, value, and .cross
#' @param scale Passed to the label scale function.
#' @param base The plot base as character or NULL.
#' @param limits The scale limits.
#' @param title The plot title as character or NULL.
#' @return A ggplot object.
#' @importFrom rlang .data
.plot_lines <- function(data, scale = NULL, base = NULL, limits = NULL, title = NULL) {

  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$item,
      y = .data$value,
      color = .data$.cross,
      group = .data$.cross
    )
    ) +
    ggplot2::geom_point(size=VLKR_POINT_SIZE, shape=VLKR_POINT_MEAN_SHAPE) +
    ggplot2::geom_line(alpha = VLKR_LINE_ALPHA)

  # Set scale
  if (!is.null(scale)) {
    if (length(scale) > 0) {
      pl <- pl +
        ggplot2::scale_y_continuous(labels = ~ label_scale(., scale))
    } else {
      pl <- pl +
        ggplot2::scale_y_continuous()
    }
  } else {
    # scale is NULL
    pl <- pl +
      ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 100))
  }

  # Add scales
  pl <- pl +
    ggplot2::scale_x_discrete(
      labels = scales::label_wrap(dplyr::coalesce(getOption("vlkr.wrap.labels"), VLKR_PLOT_LABELWRAP)),
      limits=rev
      ) +
    ggplot2::scale_color_manual(
      values = vlkr_colors_discrete(length(unique(data$.cross))),
      labels = function(x) wrap_label(x, width = dplyr::coalesce(getOption("vlkr.wrap.legend"), VLKR_PLOT_LEGENDWRAP))
    )

  # Limits
  if (!is.null(limits)){
      pl <- pl +
        ggplot2::coord_flip(ylim = limits)
  }
  else {
    pl <- pl +
      ggplot2::coord_flip(ylim = c(0,1))
  }

  # Add title
  if (!is.null(title)) {
    pl <- pl + ggplot2::ggtitle(label = title)
  }

  # Add base
  if (!is.null(base)) {
    pl <- pl + ggplot2::labs(caption = base)
  }

  # Convert to vlkr_plot
  pl <- .attr_transfer(pl, data, "missings")
  .to_vlkr_plot(pl)
}


#' Helper function: plot cor and regression outputs
#'
#' @keywords internal
#'
#' @param data Dataframe with the columns item and value.
#'             To plot errorbars, add the columns low and high and set the ci-paramater to TRUE.
#' @param ci Whether to plot confidence intervals. Provide the columns low and high in data.
#' @param base The plot base as character or NULL.
#' @param limits The scale limits.
#' @param title The plot title as character or NULL.
#' @param label The y axis label.
#' @return A ggplot object.
#' @importFrom rlang .data
.plot_cor <- function(data, ci = TRUE, base = NULL, limits = NULL, title = NULL, label = NULL) {

  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data$item,
      y = .data$value
    )
  ) +
    ggplot2::geom_point(
      size=VLKR_POINT_SIZE,
      shape=VLKR_POINT_COR_SHAPE,
      color = vlkr_colors_discrete(1)
      )

  if (ci && ("low" %in% colnames(data)) && ("low" %in% colnames(data))) {
    pl <- pl +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$low, ymax = .data$high),
        orientation ="x",
        width=0.2,
        colour = VLKR_COLOR_CI)
  }

  # Limits
  if (!is.null(limits)){
    pl <- pl +
      ggplot2::coord_flip(ylim = limits)
  }

  # Add title
  if (!is.null(title)) {
    pl <- pl + ggplot2::ggtitle(label = title)
  }

  pl <- pl +
    ggplot2::theme(legend.position = "none") +
    ggplot2::labs(y = label)

  # Add base
  if (!is.null(base)) {
    pl <- pl + ggplot2::labs(caption = base)
  }

  # Convert to vlkr_plot
  pl <- .attr_transfer(pl, data, "missings")
  .to_vlkr_plot(pl, theme_options = list(axis.title.x = TRUE))

}

#' Helper function: scree plot
#'
#' @keywords internal
#'
#' @param data Dataframe with the factor or cluster number in the first column
#'             and the metric in the second.
#' @param k Provide one of the values in the first column to color points up to this value.
#' @param lab_x Label of the x axis
#' @param lab_y Label of the y axis
#' @return A vlkr_plot object
#' @importFrom rlang .data
.plot_scree <- function(data, k = NULL, lab_x = NULL, lab_y = NULL) {

  data$selected <- data[[1]] <= k

  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(
      x = .data[[colnames(data)[1]]],
      y = .data[[colnames(data)[2]]]
    )) +
    ggplot2::geom_line(alpha = VLKR_LINE_ALPHA, color = VLKR_COLOR_DISABLED) +
    ggplot2::geom_point(ggplot2::aes(color = .data$selected), size=VLKR_POINT_SIZE, shape=VLKR_POINT_MEAN_SHAPE) +
    ggplot2::scale_color_manual(values = c(VLKR_COLOR_DISABLED, vlkr_colors_discrete(1)), guide = "none") +
    ggplot2::scale_x_continuous(breaks = data[[1]]) +
    ggplot2::ggtitle(label = "Scree plot") +
    ggplot2::labs(x = lab_x, y = lab_y)

  .to_vlkr_plot(
    pl, rows=10,
    theme_options = list("axis.title.x" = TRUE, "axis.title.y" = TRUE)
  )
}

#' Get the maximum density value in a density plot
#'
#' Useful for placing geoms in the center of density plots
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col A tidyselect column.
#' @return The maximum density value.
.density_mode <- function(data, col) {
  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes({{ col }})) +
    ggplot2::geom_density() +
    ggplot2::theme_void()

  ggplot2::ggplot_build(pl)$data[[1]]$y %>% max()
}

#' Add the volker class and options
#'
#' @keywords internal
#'
#' @param pl A ggplot object.
#' @param rows The number of items on the vertical axis. Will be automatically determined when NULL.
#'             For stacked bar charts, don't forget to set the group parameter, otherwise it won't work
#' @param maxlab The character length of the longest label to be plotted. Will be automatically determined when NULL.
#'               on the vertical axis.
#' @param baseline Whether to print a message about removed values.
#' @param theme_options Enable or disable axis titles and text, by providing a list with any of the elements
#'                      axis.text.x, axis.text.y, axis.title.x, axis.title.y set to TRUE or FALSE.
#'                      By default, titles (=scale labels) are disabled and text (= the tick labels) are enabled.
#' @return A ggplot object with vlkr_plt class.
.to_vlkr_plot <- function(pl, rows = NULL, maxlab = NULL, baseline = TRUE, theme_options = TRUE) {

  # Add vlkr_plt class
  class(pl) <- c("vlkr_plt", class(pl))

  # Apply standard theme
  if (!is.null(theme_options)) {

    theme_args <- list(
      legend.title = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )


    # TODO: simplify
    if (isTRUE(theme_options)) {
      theme_options <- list()
    }
    theme_options <- utils::modifyList(list(axis.text.x = TRUE, axis.text.y = TRUE, axis.title.x = FALSE, axis.title.y = FALSE), theme_options)


    if (isFALSE(theme_options$axis.title.x)) {
      theme_args$axis.title.x <-  ggplot2::element_blank()
    }
    if (isFALSE(theme_options$axis.title.y)) {
      theme_args$axis.title.y <-  ggplot2::element_blank()
    }
    if (isFALSE(theme_options$axis.text.x)) {
      theme_args$axis.text.x <-  ggplot2::element_blank()
    }
    if (isFALSE(theme_options$axis.text.y)) {
      theme_args$axis.text.y <-  ggplot2::element_blank()
    }

    pl <- pl + do.call(ggplot2::theme, theme_args)

  }

  # Calculate rows and label lengths
  plot_data <- ggplot2::ggplot_build(pl)
  if (is.null(rows)) {
    if ("CoordFlip" %in% class(pl$coordinates)) {
      labels <- plot_data$layout$panel_scales_x[[1]]$range$range
    } else {
      labels <- plot_data$layout$panel_scales_y[[1]]$range$range
    }
    legend <- unique(plot_data$data[[1]]$group)

    rows <- max(length(labels), (length(legend) / 3) * 2)
  }

  if (is.null(maxlab)) {
    labels <- plot_data$layout$panel_scales_x[[1]]$range$range
    #labels <- layer_scales(pl)$x$range$range
    #labels <- pl$data[[1]]
    maxlab <- max(nchar(as.character(labels)), na.rm= TRUE)

  }

  attr(pl, "vlkr_options") <- list(
    rows = rows,
    maxlab = maxlab
  )

  if (baseline == TRUE) {
    baseline <- get_baseline(pl)
  } else if (baseline == FALSE) {
    baseline <- NULL
  }

  if (!is.null(baseline)) {
    attr(pl, "baseline") <- baseline
  }

  pl
}

#' Get plot size and resolution for the current output format from the config
#'
#' @keywords internal
#'
#' @return A list with figure settings
.get_fig_settings <- function() {
  default <- utils::modifyList(VLKR_FIG_SETTINGS, getOption("vlkr.fig.settings", list()))
  result <- default[[1]]

  # Select by format
  format <- ifelse(knitr::is_html_output(), 'html', knitr::pandoc_to())
  if (!is.null(default[[format]])) {
    result <- default[[format]]
  }

  # Override width with chunk options
  chunk_options <- knitr::opts_current$get()
  if (!is.null(chunk_options$vlkr.fig.width)) {
     result$width <- as.numeric(chunk_options$vlkr.fig.width)
  }

  result
}

#' Knit volker plots
#'
#' Automatically calculates the plot height from
#' chunk options and volker options.
#'
#' Presumptions:
#' - a screen resolution of 72dpi
#' - a default plot width of 7 inches = 504px
#' - a default page width of 700px (vignette) or 910px (report)
#' - an optimal bar height of 40px for 910px wide plots. i.e. a ratio of 0.04
#' - an offset of one bar above and one bar below
#'
#' @keywords internal
#'
#' @param pl A ggplot object with vlkr_options.
#'           The vlk_options are added by .to_vlkr_plot()
#'           and provide information about the number of vertical items (rows)
#'           and the maximum.
#' @return Character string containing a html image tag, including the base64 encoded image.
.knit_plot <- function(pl) {

  # Get plot options
  plot_options <- attr(pl, "vlkr_options", exact = TRUE)
  baseline <- attr(pl, "baseline", exact = TRUE)

  # Get size and resolution settings
  fig_settings <- .get_fig_settings()
  fig_dpi <- fig_settings$dpi
  fig_scale <- fig_settings$scale
  fig_width <- fig_settings$width

  # Calculate plot height
  rows <- dplyr::coalesce(plot_options[["rows"]], 10)
  px_perline <- fig_settings$pxperline

  # Buffer above and below the diagram
  px_offset <- VLKR_PLOT_OFFSETROWS * px_perline
  if (!is.null(pl$labels$title)) {
    px_offset <- px_offset + VLKR_PLOT_TITLEROWS * px_perline
  }

  lines_wrap <- dplyr::coalesce(plot_options[["labwrap"]], getOption("vlkr.wrap.labels"), VLKR_PLOT_LABELWRAP)
  lines_perrow <- (dplyr::coalesce(plot_options[["maxlab"]], 1) %/% lines_wrap) + 2

  fig_height <- (rows * lines_perrow * px_perline) + px_offset


  # if (length(dev.list()) > 0) {
  #   dev.off()
  # }

  #pngfile <- tempfile(fileext = ".png", tmpdir = chunk_options$cache.path)
  pngfile <- tempfile(fileext = ".png")
  suppressMessages(ggplot2::ggsave(
    pngfile,
    pl,
    # type = "cairo-png",
    # antialias = "subpixel",
    width = fig_width,
    height = fig_height,
    units = "px",
    dpi = fig_dpi,
    scale = fig_scale,
    dev = "png"
  ))
  #dev.off()

  if (knitr::is_html_output()) {
    base64_string <- base64enc::base64encode(pngfile)
    result <- paste0('<img src="data:image/png;base64,', base64_string, '" width="100%">')
  } else {
    result <- paste0('![](', pngfile,')<!-- -->')
  }

  if (!is.null(baseline)) {
    attr(result, "baseline") <- baseline
  }
  result
}



#' Printing method for volker plots
#'
#' @keywords internal
#'
#' @param x The volker plot.
#' @param ... Further parameters passed to print().
#' @return No return value.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' pl <- plot_metrics(data, sd_age)
#' print(pl)
#'
#' @export
print.vlkr_plt <- function(x, ...) {

  baseline <- attr(x, "baseline", exact = TRUE)
  NextMethod()

  if (!is.null(baseline)) {
    message(paste0("In the plot, ", baseline))
  }
}

#' Printing method for volker plots when knitting
#'
#' @keywords internal
#'
#' @param x The volker plot.
#' @param ... Further parameters passed to print().
#' @return Knitr asis output
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' pl <- plot_metrics(data, sd_age)
#' print(pl)
#'
#' @importFrom knitr knit_print
#' @method knit_print vlkr_plt
#' @export
knit_print.vlkr_plt <- function(x, ...) {
  x <- .knit_plot(x)
  knitr::asis_output(x)
}

#' @rdname print.vlkr_plt
#' @method plot vlkr_plt
#' @keywords internal
#' @export
plot.vlkr_plt <- print.vlkr_plt

#' Define a default theme for volker plots
#'
#' Set ggplot colors, sizes and layout parameters.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @importFrom ggplot2 '%+replace%'
#' @param base_size Base font size.
#' @param base_color Base font color.
#' @param base_fill A list of fill color sets or at least one fill color set. Example:
#'                  \code{list(c("red"), c("red", "blue", "green"))}.
#'                  Each set can contain different numbers of colors.
#'                  Depending on the number of colors needed,
#'                  the set with at least the number of required colors is used.
#'                  The first color is always used for simple bar charts.
#' @param base_gradient A color vector used for creating gradient fill colors, e.g. in stacked bar plots.
#' @return A theme function.
#' @examples
#' library(volker)
#' library(ggplot2)
#' data <- volker::chatgpt
#'
#' theme_set(theme_vlkr(base_size=15, base_fill = list("red")))
#' plot_counts(data, sd_gender)
#' @export
theme_vlkr <- function(base_size=11, base_color="black", base_fill = VLKR_FILLDISCRETE, base_gradient = VLKR_FILLGRADIENT) {
  if (!is.list(base_fill)) {
    base_fill <- list(base_fill)
  }
  base_fill_one <- base_fill[[1]][1]
  ggplot2::update_geom_defaults("text", list(size = (base_size-2) / ggplot2::.pt, color=base_color))
  ggplot2::update_geom_defaults("col",   list(fill = base_fill_one))
  ggplot2::update_geom_defaults("density",   list(fill = base_fill_one))
  ggplot2::update_geom_defaults("point",   list(color = base_fill_one))
  ggplot2::update_geom_defaults("line",   list(color = base_fill_one))

  options(ggplot2.discrete.fill=base_fill)

  options(vlkr.gradient.fill=base_gradient)
  options(vlkr.discrete.fill=base_fill)

  ggplot2::theme_bw(base_size) %+replace%

    ggplot2::theme(
      axis.ticks.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(
        size=base_size,
        color=base_color,
        hjust=1,
        margin = ggplot2::margin(r = 0.8 * base_size/4)
      ),
      legend.text = ggplot2::element_text(size=base_size-2)
    )
}

#' Get colors for discrete scales
#'
#' If the option ggplot2.discrete.fill is set,
#' gets color values from the first list item that
#' has enough colors and reverses them to start filling
#' from the left in grouped bar charts.
#'
#' Falls back to scale_fill_hue().
#'
#' @keywords internal
#'
#' @param n Number of colors.
#' @return A vector of colors.
vlkr_colors_discrete <- function(n) {
  colors <- getOption("vlkr.discrete.fill")
  if (is.null(colors)) {
    colors <- VLKR_FILLDISCRETE
  }

  colors <- colors[sapply(colors, length) >= n]
  if (length(colors) == 0) {
    colors <- ggplot2::scale_fill_hue()$palette(n)
  } else {
    colors <- rev(colors[[1]][1:n])
  }


  colors
}

#' Get colors for sequential scales
#'
#' Creates a gradient scale based on VLKR_FILLGRADIENT.
#'
#' @keywords internal
#'
#' @param n Number of colors or NULL to get the raw colors from the config
#' @return A vector of colors.
vlkr_colors_sequential <- function(n = NULL) {
  colors <- getOption("vlkr.gradient.fill")
  if (is.null(colors)) {
    colors <- VLKR_FILLGRADIENT
  }


  if (!is.null(n)) {
    colors <- scales::gradient_n_pal(colors)(
      seq(0,1,length.out=n)
    )
  }
  colors
}

#' Get colors for polarized scales
#'
#' Creates a gradient scale based on VLKR_FILLPOLARIZED.
#'
#' @keywords internal
#'
#' @param n Number of colors or NULL to get the raw colors from the config
#' @return A vector of colors.
vlkr_colors_polarized <- function(n = NULL) {
  colors <- getOption("vlkr.polarized.fill")
  if (is.null(colors)) {
    colors <- VLKR_FILLPOLARIZED
  }

  if (!is.null(n)) {
    colors <- scales::gradient_n_pal(colors)(
      seq(0,1,length.out=n)
    )
  }
  colors
}
