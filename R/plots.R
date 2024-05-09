#' Output a frequency plot
#'
#' @description
#' The type of frequency plot depends on the number of selected columns:
#' - One column: see \link{plot_counts_one}
#' - Multiple columns: see \link{plot_counts_items}
#' - One column and one grouping column: see \link{plot_counts_one_grouped}
#' - Multiple columns and one grouping column: see \link{plot_counts_items_grouped} (not yet implemented)
#'
#' By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - One column and one metric column: see \link{plot_counts_one_cor} (not yet implemented)
#' - Multiple columns and one metric column: see \link{plot_counts_items_cor} (not yet implemented)
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
  if (!is_items && !(is_grouped || is_multi)) {
    plot_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    plot_counts_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }

  else if (!is_items && is_grouped && is_metric) {
    plot_counts_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !(is_grouped || is_multi)) {
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
#' - One column: see \link{plot_metrics_one}
#' - Multiple columns: see \link{plot_metrics_items}
#' - One column and one grouping column: see \link{plot_metrics_one_grouped}
#' - Multiple columns and one grouping column: see \link{plot_metrics_items_grouped}
#'
#' By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - Two metric columns: see \link{plot_metrics_one_cor}
#' - Multiple columns: see \link{plot_metrics_items_cor} (not yet implemented)
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
  if (!is_items && !(is_grouped ||is_multi) && !is_metric) {
    plot_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    plot_metrics_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }
  else if (!is_items && is_grouped && is_metric) {
    plot_metrics_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !(is_grouped || is_multi) && !is_metric) {
    plot_metrics_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_metric) {
    plot_metrics_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && (is_grouped || is_multi) && is_metric) {
    plot_metrics_items_cor(data, {{ cols }}, {{ cross }},  ...)
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

  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, {{ col }})

  # 4. Data
  # Count data
  result <- data %>%
    dplyr::count({{ col }}) %>%
    dplyr::mutate(p = (.data$n / sum(.data$n)) * 100)

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

  # Numbers
  result <- result %>%
    dplyr::mutate(
      .values = dplyr::case_when(
        .data$p < VLKR_LOWPERCENT ~ "",
        all(numbers == "n") ~ as.character(.data$n),
        all(numbers == "p") ~ paste0(round(.data$p, 0), "%"),
        TRUE ~ paste0(.data$n, "\n", round(.data$p, 0), "%")
      )
    )

  # Item labels
  result <- result |>
    dplyr::mutate( "{{ col }}" := as.factor({{ col }}))

  if (labels) {
    result <- labs_replace(result, {{ col }}, codebook(data, {{ col }}))
  }

  # Detect the scale (whether the categories are binary and direction)
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

  # Title
  if (title == TRUE) {
    title <- get_title(data, {{ col }})
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

  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Swap columns
  if (prop == "cols") {
    col <- rlang::enquo(col)
    cross <- rlang::enquo(cross)
    col_temp <- col
    col <- cross
    cross <- col_temp
    #stop("To display column proportions, swap the first and the grouping column. Then set the prop parameter to \"rows\".")
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ col }}, {{ cross }}))

  # 4. Calculate data
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

#' Plot correlation of categories with one metric column
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param cross The metric column
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object.
#' @importFrom rlang .data
plot_counts_one_cor <- function(data, col, cross, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
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
  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}))

  # 4. Calculate data

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
    dplyr::mutate(value = as.factor(.data$value)) %>%
    dplyr::mutate(item = factor(.data$item, levels=cols_names)) |>
    dplyr::arrange(.data$item)

  # Detect whether the categories are binary
  categories <- result$value |> unique() |> as.character()
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

  # Item labels
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

#' Plot frequencies of multiple items compared by groups
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols The item columns that hold the values to summarize.
#' @param cross The column holding groups to compare.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object.
#' @importFrom rlang .data
plot_counts_items_grouped <- function(data, cols, cross, title = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Correlate categorical items with a metric column
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols The item columns that hold the values to summarize.
#' @param cross The column holding metric values to correlate.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object.
#' @importFrom rlang .data
plot_counts_items_cor <- function(data, cols, cross, title = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Output a density plot for a single metric variable
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
plot_metrics_one <- function(data, col, negative = FALSE, ci = FALSE, box = FALSE, limits = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {

  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col }})

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
    x = mean(dplyr::pull(data, {{col}} ), na.rm=T),
    y = max_density / 2
  )

  pl <- pl +

    ggplot2::geom_point(
      ggplot2::aes(x= .data$x, y = .data$y),
      data = mean_data,
      size=4,
      shape=18,
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

  # Plot styling
  pl <- pl +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y=ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )

  # Pass row number and label length to the knit_plot() function
  # TODO: Don't set rows manually
  pl <- .attr_transfer(pl, data, "missings")
  .to_vlkr_plot(pl, rows=4)
}

#' Output averages for multiple variables
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param cross The column holding groups to compare.
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
plot_metrics_one_grouped <- function(data, col, cross, negative = FALSE, ci = FALSE, box = FALSE, limits = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {

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

  # 4. Remove negatives
  if (!negative) {
    data <- data_rm_negatives(data, {{ col }})
  }

  # 5. Add n to labels
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
  .plot_lines(
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
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
plot_metrics_one_cor <- function(data, col, cross, negative = FALSE, limits = NULL, log = FALSE, title = TRUE, labels = TRUE, clean = TRUE, ...) {

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

  # 4. Remove negatives
  if (!negative) {
    data <- data_rm_negatives(data, c({{ col }}, {{ cross}}))
  }

  # 5. Remove 0 values in log plots
  if (log) {
    data <- data_rm_zeros(data, c({{ col }}, {{ cross }}))
  }

  # 6. Get column positions
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
    ggplot2::geom_point(size=3, alpha=VLKR_SCATTER_ALPHA)

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

  # Add theming
  pl <- pl +
    ggplot2::theme(
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )

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
  .to_vlkr_plot(pl, rows=15)
}

#' Output averages for multiple variables
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
plot_metrics_items <- function(data, cols, negative = FALSE, ci = FALSE, box = FALSE, limits = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})

  # 2. Clean
   if (clean) {
    data <- data_clean(data)
   }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}))

  # 4. Remove missings
  if (!negative) {
    data <- data_rm_negatives(data, c({{ cols }}))
  }

  # 5. Pivot items
  result <- data %>%
    labs_clear({{ cols }}) %>%
    tidyr::pivot_longer(
      {{ cols }},
      names_to = "item",
      values_to = "value"
    )

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
  .plot_lines(
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
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
plot_metrics_items_grouped <- function(data, cols, cross, negative = FALSE, limits = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)
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

  # 4. Calculate
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
        dplyr::select(item = "skim_variable", group = !!sym(col), "numeric.mean") %>%
        tidyr::drop_na()
    }
  ) %>%
    purrr::reduce(
      dplyr::bind_rows
    )

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

  # print(result)
  # class(result) <- setdiff(class(result),"skim_df")

  pl <- result %>%
    ggplot2::ggplot(ggplot2::aes(
      .data$item,
      y = .data$numeric.mean,
      color = .data$group,
      group=.data$group)
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size=3, shape=18)

  # Set the scale
  # TODO: get from attributes
  scale <- data %>%
    codebook({{ cols }}) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label")))) %>%
    prepare_scale()

  if (length(scale) > 0) {
    pl <- pl +
      ggplot2::scale_y_continuous(labels = ~ label_scale(., scale) )
  } else {
    pl <- pl +
      ggplot2::scale_y_continuous()
  }

  # Add scales, labels and theming
  pl <- pl +
    ggplot2::scale_x_discrete(labels = scales::label_wrap(40), limits = rev) +
    ggplot2::scale_color_manual(
      values = vlkr_colors_discrete(length(unique(result$group))),
      labels = function(x) wrap_label(x, width = 40)
      #guide = ggplot2::guide_legend(reverse = TRUE)
    ) +
    #ggplot2::scale_color_discrete(labels = function(x) stringr::str_wrap(x, width = 40)) +

    ggplot2::ylab("Mean values") +
    ggplot2::coord_flip(ylim = limits) +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(), #size = 11
      legend.title = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )

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
  pl <- pl + ggplot2::labs(caption = paste0("n=", base_n, "; multiple responses possible"))

  # Convert to vlkr_plot
  pl <- .attr_transfer(pl, data, "missings")
  .to_vlkr_plot(pl)
}

#' Scatter plot of correlations between multiple items
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross Tidyselect item variables to correlate (e.g. starts_with...).
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @importFrom rlang .data
plot_metrics_items_cor <- function(data, cols, cross, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
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
    ggplot2::scale_y_continuous(labels = scales::percent, limits=limits) +
    ggplot2::scale_x_discrete(labels = scales::label_wrap(40), limits = rev) +

    ggplot2::ylab("Share in percent") +
    ggplot2::coord_flip() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(),
      legend.title = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
    )

  if (all(data$item == "TRUE")) {
    pl <- pl +
      ggplot2::theme(axis.text.y = ggplot2::element_blank())
  }

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
        values = vlkr_colors_sequential(length(levels(data$value))),
        guide = ggplot2::guide_legend(reverse = TRUE)
      )
  } else {
    pl <- pl +
      ggplot2::scale_fill_manual(
        values = vlkr_colors_discrete(length(levels(data$value))),
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
  .to_vlkr_plot(pl)
}


#' Helper function: plot grouped line chart
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
.plot_lines <- function(data, ci = FALSE, scale = NULL, base = NULL, box = FALSE, limits = NULL, title = NULL) {

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
    ggplot2::scale_y_discrete(labels = scales::label_wrap(40), limits = rev)

  # Add theming
  pl <- pl +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(),
      legend.title = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot",
      plot.caption.position = "plot"
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
#' @return A ggplot object with vlkr_plt class.
.to_vlkr_plot <- function(pl, rows = NULL, maxlab = NULL, baseline = TRUE) {
  class(pl) <- c("vlkr_plt", class(pl))

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
knit_plot <- function(pl) {
  # Get knitr and volkr chunk options
  chunk_options <- knitr::opts_chunk$get()
  plot_options <- attr(pl, "vlkr_options")
  baseline <- attr(pl, "baseline", exact=TRUE)

  fig_width <- chunk_options$fig.width * 72
  fig_height <- chunk_options$fig.height * 72
  fig_dpi <- VLKR_PLOT_DPI
  fig_scale <- fig_dpi / VLKR_PLOT_SCALE

  # TODO: GET PAGE WIDTH FROM SOMEWHERE
  # page_width <- dplyr::coalesce(chunk_options$page.width, 1)

  # Calculate plot height
  if (!is.null(plot_options[["rows"]])) {
    fig_width <- VLKR_PLOT_WIDTH # TODO: make configurable
    px_perline <- VLKR_PLOT_PXPERLINE # TODO: make configurable

    # Buffer above and below the diagram
    px_offset <- VLKR_PLOT_OFFSETROWS * px_perline
    if (!is.null(pl$labels$title)) {
      px_offset <- px_offset + VLKR_PLOT_TITLEROWS * px_perline
    }

    rows <- plot_options[["rows"]]
    lines_wrap <- dplyr::coalesce(plot_options[["labwrap"]], VLKR_PLOT_LABELWRAP)
    lines_perrow <- (dplyr::coalesce(plot_options[["maxlab"]], 1) %/% lines_wrap) + 2

    fig_height <- (rows * lines_perrow * px_perline) + px_offset
  }

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

  base64_string <- base64enc::base64encode(pngfile)
  result <- paste0('<img src="data:image/png;base64,', base64_string, '" width="100%">')

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

  if (knitr::is_html_output()) {

    # TODO: leads to endless recursion
    # x <- knit_plot(x)
    # x <- knitr::asis_output(x)
    # knitr::knit_print(x)

    NextMethod()
  } else {
    NextMethod()
  }

  baseline <- attr(x, "baseline", exact = TRUE)
  if (!is.null(baseline)) {
    message(paste0("In the plot, ", baseline))
  }
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
#' @param n Number of colors.
#' @return A vector of colors.
vlkr_colors_sequential <- function(n) {
  colors <- getOption("vlkr.gradient.fill")
  if (is.null(colors)) {
    colors <- VLKR_FILLGRADIENT
  }


  colors <- scales::gradient_n_pal(colors)(
    seq(0,1,length.out=n)
  )
  colors
}
