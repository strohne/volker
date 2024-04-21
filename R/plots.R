#' Output a frequency plot
#'
#' The type of frequency plot depends on the number of selected columns:
#' - One column: see \link{plot_counts_one}
#' - Multiple columns: see \link{plot_counts_items}
#' - One column and one grouping column: see \link{plot_counts_one_grouped}
#' - Multiple columns and one grouping column: see \link{plot_counts_items_grouped}
#'
#'
#' @param data A data frame
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with()
#' @param col_group Optional, a grouping column. The column name without quotes.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Other parameters passed to the appropriate plot function
#' @return A ggplot2 plot object
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts(data, sd_gender)
#'
#' @export
plot_counts <- function(data, cols, col_group = NULL, clean = TRUE, ...) {
  # Check
  check_is_dataframe(data)

  # Find columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  col_group_eval <- tidyselect::eval_select(expr = enquo(col_group), data = data)
  is_items <- length(cols_eval) > 1
  is_grouped <- length(col_group_eval)== 1

  # Single variables
  if (!is_items && !is_grouped) {
    plot_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped) {
    plot_counts_one_grouped(data, {{ cols }}, {{ col_group }}, ...)
  }

  # Items
  else if (is_items && !is_grouped) {
    plot_counts_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped) {
    plot_counts_items_grouped(data, {{ cols }}, {{ col_group }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not supported by volker functions.")
  }

}

#' Output a plot with distribution parameters such as the mean values
#'
#' The table type depends on the number of selected columns:
#' - One column: see \link{plot_metrics_one}
#' - Multiple columns: see \link{plot_metrics_items}
#' - One column and one grouping column: see \link{plot_metrics_one_grouped}
#' - Multiple columns and one grouping column: see \link{plot_metrics_items_grouped}
#'
#' @param data A data frame
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param col_group Optional, a grouping column (without quotes).
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Other parameters passed to the appropriate plot function
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics(data, sd_age)
#'
#' @export
plot_metrics <- function(data, cols, col_group = NULL, clean = TRUE, ...) {
  # Check
  check_is_dataframe(data)

  # Find columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  col_group_eval <- tidyselect::eval_select(expr = enquo(col_group), data = data)
  is_items <- length(cols_eval) > 1
  is_grouped <- length(col_group_eval)== 1

  # Single variables
  if (!is_items && !is_grouped) {
    plot_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped) {
    plot_metrics_one_grouped(data, {{ cols }}, {{ col_group }}, ...)
  }

  # Items
  else if (is_items && !is_grouped) {
    plot_metrics_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped) {
    plot_metrics_items_grouped(data, {{ cols }}, {{ col_group }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not supported by volker functions.")
  }

}

#' Plot the frequency of  values in one column.
#'
#' Note: only non-missing cases are used to calculate the percentage.
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding values to count
#' @param missings Include missing values (default FALSE)
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts_one(data, sd_gender)
#'
#' @importFrom rlang .data
#' @export
plot_counts_one <- function(data, col, missings = FALSE, numbers = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {

  # 1. Checks
  # Check columns
  check_is_dataframe(data)
  check_has_column(data, {{ col }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  if (!missings) {
    data <- data %>%
      tidyr::drop_na({{ col }})
  }

  # 3. Data
  # Count data
  result <- data %>%
    dplyr::count({{ col }}) %>%
    dplyr::mutate(p = (.data$n / sum(.data$n)) * 100)


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
    result <- labs_replace_values(result, {{ col }}, codebook(data, {{ col }}))
  }

  # 3. Plot
  # TODO: Make dry, see plot_item_counts and tab_group_counts
  pl <- result %>%
    ggplot2::ggplot(ggplot2::aes({{ col }}, y = .data$p / 100)) +
    ggplot2::geom_col() +

    # TODO: make limits configurable
    # scale_y_continuous(limits =c(0,100), labels=c("0%","25%","50%","75%","100%")) +

    ggplot2::scale_y_continuous(labels = scales::percent) +
    ggplot2::scale_x_discrete(limits=rev) +
    ggplot2::ylab("Share in percent") +
    ggplot2::coord_flip() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_text(), #size = 11
      legend.title = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.caption.position = "plot"
    )

  # Plot numbers
  if (!is.null(numbers)) {
    pl <- pl +
      ggplot2::geom_text(
        ggplot2::aes(label = .data$.values),
        position = ggplot2::position_stack(vjust = 0.5),
        color = "white" #size = 3
      )
  }

  # Title
  if (title == TRUE) {
    title <- get_title(data, {{ col }})
  } else if (title == FALSE) {
    title <- NULL
  }
  if (!is.null(title)) {
    pl <- pl + ggplot2::ggtitle(label = title)
  }

  # Base
  # TODO: report missing cases
  base_n <- nrow(data)
  pl <- pl + ggplot2::labs(caption = paste0("n=", base_n))

  # Pass row number and label length to the knit_plot() function
  .to_vlkr_plot(pl)
}

#' Plot frequencies cross tabulated with a grouping column
#'
#' Note: only non-missing cases are used to calculate the percentage.
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding factor values
#' @param col_group The column holding groups to compare
#' @param ordered Values can be nominal (0) or ordered ascending (1) descending (-1).
#'                By default (NULL), the ordering is automatically detected.
#'                An appropriate color scale should be choosen depending on the ordering.
#'                For unordered values, colors from VLKR_FILLDISCRETE are used.
#'                For ordered values, shades of the VLKR_FILLGRADIENT option are used.
#' @param category The value FALSE will force to plot all categories.
#'                  A character value will focus a selected category.
#'                  When NULL, in case of boolean values, only the TRUE category is plotted.
#' @param missings Include missing values (default FALSE)
#' @param prop The basis of percent calculation: "total" (the default), "rows" or "cols".
#'             Plotting row or column percentages results in stacked bars that add up to 100%.
#'             Whether you set rows or cols determines which variable is in the legend (fill color)
#'             and which on the vertical scale.
#' @param numbers The numbers to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts_one_grouped(data, adopter, sd_gender)
#'
#' @export
#' @importFrom rlang .data
plot_counts_one_grouped <- function(data, col, col_group, category = NULL, ordered = NULL, missings = FALSE, prop = "total", numbers = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {

  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ col_group }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Swap columns
  if (prop == "cols") {
    col <- rlang::enquo(col)
    col_group <- rlang::enquo(col_group)
    col_temp <- col
    col <- col_group
    col_group <- col_temp
    #stop("To display column proportions, swap the first and the grouping column. Then set the prop parameter to \"rows\".")
  }

  if (!missings) {
    data <- data %>%
      tidyr::drop_na({{ col }}, {{ col_group }})
  }

  # 3. Calculate data
  result <- data %>%
    dplyr::count({{ col }}, {{ col_group }})

  # Detect whether the categories are binary
  categories <- dplyr::pull(result, {{ col }}) |> unique() |> as.character()
  if ((length(categories) == 2) && (is.null(category)) && ("TRUE" %in% categories)) {
    category <- "TRUE"
  }
  scale <-dplyr::coalesce(ordered, get_direction(data, {{ col }}))

  data <- data |>
    dplyr::mutate(data, "{{ col_group }}" := as.factor({{ col_group }}))

  if (labels) {
    result <- labs_replace_values(result, {{ col_group }}, codebook(data, {{ col_group }}))
  }

  result <- result %>%
    dplyr::mutate(item = as.factor({{ col_group }})) |>
    dplyr::mutate(value = factor({{ col }}, levels = categories))

  if ((prop == "rows") || (prop == "cols")) {
    result <- result %>%
      dplyr::group_by({{ col_group }}) %>%
      dplyr::mutate(p = (.data$n / sum(.data$n)) * 100) %>%
      dplyr::ungroup()
  } else {
    result <- result %>%
      dplyr::mutate(p = (.data$n / sum(.data$n)) * 100)
  }

  # Select numbers to print on the bars
  # ...omit the last category in scales, omit small bars
  lastcategory <- ifelse(scale > 0, categories[1], categories[length(categories)])
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
  # TODO: report missing cases
  base_n <- nrow(data)

  .plot_bars(
    result,
    category = category,
    scale = scale,
    numbers = numbers,
    base = paste0("n=", base_n),
    title = title
  )
}

#' Output frequencies for multiple variables
#'
#' TODO: move missings to the end
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param category The value FALSE will force to plot all categories.
#'                  A character value will focus a selected category.
#'                  When NULL, in case of boolean values, only the TRUE category is plotted.
#' @param ordered Values can be nominal (0) or ordered ascending (1) descending (-1).
#'                By default (NULL), the ordering is automatically detected.
#'                An appropriate color scale should be choosen depending on the ordering.
#'                For unordered values, colors from VLKR_FILLDISCRETE are used.
#'                For ordered values, shades of the VLKR_FILLGRADIENT option are used.
#' @param missings Include missing values (default FALSE)
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_counts_items(data, starts_with("cg_adoption_"))
#'
#' @export
#' @importFrom rlang .data
plot_counts_items <- function(data, cols, category = NULL, ordered = NULL, missings = FALSE, numbers = NULL, title = TRUE, labels = TRUE, clean = TRUE, ...) {
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

  # 3. Calculate data
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
    dplyr::arrange(.data$value)

  # Detect whether the categories are binary
  categories <- result$value |> unique() |> as.character()
  if ((length(categories) == 2) && (is.null(category)) && ("TRUE" %in% categories)) {
    category <- "TRUE"
  }

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
    result <- labs_replace_names(result, "item", codebook(data, {{ cols }}))
    result <- labs_replace_values(result, "value", codebook(data, {{ cols }}))
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
  # TODO: report missing cases
  base_n <- nrow(data)

  .plot_bars(
    result,
    category = category,
    scale = scale,
    numbers = numbers,
    base = paste0("n=", base_n, "; multiple responses possible"),
    title = title
  )
}

#' Plot frequencies of multiple items compared by groups
#'
#' TODO: implement -> focus one category and show n / p
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols The item columns that hold the values to summarize
#' @param col_group The column holding groups to compare
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return A ggplot object
#' @importFrom rlang .data
plot_counts_items_grouped <- function(data, cols, col_group, clean = TRUE, ...) {
  stop("Not implemented yet")

  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

}

#' Output a histogram for a single metric variable
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The columns holding metric values
#' @param limits The scale limits. Set NULL to extract limits from the label. NOT IMPLEMENTED YET.
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_one(data, sd_age)
#'
#' @export
#' @importFrom rlang .data
plot_metrics_one <- function(data, col, limits = NULL, negative = FALSE, title = TRUE, labels = TRUE, clean = TRUE, ...) {

  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove negative values
  # TODO: warn if any negative values were recoded
  if (!negative) {
    data <- data |>
      labs_store() |>
      dplyr::mutate(dplyr::across({{ col }}, ~ dplyr::if_else(. < 0, NA, .))) |>
      labs_restore()
  }

  # Drop missings
  # TODO: Report missings
  data <- tidyr::drop_na(data, {{ col }})

  # TODO: make configurable: density, boxplot or histogram
  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes({{ col }})) +
    # geom_histogram(fill=VLKR_FILLCOLOR, bins=20)
    ggplot2::geom_density() +
    ggplot2::geom_vline(ggplot2::aes(xintercept=mean({{ col }})), color="black")

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

  # Get base
  # TODO: Report missings
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
  .to_vlkr_plot(pl, rows=4)
}

#' Output averages for multiple variables
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures
#' @param col The column holding metric values
#' @param col_group The column holding groups to compare
#' @param limits The scale limits. Set NULL to extract limits from the labels. NOT IMPLEMENTED YET.
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_one_grouped(data, sd_age, sd_gender)
#'
#' @export
#' @importFrom rlang .data
plot_metrics_one_grouped <- function(data, col, col_group, limits = NULL, negative = FALSE, title = TRUE, labels = TRUE, clean = TRUE, ...) {

  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ col_group }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove negative values
  # TODO: warn if any negative values were recoded
  if (!negative) {
    data <- data |>
      labs_store() |>
      dplyr::mutate(dplyr::across({{ col }}, ~ ifelse(. < 0, NA, .))) |>
      labs_restore()
  }

  # Drop missings
  # TODO: Report missings
  data <- tidyr::drop_na(data, {{ col }}, {{ col_group }})

  # Set the scale
  # if (is.null(limits)) {
  #   limits <- get_limits(data, {{ col }})
  # }


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


  # Add base
  # TODO: Report missing values, output range
  base_n <- nrow(data)

  data <- data %>%
    dplyr::mutate(item = as.factor({{ col_group }})) |>
    dplyr::mutate(value = ({{ col }}))

  .plot_lines(
    data,
    scale = scale,
    base = paste0("n=", base_n),
    title = title
  )

}

#' Output averages for multiple variables
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param limits The scale limits. Set NULL to extract limits from the labels. NOT IMPLEMENTED YET.
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_items(data, starts_with("cg_adoption_"))
#'
#' @export
plot_metrics_items <- function(data, cols, limits = NULL, negative = FALSE, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove negative values
  # TODO: warn if any negative values were recoded
  # TODO: Call prepare in every function and replace there, same for missings
  if (!negative) {
    data <- data %>%
      labs_store() %>%
      dplyr::mutate(dplyr::across({{ cols }}, ~ ifelse(. < 0, NA, .))) %>%
      labs_restore()
  }

  # Drop missings
  # TODO: Report missings
  data <- tidyr::drop_na(data, {{ cols }})

  # Pivot items
  result <- data %>%
    labs_clear({{ cols }}) %>%
    tidyr::pivot_longer(
      {{ cols }},
      names_to = "item",
      values_to = "value"
    )

  # Replace item labels
  if (labels) {
    result <- labs_replace_names(result, "item", codebook(data, {{ cols }}))
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
  # TODO: report missings
  base_n <- nrow(data)

  .plot_lines(
    result,
    scale = scale,
    base = paste0("n=", base_n, "; multiple responses possible"),
    title = title
  )
}


#' Output averages for multiple variables compared by a grouping variable
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param col_group The column holding groups to compare
#' @param limits The scale limits. Set NULL to extract limits from the labels. NOT IMPLEMENTED YET.
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param title If TRUE (default) shows a plot title derived from the column labels.
#'              Disable the title with FALSE or provide a custom title as character value.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' plot_metrics_items_grouped(data, starts_with("cg_adoption_"), sd_gender)
#'
#' @export
#' @importFrom rlang .data
plot_metrics_items_grouped <- function(data, cols, col_group, limits = NULL, negative = FALSE, title = TRUE, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col_group }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # TODO: warn if any negative values were recoded
  if (!negative) {
    data <- dplyr::mutate(data, dplyr::across({{ cols }}, ~ dplyr::if_else(. < 0, NA, .)))
  }

  # Drop missings
  # TODO: Report missings
  data <- tidyr::drop_na(data, {{ cols }}, {{ col_group }})


  # 3. Calculate
  # Get positions of group cols
  col_group <- tidyselect::eval_select(expr = rlang::enquo(col_group), data = data)

  # Grouped means
  result <- map(
    col_group,
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
    result <- labs_replace_names(result, "item", codebook(data, {{ cols }}))
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
      labels = function(x) stringr::str_wrap(x, width = 40)
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
  # TODO: Report missings
  base_n <- nrow(data)
  pl <- pl + ggplot2::labs(caption = paste0("n=", base_n, "; multiple responses possible"))

  # Convert to vlkr_plot
  .to_vlkr_plot(pl)
}

#' Helper function: plot grouped bar chart
#'
#' @keywords internal
#'
#' @param data Dataframe with the columns item, value, p, n
#' @param category Category for filtering the dataframe
#' @param scale Direction of the scale: 0 = no direction for categories,
#'              -1 = descending or 1 = ascending values.
#' @param numbers The values to print on the bars: "n" (frequency), "p" (percentage) or both.
#' @param title The plot title as character or NULL
#' @param base The plot base as character or NULL.
#' @return A ggplot object
#' @importFrom rlang .data
.plot_bars <- function(data, category = NULL, scale = NULL, numbers = NULL, base = NULL, title = NULL) {

  if (!is.null(category)) {
    data <- dplyr::filter(data, .data$value == category)
  }

  if (scale <= 0) {
    data <- data %>%
      dplyr::mutate(value = forcats::fct_rev(.data$value))
  }

  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(.data$item, y = .data$p / 100, fill = .data$value, group = .data$value)) +
    ggplot2::geom_col() +

    ggplot2::scale_y_continuous(labels = scales::percent) +
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

  # Select scales:
  # - Simplify binary plots
  # - Generate a color scale for ordinal scales by vlkr_colors_sequential()
  # - Use vlkr_colors_discrete() for other cases
  if (!is.null(category)) {
    pl <- pl +
      ggplot2::scale_fill_manual(
        guide = ggplot2::guide_legend(reverse = TRUE)
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
  .to_vlkr_plot(pl)
}


#' Helper function: plot grouped line chart
#'
#' @keywords internal
#'
#' @param data Dataframe with the columns item, value
#' @param scale ???
#' @param title The plot title as character or NULL
#' @param base The plot base as character or NULL.
#' @return A ggplot object
#' @importFrom rlang .data
.plot_lines <- function(data, scale = NULL, base = NULL, title = NULL) {

  pl <- data %>%
    ggplot2::ggplot(ggplot2::aes(y=.data$item, x=.data$value, group=1)) +
    #ggplot2::geom_point(size=3, shape=18)
    #ggplot2::geom_boxplot(fill="transparent", color="darkgray") +
    ggplot2::stat_summary(fun = mean, geom="line") +
    ggplot2::stat_summary(fun = mean, geom="point", size=4, shape=18)


  if (length(scale) > 0) {
    pl <- pl +
      ggplot2::scale_x_continuous(labels = ~ label_scale(., scale) )
  } else {
    pl <- pl +
      ggplot2::scale_x_continuous()
  }

  # Add scales, labels and theming
  pl <- pl +
    ggplot2::scale_y_discrete(labels = scales::label_wrap(40), limits = rev) +


    # TODO: set limits
    #coord_flip(ylim = limits) +
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
    dplyr::pull(.data$item) %>%
    stringr::str_length() %>%
    max(na.rm= TRUE)

  # Convert to vlkr_plot
  # Pass row number and label length to the knit_plot() function
  .to_vlkr_plot(pl, maxlab=maxlab)
}

#' Add the volker class and options
#'
#' @keywords internal
#'
#' @param pl A ggplot object
#' @param rows The number of items on the vertical axis. Will be automatically determined when NULL.
#'             For stacked bar charts, don't forget to set the group parameter, otherwise it won't work
#' @param maxlab The character length of the longest label to be plotted. Will be automatically determined when NULL.
#'               on the vertical axis
#' @return A ggplot object with vlkr_plt class
.to_vlkr_plot <- function(pl, rows = NULL, maxlab = NULL) {
  class(pl) <- c("vlkr_plt", class(pl))

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
    maxlab <- max(stringr::str_length(labels), na.rm= TRUE)

  }

  attr(pl, "vlkr_options") <- list(
    rows = rows,
    maxlab = maxlab
  )
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
#'           and the maximum
#' @return Character string containing a html image tag, including the base64 encoded image
knit_plot <- function(pl) {
  # Get knitr and volkr chunk options
  chunk_options <- knitr::opts_chunk$get()
  plot_options <- attr(pl, "vlkr_options")

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
  paste0('<img src="data:image/png;base64,', base64_string, '" width="100%">')
}


#' Printing method for volker plots
#'
#' @keywords internal
#'
#' @param x The volker plot
#' @param ... Further parameters passed to print()
#' @return No return value
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
}

#' @rdname print.vlkr_plt
#' @method plot vlkr_plt
#' @keywords internal
#' @export
plot.vlkr_plt <- print.vlkr_plt


#' Define a default theme for volker plots
#'
#' @importFrom ggplot2 '%+replace%'
#' @param base_size Base font size
#' @param base_color Base font color
#' @param base_fill A list of fill color sets. Each set can contain different numbers of colors.
#' @return A theme function
#' @examples
#' library(volker)
#' library(ggplot2)
#' data <- volker::chatgpt
#'
#' theme_set(theme_vlkr(base_size=15, base_fill = list("red")))
#' plot_counts(data, sd_gender)
#' @export
theme_vlkr <- function(base_size=11, base_color="black", base_fill = VLKR_FILLDISCRETE) {

  ggplot2::update_geom_defaults("text", list(size = (base_size-2) / ggplot2::.pt, color=base_color))
  ggplot2::update_geom_defaults("col",   list(fill = base_fill[[1]]))
  ggplot2::update_geom_defaults("density",   list(fill = base_fill[[1]]))
  ggplot2::update_geom_defaults("point",   list(color = base_fill[[1]]))
  ggplot2::update_geom_defaults("line",   list(color = base_fill[[1]]))

  options(ggplot2.discrete.fill=base_fill)

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
#' @param n Number of colors
#' @return A vector of colors
vlkr_colors_discrete <- function(n) {
  colors <- getOption("ggplot2.discrete.fill")
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
#' Creates a gradient scale based on VLKR_FILLGRADIENT
#'
#' @keywords internal
#'
#' @param n Number of colors
#' @return A vector of colors
vlkr_colors_sequential <- function(n) {
  colors <- scales::gradient_n_pal(VLKR_FILLGRADIENT)(
    seq(0,1,length.out=n)
  )
  colors
}



