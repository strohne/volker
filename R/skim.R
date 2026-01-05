#' A skimmer for metric variables
#'
#' Inspired by the skimr package.
#' Computes descriptive statistics (min, quartiles, mean, sd, CI, items, alpha)
#' for selected numeric variables, respecting dplyr groupings and preserving attributes.
#'
#' @keywords internal
#'
#' @param data A data frame or tibble (may be grouped with dplyr::group_by()).
#' @param cols Columns to summarise, specified using tidyselect (e.g., where(is.numeric)).
#'
#' @return A tibble with summary statistics per variable (and per group if grouped).
#' @export
skim_metrics <- function(data, cols = tidyselect::where(is.numeric)) {
  group_vars <- dplyr::group_vars(data)
  grouped <- length(group_vars) > 0

  summarise_one <- function(x) {
    n <- sum(!is.na(x))
    missing <- sum(is.na(x))

    numeric_min <- numeric_q1 <- numeric_median <- numeric_q3 <- numeric_max <- NA
    numeric_mean <- numeric_sd <- ci_low <- ci_high <- items <- alpha <- NA

    if (any(!is.na(x))) {
      numeric_min <- min(x, na.rm = TRUE)
      numeric_q1 <- stats::quantile(x, 0.25, na.rm = TRUE, names = FALSE)
      numeric_median <- stats::median(x, na.rm = TRUE)
      numeric_q3 <- stats::quantile(x, 0.75, na.rm = TRUE, names = FALSE)
      numeric_max <- max(x, na.rm = TRUE)
      numeric_mean <- mean(x, na.rm = TRUE)
      numeric_sd <- stats::sd(x, na.rm = TRUE)

      # t-Test if appropriate
      if (length(stats::na.omit(x)) > 3 && dplyr::n_distinct(x) > 1) {
        ttest <- try(stats::t.test(stats::na.omit(x)), silent = TRUE)
        if (!inherits(ttest, "try-error")) {
          ci_low <- ttest$conf.int[1]
          ci_high <- ttest$conf.int[2]
        }
      }

      # Retrieve reliability summary
      alpha_info <- try(get_alpha(x), silent = TRUE)
      if (!inherits(alpha_info, "try-error")) {
        items <- alpha_info$items
        alpha <- alpha_info$alpha
      }
    }

    dplyr::tibble(
      n = n,
      missing = missing,
      min = numeric_min,
      q1 = numeric_q1,
      median = numeric_median,
      q3 = numeric_q3,
      max = numeric_max,
      mean = numeric_mean,
      sd = numeric_sd,
      ci.low = ci_low,
      ci.high = ci_high,
      items = items,
      alpha = alpha
    )
  }

  # Handle grouped vs ungrouped data
  if (grouped) {
    group_keys <- dplyr::group_keys(data)
    group_data <- dplyr::group_split(data, .keep = TRUE)

    purrr::map2_dfr(group_data, seq_along(group_data), function(df, i) {
      selected_vars <- tidyselect::eval_select(rlang::enquo(cols), df)
      var_summaries <- purrr::map_dfr(names(selected_vars), function(var) {
        x <- df[[var]]  # preserve attributes
        stats <- summarise_one(x)
        dplyr::tibble(skim_variable = var) %>%
          dplyr::bind_cols(stats)
      })

      dplyr::bind_cols(group_keys[i, , drop = FALSE], var_summaries)
    })
  } else {
    selected_vars <- tidyselect::eval_select(rlang::enquo(cols), data)
    purrr::map_dfr(names(selected_vars), function(var) {
      x <- data[[var]]  # preserve attributes
      stats <- summarise_one(x)
      dplyr::tibble(skim_variable = var) %>%
        dplyr::bind_cols(stats)
    })
  }
}


#' Calculate outliers
#'
#' @keywords internal
#'
#' @param x A numeric vector.
#' @return A list of outliers.
.outliers <- function(x, k=1.5) {
  list(stats::na.omit(x[(x < .whisker_lower(x)) | (x > .whisker_upper(x))]))
}


#' Calculate a metric by groups
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The item columns that hold the values to summarize.
#' @param cross The column holding groups to compare.
#' @param value The metric to extract from the skim result, e.g. mean or sd.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @return A tibble with each item in a row, a total column and columns for all groups.
skim_grouped <- function(data, cols, cross, value = "mean", labels = TRUE) {

  # Get positions of group cols
  cross <- tidyselect::eval_select(expr = enquo(cross), data = data)

  total <- data %>%
    dplyr::select({{ cols }}) %>%
    skim_metrics() %>%
    dplyr::select("skim_variable", total = !!sym(value))

  grouped <- purrr::map(
    cross,
    function(col) {
      col <- names(data)[col]

      data %>%
        dplyr::filter(!is.na(!!sym(col))) %>%
        dplyr::group_by(!!sym(col)) %>%
        dplyr::select(!!sym(col), {{ cols }}) %>%
        skim_metrics(tidyselect::where(is.numeric)) %>%
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


  result <- dplyr::inner_join(total, grouped, by = "skim_variable") %>%
    dplyr::rename(item = tidyselect::all_of("skim_variable"))


  if (labels) {

    # Item labels
    result <- labs_replace(
      result, "item",
      codebook(data, {{ cols }}),
      "item_name","item_label"
    )

    # Value labels
    codes <- codebook(data, {{ cross }}) %>%
      dplyr::distinct(.data$value_name, .data$value_label)

    value_labels <- colnames(result)
    value_labels[-(1:2)] <- value_labels[-(1:2)] %>%
      purrr::map_chr(~ ifelse(
        .x %in% codes$value_name,
        codes$value_label[match(.x, codes$value_name)],
        .x
      ))

    colnames(result) <- value_labels
  }

  result
}


#' Calculate IQR
#'
#' @keywords internal
#'
#' @param x A numeric vector
#' @return The IQR
.iqr <- function(x) {
  q <- stats::quantile(x, probs = c(0.25,0.75), na.rm = TRUE, names = FALSE)
  diff(q)
}

#' Calculate lower whisker in a boxplot
#'
#' @keywords internal
#'
#' @param x A numeric vector.
#' @return The lower whisker value.
.whisker_lower <- function(x, k=1.5) {
  i <- stats::quantile(x, 0.25, na.rm= TRUE) - k * stats::IQR(x, na.rm= TRUE)
  min(x[x >= i], na.rm= TRUE)
}

#' Calculate upper whisker in a boxplot
#'
#' @keywords internal
#'
#' @param x A numeric vector.
#' @return The upper whisker value.
.whisker_upper <- function(x, k=1.5) {
  i <- stats::quantile(x, 0.75, na.rm= TRUE) + k * stats::IQR(x, na.rm= TRUE)
  max(x[x <= i], na.rm= TRUE)
}
