#' Output a regression table with estimates and makro statistics
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding metric values
#' @param col_group The column holding groups to compare
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param missings Include missing values in the output (default FALSE)
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
stat_metrics_one_grouped <- function(data, col, col_group, negative = FALSE, missings= FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)


  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  missings <- sum(is.na(dplyr::select(data, {{ col }}, {{ col_group }})))

  if (missings > 0) {
    message(paste0(missings, " missing values have been removed."))
  }

  data <- tidyr::drop_na(data, {{ col }}, {{ col_group }})

  # Remove negative values
  # TODO: warn if any negative values were recoded
  if (!negative) {
    data <- dplyr::mutate(data, dplyr::across({{ col }}, ~ dplyr::if_else(. < 0, NA, .)))
  }

  lm <- data |>
    dplyr::select(av = {{ col }}, uv = {{ col_group }})

  # TODO: Add assumption tests, Shapiro (ttest, two groups), levene test?
  # Differentiate between samples? (independent?)

  stats:shapiro.test(av)

  car::levene.test(fit)

  fit <- stats::lm(av ~ uv, data = lm)

  result <- broom::tidy(fit)

  # Add macro statistics
  # TODO: .

  # result_macro <- broom::glance(fit)

  # Rename result labels
  # TODO: .

  .to_vlkr_tab(result,digits= digits)

}

#' Output Chi^2 and Cramers V
#'
#' @keywords internal
#'
#' TODO: pass to .to_vlkr_tab? digits? And how to deal with sample size? Approximation might be incorrect.
#'
#' @param data A tibble
#' @param col The column holding metric values
#' @param col_group The column holding groups to compare
#' @param digits The number of digits to print
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_counts_one_grouped(ds, adopter, sd_gender)
#'
#' @importFrom rlang .data

stat_counts_one_grouped <- function(data, col, col_group, clean = TRUE, ...) {

  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ col_group }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  missings <- sum(is.na(dplyr::select(data, {{ col }}, {{ col_group }})))

  if (missings > 0) {
    message(paste0(missings, " missing values have been removed."))
  }

  data <- tidyr::drop_na(data, {{ col }}, {{ col_group }})

  # Chi-squared test
  contingency <- data %>%
    count({{ col }}, {{ col_group }}) %>%
    spread({{ col_group }}, n, fill = 0) %>%

    # note: transformation due to incompatibility with chisq.test
    as.data.frame() %>%
    select(-1) %>%
    as.matrix()

  test <- stats::chisq.test(contingency)

  # Calculate Cramer's V
  n <- sum(contingency)
  phi <- sqrt(test$statistic / n)

  levels_col <- data %>%
    dplyr::distinct({{ col }}) %>%
    nrow()

  levels_col_group <- data %>%
    dplyr::distinct({{ col_group }}) %>%
    nrow()

  cramer_v <- round(phi / sqrt(min(levels_col, levels_col_group) - 1),2)

  # @Jakob: Alternative using effectsize package

  # Chi-squared test
  # data <- data %>%
  #   dplyr::count({{ col }}, {{ col_group }}) %>%
  #   tidyr::spread({{ col_group }}, n, fill = 0) %>%
  #
  #   # Note: transformation due to incompatibility with chisq.test
  #   as.data.frame() %>%
  #   select(-1) %>%
  #   as.matrix()
  #
  # test <- stats::chisq.test(data)
  #
  # # Calculate Cramer's V
  # cramer_v <- effectsize::cramers_v(test, adjust = F)

  # print
  print(test)
  cat("\n")
  cat("Cramer's V:", cramer_v, "\n")

}
