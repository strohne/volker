#' Output effect sizes for count data
#'
#' The type of effect size depends on the number of selected columns:
#' - One column: see \link{stat_counts_one}
#' - Multiple columns: see \link{stat_counts_items}
#' - One column and one grouping column: see \link{stat_counts_one_grouped}
#' - Multiple columns and one grouping column: see \link{stat_counts_items_grouped}
#'
#'
#' @param data A data frame
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with()
#' @param col_group Optional, a grouping column. The column name without quotes.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Other parameters passed to the appropriate stat function
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_counts(data, sd_gender)
#'
#' @export
stat_counts <- function(data, cols, col_group = NULL, clean = TRUE, ...) {
  # Check
  check_is_dataframe(data)

  # Find columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  col_group_eval <- tidyselect::eval_select(expr = enquo(col_group), data = data)
  is_items <- length(cols_eval) > 1
  is_grouped <- length(col_group_eval)== 1

  # Single variables
  if (!is_items && !is_grouped) {
    # TODO: implement
    #stat_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped) {
    stat_counts_one_grouped(data, {{ cols }}, {{ col_group }}, ...)
  }

  # Items
  else if (is_items && !is_grouped) {
    # TODO: implement
    #stat_counts_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped) {
    # TODO: implement
    #stat_counts_items_grouped(data, {{ cols }}, {{ col_group }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not supported by volker functions.")
  }

}

#' Output effect sizes and regression model parameters
#'
#' The regression type depends on the number of selected columns:
#' - One column: see \link{stat_metrics_one}
#' - Multiple columns: see \link{stat_metrics_items}
#' - One column and one grouping column: see \link{stat_metrics_one_grouped}
#' - Multiple columns and one grouping column: see \link{stat_metrics_items_grouped}
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
stat_metrics <- function(data, cols, col_group = NULL, clean = TRUE, ...) {
  # Check
  check_is_dataframe(data)

  # Find columns
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  col_group_eval <- tidyselect::eval_select(expr = enquo(col_group), data = data)
  is_items <- length(cols_eval) > 1
  is_grouped <- length(col_group_eval)== 1

  # Single variables
  if (!is_items && !is_grouped) {
    # TODO: implement
    #stat_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped) {
    stat_metrics_one_grouped(data, {{ cols }}, {{ col_group }}, ...)
  }

  # Items
  else if (is_items && !is_grouped) {
    # TODO: implement
    #stat_metrics_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped) {
    # TODO: implement
    #stat_metrics_items_grouped(data, {{ cols }}, {{ col_group }},  ...)
  }

  # Not found
  else {
    stop("Check your parameters: the column selection is not supported by volker functions.")
  }

}


#' Output confidence intervals for category shares
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding factor values
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{stat_counts}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_counts_one(ds, adopter)
#'
#' @importFrom rlang .data
stat_counts_one <- function(data, col, col_group, clean = TRUE, ...) {

  #stop("Not implemented yet")

  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 4. Remove missings
  data <- data_rm_missings(data, c({{ col }}, {{col_group}}))


  # 4. Prepare data
  # TODO: calculate CI for shares
  # @Jakob: quick and dirty - param for confindence level?

  # Count
  result <- data %>%
    dplyr::count({{ col }}) %>%
    tidyr::drop_na() %>%
    dplyr::mutate("{{ col }}" := as.character({{ col }})) %>%
    dplyr::mutate(p = .data$n / sum(.data$n))

  # Calculate params
  total_obs <- sum(result$n)

  num_groups <- nrow(result)

  z <- qnorm(1 - (1 - 0.95) / 2)

  # Confidence Intervals
  result <- result %>%
    dplyr::mutate(
      std_error = sqrt(p * (1 - p) / total_obs),
      ci_lower = p - z * std_error,
      ci_upper = p + z * std_error
    )

  # Calculate gini coefficent
  # TODO: cumsum and calculation

  # 6. Prepare output
  #results <- tibble()

  .to_vlkr_tab(result)
}


#' Output test statistics and effect size for contingency tables (Chi^2 and Cramer's V)
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding factor values
#' @param col_group The column holding groups to compare
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{stat_counts}.
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

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ col }}, {{ col_group }}))


  # 4. Prepare data
  contingency <- data %>%
    count({{ col }}, {{ col_group }}) %>%
    spread({{ col_group }}, n, fill = 0) %>%
    as.data.frame() %>%
    select(-1) %>%
    as.matrix()

  # 5. Chi-squared test and Cramer's V
  exact <- any(contingency < 5)
  fit <- stats::chisq.test(contingency,simulate.p.value = exact)

  n <- sum(contingency)
  phi <- sqrt(fit$statistic / n)
  cramer_v <- round(phi / sqrt(min(dim(contingency)[1], dim(contingency)[1]) - 1),2)

  # Alternative using effectsize package
  # cramer_v <- effectsize::cramers_v(test, adjust = F)

  # 6. Prepare output
  results <- tibble::tribble(
    ~Statistic, ~Value, ~.digits,
    "Phi", phi, 2,
    "Cramér's V", cramer_v, 2,
    "Chi-quared", fit$statistic, 2,
    "p value", fit$p.value, 3,
    "Number of cases", n, 0
  )

  .to_vlkr_tab(results, caption=fit$method)
}


#' Output a regression table with estimates and macro statistics
#'
#' #TODO: Do we need the digits parameter?
#' #TODO: Add CI for effect sizes
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding metric values
#' @param col_group The column holding groups to compare
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param digits The number of digits to print
#' @param missings Include missing values in the output (default FALSE)
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{stat_metrics}.
#' @return A volker chunks object containing tables for micro and macro statistics
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_metrics_one_grouped(data, sd_age, sd_gender)
#'
#' @export
#' @importFrom rlang .data
stat_metrics_one_grouped <- function(data, col, col_group, negative = FALSE, digits = 1, missings= FALSE, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)


  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Recode negative values
  if (!negative) {
    data <- data_rc_negatives(data, {{ col }})
  }

  # 3. Remove missings
  if (!missings) {
    data <- data_rm_missings(data, c({{ col }}, {{ col_group }}))
  }

  lm_data <- data |>
    dplyr::select(av = {{ col }}, uv = {{ col_group }})

  # TODO: Add assumption tests, Shapiro (ttest, two groups), levene test?
  # Differentiate between samples? (independent?)

  #stats::shapiro.test(av)
  #car::levene.test(fit)
  fit <- stats::lm(av ~ uv, data = lm_data)

  result_params <- broom::tidy(fit, conf.int = TRUE)
  result_params <- tidycat::tidy_categorical(m = fit, result_params, include_reference = TRUE)

  result_params <- result_params |>
    dplyr::mutate(level = ifelse(
      .data$reference == "Baseline Category",
      paste0("(Baseline) ", .data$level),
      as.character(.data$level)
    )) |>
    dplyr::select(level, estimate,  conf.low, conf.high, std.error, p.value)


  result_macro <- broom::glance(fit)

  # Rename result labels
  # TODO: .

  result <- list(
    micro = .to_vlkr_tab(result_params, caption = "Regression parameters"),
    macro = .to_vlkr_tab(result_macro, caption = "Model statistics")
  )

  .to_vlkr_list(result)
}
