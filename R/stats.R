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
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{stat_counts}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_counts_one(data, adopter)
#'
#' @importFrom rlang .data
stat_counts_one <- function(data, col, col_group, percent = TRUE, labels = TRUE, clean = TRUE, ...) {

  #stop("Not implemented yet")

  # 1. Check parameters
  check_is_dataframe(data)
  check_has_column(data, {{ col }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 4. Remove missings
  data <- data_rm_missings(data, c({{ col }}, {{ col_group }}))


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
    std_error = round(sqrt(p * (1 - p) / total_obs), 2),
    conf_low = round(p - z * std_error, 2),
    conf_high = round(p + z * std_error, 2)
  )

  # Get variable caption from the attributes
  if (labels) {
    result <- labs_replace_values(result, {{ col }}, codebook(data, {{ col }}))
    label <- get_title(data, {{ col }})
    result <- dplyr::rename(result, {{ label }} := {{ col }})

  }

  # Formatting proportions in percent
  if (percent) {
    result <- dplyr::mutate(result, p = paste0(round(.data$p * 100, 0), "%"))
  }

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



#' Output confidence intervals and effect size for multiple variables
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param clean Prepare data by \link{data_clean}.
#' @param percent Proportions are formatted as percent by default. Set to FALSE to get bare proportions.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_counts}.
#' @return  A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_counts_items(data, starts_with("cg_adoption_"))
#'
#' @importFrom rlang .data
#'
stat_counts_items <- function(data, cols, clean = TRUE, percent = TRUE, ...) {

  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}))

  # 4. Prepare data
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


  # # Absolute frequency
  # value <- "n"
  # result_n <- result %>%
  #   dplyr::select("item", "value", !!sym(value)) %>%
  #   tidyr::pivot_wider(
  #     names_from = value,
  #     values_from = !!sym(value),
  #     values_fill = stats::setNames(list(0), value)
  #   ) # %>%
  #   #janitor::adorn_totals("col")

  # Relative frequency
  # value <- "p"
  # result_p <- result %>%
  #   dplyr::select("item", "value", !!sym(value)) %>%
  #   tidyr::pivot_wider(
  #     names_from = value,
  #     values_from = !!sym(value),
  #     values_fill = stats::setNames(list(0), value)
  #   )

  # Confidence intervals

  # Calculate Confidence Intervals

  total_obs <- sum(result$n)
  #
  num_groups <- nrow(result)
  #
  z <- qnorm(1 - (1 - 0.95) / 2)
  #
  # Confidence Intervals
  result <- result %>%
    dplyr::mutate(
      std_error = round(sqrt(p * (1 - p) / total_obs), 2),
      conf_low = round(p - z * std_error, 2),
      conf_high = round(p + z * std_error, 2)
    )

  if (percent) {
    result <- dplyr::mutate(result, p = paste0(round(.data$p * 100, 0), "%"))
  }

  result <- result %>%
    dplyr::select("item", "value", "p") %>%
    tidyr::pivot_wider(
      names_from = value,
      values_from = p)

  .to_vlkr_tab(result)

}


#' Output confidence intervals and effect size for multiple variables by a grouping variable
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures and grouping variable
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param col_group The column holding groups to compare
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_counts_items_grouped(data, starts_with("cg_adoption_"))
#'
#' @importFrom rlang .data
#'
stat_counts_items_grouped <- function(data, cols, col_group, clean = T, ...) {

  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}, {{ col_group }}))

  # 4. Calculate confindence intervals


}


#' Correlate the values in multiple items (Pearson's R)
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures
#' @param cols Tidyselect item variables (e.g. starts_with...)
#' @param cols_cor The target columns or NULL to calculate correlations within the source columns
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_counts_items_grouped(ds, starts_with("cg_adoption_"))
#'
#' @importFrom rlang .data
#'
stat_counts_items_cor <- function(data, cols, cols_cor, clean = T, ...) {

}


#' Output confidence interval of mean for one metric variable
#'
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding metric values
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_metrics_one(data, sd_age)
#'
#' @export
#' @importFrom rlang .data
stat_metrics_one <- function(data, col, clean = T, ... ) {

  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, {{ col }})

  # 4. Get label
  label <- get_title(data, {{ col }})

  # 5. Calculate mean and confidence interval

  data <- data |>
    dplyr::select(av = {{ col }})

  fit <- lm(av ~ 1, data)

  result <- broom::tidy(fit, conf.int = TRUE)

  result <- result |>
    dplyr::select(mean = estimate, conf.low, conf.high, std.error)

  # Alternative choosing level param
  # result <- confint(fit, level=level)

  .to_vlkr_tab(result)

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
  # @JJ: optional param? if missings = FALSE, calculation fails

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


#' Output test statistics and effect size (Cohen's d) for paired samples
#'
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols The column holding metric values
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_metrics_items(data, starts_with("cg_adoption))
#'
#'
#' @importFrom rlang .data
stat_metrics_items <- function(data, cols, clean = T, ...) {

  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  #data <- data_rm_missings(data, c({{ cols }}))

  # 4. Calculate means and confidence intervals for items

  result <- data %>%
    dplyr::select({{ cols }})


  # 5. TODO: Anova (multiple items), t-test (two items)

  # Print

  .to_vlkr_tab(result)

}


#' Output confindence intervals of group means, F-Statistics and effectsize (Eta^2)
#'
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols The item columns that hold the values to summarize
#' @param col_group The column holding groups to compare and test
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_metrics_items_grouped(data, starts_with("cg_adoption), sd_gender)
#'
#'
#' @importFrom rlang .data
stat_metrics_items_grouped <- function(data, cols, col_group, clean = T, ...) {

  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}), {{ col_group}})

}



#' Output correlation coefficents for items
#'
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param cols The item columns that hold the values to summarize
#' @param col_group The column holding groups to compare and test
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' stat_metrics_items_grouped(data, starts_with("cg_adoption), sd_gender)
#'
#'
#' @importFrom rlang .data

stat_metrics_items_cor <- function(data, cols, clean = T, ... ) {

  # 1. Check parameters
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}))


}
