#' Output effect sizes and test statistics for count data
#'
#' @description
#' The type of effect size depends on the number of selected columns:
#' - One categorical column: see \link{effect_counts_one}
#' - Multiple categorical columns: see \link{effect_counts_items}
#'
#' Cross tabulations:
#'
#' - One categorical column and one grouping column: see \link{effect_counts_one_grouped}
#' - Multiple categorical columns and one grouping column: see \link{effect_counts_items_grouped} (not yet implemented)
#' - Multiple categorical columns and multiple grouping columns: \link{effect_counts_items_grouped_items} (not yet implemented)
#'
#' By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - One categorical column and one metric column: see \link{effect_counts_one_cor} (not yet implemented)
#' - Multiple categorical columns and one metric column: see \link{effect_counts_items_cor} (not yet implemented)
#' - Multiple categorical columns and multiple metric columns:\link{effect_counts_items_cor_items} (not yet implemented)
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
#' @param ... Other parameters passed to the appropriate effect function.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_counts(data, sd_gender, adopter)
#'
#' @export
effect_counts <- function(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...) {
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
    effect_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    effect_counts_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }
  else if (!is_items && is_grouped && is_metric) {
    effect_counts_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !is_grouped && !is_multi) {
    effect_counts_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_metric) {
    effect_counts_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && is_grouped && is_metric) {
    effect_counts_items_cor(data, {{ cols }}, {{ cross }},  ...)
  }
  # Not found
  else {
    stop("Check your parameters: the column selection is not yet supported by volker functions.")
  }
}

#' Output effect sizes and test statistics for metric data
#'
#' @description
#' The calculations depend on the number of selected columns:
#'
#' - One metric column: see \link{effect_metrics_one}
#' - Multiple metric columns: see \link{effect_metrics_items}
#'
#' Group comparisons:
#'
#' - One metric column and one grouping column: see \link{effect_metrics_one_grouped}
#' - Multiple metric columns and one grouping column: see \link{effect_metrics_items_grouped}
#' - Multiple metric columns and multiple grouping columns: not yet implemented
#'
#' By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - Two metric columns: see \link{effect_metrics_one_cor}
#' - Multiple metric columns and one metric column: see \link{effect_metrics_items_cor}
#' - Two metric column selections: see \link{effect_metrics_items_cor_items}
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
#' @param ... Other parameters passed to the appropriate effect function.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics(data, sd_age, sd_gender)
#'
#' @export
effect_metrics <- function(data, cols, cross = NULL, metric = FALSE, clean = TRUE, ...) {
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
    effect_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    effect_metrics_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }
  else if (!is_items && is_grouped && is_metric) {
    effect_metrics_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !is_grouped && !is_multi) {
    effect_metrics_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_metric) {
    effect_metrics_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && is_grouped && is_metric) {
    effect_metrics_items_cor(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && !is_grouped && is_multi && is_metric) {
    effect_metrics_items_cor_items(data, {{ cols }}, {{ cross }},  ...)
  }
  # Not found
  else {
    stop("Check your parameters: the column selection is not yet supported by volker functions.")
  }

}

#' Test homogeneity of category shares
#'
#' Performs a goodness-of-fit test and calculates the Gini coefficient.
#' The goodness-of-fit-test is calculated using \code{stats::\link[stats:chisq.test]{chisq.test}}.
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param clean Prepare data by \link{data_clean}
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#' data <- data |>
#'   filter(sd_gender != "diverse") |>
#'   effect_counts_one(sd_gender)
#'
#' @export
#' @importFrom rlang .data
effect_counts_one <- function(data, col, clean = TRUE, ...) {

  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, clean = clean)

  # 2. Observed
  observed <- data %>%
    dplyr::count({{ col }}) %>%
    dplyr::arrange({{ col }}) %>%
    dplyr::pull(.data$n)

  # 3. Expected
  expected <- rep(sum(observed) / length(observed), length(observed))

  # 4. Perform Chi-Square Goodness-of-Fit Test
  fit <- stats::chisq.test(x = observed, p = expected / sum(expected))

  # To tibble
  result <- list(
    "Gini coefficient" = sprintf("%.2f", get_gini(observed)),
    "Number of cases" = as.character(sum(observed)),
    "Chi-squared" = sprintf("%.2f", round(fit$statistic, 2)),
    "p value" = sprintf("%.3f", round(fit$p.value, 3)),
    "stars" = get_stars(fit$p.value)
  ) |>
  tibble::enframe(
    name = "Chi-Square Goodness-of-Fit",
    value = "value"
  )

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, caption = fit$method)
}


#' Output test statistics and effect size for contingency tables
#'
#' Chi squared is calculated using  \code{stats::\link[stats:chisq.test]{chisq.test}}.
#' If any cell contains less than 5 observations, the exact-parameter is set.
#'
#' Phi is derived from the Chi squared value by \code{sqrt(fit$statistic / n)}.
#' Cramer's V is derived by \code{sqrt(phi / (min(dim(contingency)[1], dim(contingency)[2]) - 1))}.
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param cross The column holding groups to compare.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_counts_one_grouped(data, adopter, sd_gender)
#'
#' @importFrom rlang .data
#' @export
effect_counts_one_grouped <- function(data, col, cross, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, clean = clean)

  # 2. Prepare data
  contingency <- data %>%
    dplyr::count({{ col }}, {{ cross }}) %>%
    tidyr::pivot_wider(
      names_from = {{ cross }},
      values_from = "n",
      values_fill = 0) %>%
    as.data.frame() %>%
    dplyr::select(-1) %>%
    as.matrix()

  # 3. Chi-squared test and Cramer's V
  exact <- any(contingency < 5)
  fit <- stats::chisq.test(contingency, simulate.p.value = exact)

  n <- sum(contingency)
  cells <- min(dim(contingency)[1], dim(contingency)[2]) - 1
  cramer_v <- round(sqrt( (fit$statistic / n) / cells), 2)

  # 4. Prepare output
  result <- tibble::tribble(
    ~Statistic, ~Value,
    "Cramer's V", sprintf("%.2f", round(cramer_v, 2)),
    "Number of cases", as.character(n),
    "Degrees of freedom", as.character(fit$parameter),
    "Chi-squared", sprintf("%.2f", round(fit$statistic, 2)),
    "p value", sprintf("%.3f", round(fit$p.value, 3)),
    "stars", get_stars(fit$p.value)
  )

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, caption=fit$method)
}

#' Output test statistics and effect size from a logistic regression of one metric predictor
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param cross The column holding metric values.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_counts_one_cor <- function(data, col, cross, clean = TRUE, labels = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}


#' Test homogeneity of category shares for multiple items
#'
#' Performs a goodness-of-fit test and calculates the Gini coefficient for each item.
#' The goodness-of-fit-test is calculated using  \code{stats::\link[stats:chisq.test]{chisq.test}}.
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return  A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_counts_items(data, starts_with("cg_adoption_adv"))
#'
#' @export
#' @importFrom rlang .data
effect_counts_items <- function(data, cols, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, clean = clean)

  # 2. Count
  result <- data %>%
    labs_clear({{ cols }}) %>%
    tidyr::pivot_longer(
      {{ cols }},
      names_to = "item",
      values_to = ".value",
      values_drop_na = TRUE
    ) %>%
    dplyr::group_by(.data$item, .data$.value) %>%
    dplyr::count() %>%
    dplyr::ungroup()

  # Chi-square goodness-of-fit test for each item
  result <- result %>%
    dplyr::group_by(.data$item) %>%
    dplyr::summarize(
      "Gini" = sprintf("%.2f", get_gini(.data$n)),
      "Number of cases" = as.character(sum(.data$n)),
      "Chi-squared" = stats::chisq.test(.data$n)$statistic,
      "p value" = stats::chisq.test(.data$n)$p.value,
      "stars" = get_stars(stats::chisq.test(.data$n)$p.value)
    )

  # Get variable caption from the attributes
  if (labels) {
    result <- labs_replace(result, "item", codebook(data, {{ cols }}), col_from="item_name", col_to="item_label")
    prefix <- get_prefix(result$item)
    result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))
  }

  # Rename first column
  if (prefix != "") {
    colnames(result)[1] <- prefix
  } else {
    result <- dplyr::rename(result, Item = tidyselect::all_of("item"))
  }

  # Result
  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result)
}


#' Effect size and test for comparing multiple variables by a grouping variable
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures and grouping variable.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The column holding groups to compare.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_counts_items_grouped <- function(data, cols, cross, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Effect size and test for comparing multiple variables by multiple grouping variables
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures and grouping variable.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The columns holding groups to compare.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_counts_items_grouped_items <- function(data, cols, cross, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Correlate the values in multiple items with one metric column and output effect sizes and tests
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The metric column.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_counts_items_cor <- function(data, cols, cross, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}


#' Correlate the values in multiple items with multiple metric columns and output effect sizes and tests
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The metric target columns.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_counts_items_cor_items <- function(data, cols, cross, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}


#' Test whether a distribution is normal
#'
#' The test is calculated using \code{stats::\link[stats:shapiro.test]{shapiro.test}}.
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param clean Prepare data by \link{data_clean}.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics_one(data, sd_age)
#'
#' @export
#' @importFrom rlang .data
effect_metrics_one <- function(data, col, labels = TRUE, clean = TRUE, ... ) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, clean = clean)

  # 2. Normality test
  stats <- dplyr::select(data, av = {{ col }})
  stats_shapiro <- stats::shapiro.test(stats$av)

  stats_shapiro <- tibble::enframe(
    name = "Shapiro-Wilk normality test",
    value = "value",
    x = list(
      "W-statistic" = sprintf("%.2f", round(stats_shapiro$statistic, 2)),
      "p value" = sprintf("%.3f", round(stats_shapiro$p.value, 3)),
      "stars" = get_stars(stats_shapiro$p.value),
      "normality" = ifelse(stats_shapiro$p.value > 0.05, "normal", "not normal")
    )
  )

  # 3. Skewness and kurtosis
  stats_skew <- psych::describe(stats$av)
  stats <- tibble::tibble(
    "metric" = c("skewness", "kurtosis"),
    "value" = c(
      sprintf("%.2f", round(stats_skew$skew, 2)),
      sprintf("%.2f", round(stats_skew$kurtosis, 2))
    )
  )

  # 4. Get item label from the attributes
  if (labels) {
    label <- get_title(data, {{ col }})
    stats <- dplyr::rename(stats, {{ label }} := "metric")
  }

  # 5. Results
  result <- c(
    list(.to_vlkr_tab(stats, digits=2)),
    list(.to_vlkr_tab(stats_shapiro, digits=2))
  )

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_list(result)
}

#' Output a regression table with estimates and macro statistics
#'
#' The regression output comes from \code{stats::\link[stats:lm]{lm}}.
#' T-test is performed using \code{stats::\link[stats:t.test]{t.test}}.
#' Normality check is performed using
#' \code{stats::\link[stats:shapiro.test]{shapiro.test}}.
#' Equality of variances across groups is assessed using \code{car::\link[car:leveneTest]{leveneTest}}.
#' Cohen's d is calculated using \code{effectsize::\link[effectsize:cohens_d]{cohens_d}}.
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param cross The column holding groups to compare.
#' @param method A character vector of methods, e.g. c("t.test","lm").
#'              Supported methods are t.test (only valid if the cross column contains two levels)
#'              and lm (regression results).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker list object containing volker tables with the requested statistics.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics_one_grouped(data, sd_age, sd_gender)
#'
#' @export
#' @importFrom rlang .data
effect_metrics_one_grouped <- function(data, col, cross, method = "lm", labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, clean = clean)

  check_is_param(method, c("lm", "t.test"))

  # 2. Calculate
  result <- list()
  lm_data <- dplyr::select(data, av = {{ col }}, uv = {{ cross }})

  # t.test
  if ("t.test" %in% method) {

    if (length(unique(lm_data$uv)) != 2) {
      stop("Check your parameters: the t.test method is only allowed for comparing two groups.")
    }

    stats_shapiro <- stats::shapiro.test(lm_data$av)
    stats_levene <- car::leveneTest(lm_data$av, group = lm_data$uv)
    stats_varequal = stats_levene[["Pr(>F)"]][1] > 0.05
    stats_cohen <- effectsize::cohens_d(lm_data$av, lm_data$uv, pooled_sd = stats_varequal, paired=FALSE)
    stats_t <- stats::t.test(lm_data$av ~ lm_data$uv, var.equal = stats_varequal)

    stats_t <- tibble::tribble(
      ~"Test", ~ "Results",
      "Shapiro-Wilk normality test", list(
        "W" = sprintf("%.2f",round(stats_shapiro$statistic,2)),
        "p" = sprintf("%.3f",round(stats_shapiro$p.value,3)),
        "stars" = get_stars(stats_shapiro$p.value),
        "normality" = ifelse(stats_shapiro$p.value > 0.05, "normal", "not normal")
      ),
      "Levene test", list(
        "F" = sprintf("%.2f",round(stats_levene[["F value"]][1],2)),
        "p" = sprintf("%.3f",round(stats_levene[["Pr(>F)"]][1],3)),
        "stars" = get_stars(stats_levene[["Pr(>F)"]][1]),
        "variances" = ifelse(stats_varequal, "equal", "not equal")
      ),
      "Cohen's d" , list(
        "d" = sprintf("%.1f",round(stats_cohen$Cohens_d, 1)),
        "ci low" = sprintf("%.1f",round(stats_cohen$CI_low, 1)),
        "ci high" = sprintf("%.1f",round(stats_cohen$CI_high, 1))
      ),
      "t-Test" ,list(
        "method" = stats_t$method,
        "difference" = sprintf("%.2f", round(stats_t$estimate[1] - stats_t$estimate[2], 2)),
        "ci low" = sprintf("%.2f", round(stats_t$conf.int[1], 2)),
        "ci high" = sprintf("%.2f",round(stats_t$conf.int[2], 2)),
        "standard error" = sprintf("%.2f",round(stats_t$stderr,2)),
        "df" = round(stats_t$parameter,2),
        "t" = sprintf("%.2f",round(stats_t$statistic,2)),
        "p" = sprintf("%.3f",round(stats_t$p.value,3)),
        "stars" = get_stars(stats_t$p.value)
      )
    )

    stats_t <- stats_t |>
      tidyr::unnest_longer(
        tidyselect::all_of("Results"),
        indices_to="statistic",
        values_to="value",
        transform=as.character
      ) |>
      dplyr::select("Test", "statistic","value")

    result <- c(result, list(.to_vlkr_tab(stats_t)))
  }


  # Regression model
  else if ("lm" %in% method) {
    fit <- stats::lm(av ~ uv, data = lm_data)

    # Regression parameters
    lm_params <- tidy_lm_levels(fit)

    lm_params <- lm_params |>
      dplyr::mutate(
        Term = .data$term,
        stars = get_stars(.data$p.value),
        estimate = sprintf("%.2f",round(.data$estimate,2)),
        "ci low" = sprintf("%.2f",round(.data$conf.low,2)),
        "ci high" = sprintf("%.2f",round(.data$conf.high,2)),
        "standard error" = sprintf("%.2f",round(.data$std.error,2)),
        t = sprintf("%.2f", round(.data$statistic,2)),
        p = sprintf("%.3f",round(.data$p.value,3))
      ) |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(
        c("estimate","ci low", "ci high" , "standard error","t","p")
      ), function(x) ifelse(x == "NA","",x))) |>
      dplyr::select(tidyselect::all_of(c(
        "Term","estimate","ci low","ci high","standard error","t","p","stars"
      )))

    # Regression model statistics
    lm_model <- broom::glance(fit) |>
      dplyr::mutate(dplyr::across(tidyselect::where(is.numeric), function(x) as.character(round(x,2)))) |>
      dplyr::mutate(stars = get_stars(.data$p.value)) |>
      tidyr::pivot_longer(
        tidyselect::everything(),
        names_to="Statistic",
        values_to="value"
      ) |>
      labs_replace("Statistic", tibble::tibble(
        value_name=c(
          "adj.r.squared", "df", "df.residual", "statistic", "p.value", "stars"
        ),
        value_label=c(
          "Adjusted R squared", "Degrees of freedom", "Residuals' degrees of freedom",
          "F", "p", "stars"
        )
      ), na.missing = TRUE) |>
      stats::na.omit() |>
      dplyr::arrange(.data$Statistic)


    result <- c(
      result,
      list(.to_vlkr_tab(lm_params, digits=2)),
      list(.to_vlkr_tab(lm_model, digits=2))
    )
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_list(result)
}


#' Test whether the correlation is different from zero
#'
#' The correlation is calculated using \code{stats::\link[stats:cor.test]{cor.test}}.
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param cross The column holding metric values to correlate.
#' @param method The output metrics, TRUE or pearson = Pearson's R, spearman = Spearman's rho.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker table containing the requested statistics.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics_one_cor(data, sd_age, use_private, metric = TRUE)
#'
#' @export
#' @importFrom rlang .data
effect_metrics_one_cor <- function(data, col, cross, method = "pearson", labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ col }}, {{ cross }}, clean = clean)

  # 2. Calculate correlation
  result <- .effect_correlations(data, {{ col }}, {{ cross}}, method = method, labels = labels)

  # 3. Labeling
  # Remove common prefix
  prefix <- get_prefix(c(result$item1, result$item2))
  result <- dplyr::mutate(result, item1 = trim_prefix(.data$item1, prefix))
  result <- dplyr::mutate(result, item2 = trim_prefix(.data$item2, prefix))

  if (prefix == "") {
    prefix <- "Item"
  }

  result <- result %>%
    dplyr::rename("Item 1" = tidyselect::all_of("item1")) |>
    dplyr::rename("Item 2" = tidyselect::all_of("item2"))

  if (prefix == "") {
    title <- NULL
  }

  result <- result |>
    dplyr::mutate(dplyr::across(tidyselect::everything(), \(x) as.character(x))) |>
    tidyr::pivot_longer(
      cols = -tidyselect::all_of(c("Item 1", "Item 2")),
      names_to ="Statistic"
    ) |>
    dplyr::select(-tidyselect::all_of(c("Item 1", "Item 2")))

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits= 2, caption=title)
}

#' Test whether a distribution is normal for each item
#'
#' The test is calculated using \code{stats::\link[stats:shapiro.test]{shapiro.test}}.
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols The column holding metric values.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker table containing item statistics
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics_items(data, starts_with("cg_adoption"))
#'
#'
#' @importFrom rlang .data
#' @export
effect_metrics_items <- function(data, cols, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, clean = clean)

  data <- dplyr::select(data, {{ cols }})

  result <- purrr::imap(
    data,
    \(.x, .y) {
      shapiro <- stats::shapiro.test(.x)
      stats <- psych::describe(.x)

      tibble::tibble(
        "Item" = .y,
        "skewness" = sprintf("%.2f", round(stats$skew, 2)),
        "kurtosis" = sprintf("%.2f", round(stats$kurt, 2)),
        "W-statistic" = sprintf("%.2f", round(shapiro$statistic,2)),
        "p value" = sprintf("%.3f", round(shapiro$p.value, 3)),
        "stars" = get_stars(shapiro$p.value),
        "normality" = ifelse(shapiro$p.value > 0.05, "normal", "not normal")
      )
    }
  ) %>%
    dplyr::bind_rows()

  # 3. Labels
  if (labels) {
    result <- labs_replace(
      result, "Item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
    )
  }

  prefix <- get_prefix(result$Item)
  result <- dplyr::mutate(
    result, Item = trim_prefix(.data$Item, prefix)
  )

  # Rename first column
  if (prefix != "") {
    colnames(result)[1] <- prefix
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result)
}

#' Compare groups for each item by calculating F-statistics and effect sizes
#'
#'
#' The models are fitted using \code{stats::\link[stats:lm]{lm}}.
#' ANOVA of type II is computed for each fitted model using \code{car::\link[car:Anova]{Anova}}.
#' Eta Squared is calculated for each ANOVA result
#' using \code{effectsize::\link[effectsize:eta_squared]{eta_squared}}.
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The column holding groups to compare.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker tibble.
#' @examples
#' library(volker)
#'
#' data <- volker::chatgpt
#' effect_metrics(data, starts_with("cg_adoption_"), adopter)
#'
#' @export
#' @importFrom rlang .data
effect_metrics_items_grouped <- function(data, cols, cross, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, clean = clean)

  # 2. Pivot
  lm_data <- data %>%
    dplyr::rename(uv = {{ cross }})

  lm_data <- lm_data %>%
    tidyr::pivot_longer(
    cols = {{ cols }},
    names_to = "item",
    values_to = "value") %>%
    dplyr::group_by(.data$item)

  # 3. Linear model
  lm <- lm_data %>%
    dplyr::summarise(
      model = list(lm(value ~ uv))
    )

  result <- lm %>%
    dplyr::mutate(
      tidy_model = purrr::map(.data$model, broom::tidy),
      eta_sq = purrr::map(.data$model, ~ effectsize::eta_squared(car::Anova(.x, type = 2), verbose = FALSE)),
      f_statistic = purrr::map_dbl(.data$model, ~ round(summary(.x)$fstatistic[1], 2)),
      p_value = purrr::map_dbl(.data$model, ~ round(summary(.x)$coefficients[2, 4], 2)),
      stars = purrr::map_chr(.data$model,~ get_stars(summary(.x)$coefficients[2, 4]))
    ) %>%
    tidyr::unnest(cols = c(.data$tidy_model, .data$eta_sq)) %>%
    dplyr::mutate(
      "Eta" = purrr::map_dbl(.data$Eta2, ~ round(sqrt(.), 2)),
      "Eta squared" = purrr::map_dbl(.data$Eta2, ~ round(., 2))
    ) %>%
    dplyr::select(tidyselect::all_of(c(
      "item", "F" = "f_statistic", "p" = "p_value",
      "stars", "Eta", "Eta squared"
    ))) %>%
    dplyr::distinct(.data$item, .keep_all = TRUE)

  # 4. Labels
  if (labels) {
     result <- labs_replace(
       result, "item",
       codebook(data, {{ cols }}),
       "item_name", "item_label"
     )
   }

  prefix <- get_prefix(result$item)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))

  # Rename first column
  if (prefix != "") {
    colnames(result)[1] <- prefix
  } else {
    result <- dplyr::rename(result, Item = tidyselect::all_of("item"))
  }

   result <- .attr_transfer(result, data, "missings")
   .to_vlkr_tab(result)
}

#' Compare groups for each item with multiple target items by calculating F-statistics and effect sizes
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The grouping items.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_metrics_items_grouped_items <- function(data, cols, cross, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Output correlation coefficients for items and one metric variable
#'
#' The correlation is calculated using \code{stats::\link[stats:cor.test]{cor.test}}.
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The column holding metric values to correlate.
#' @param method The output metrics, pearson = Pearson's R, spearman = Spearman's rho.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker table containing correlations.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics_items_cor(
#'   data, starts_with("cg_adoption_adv"), sd_age
#' )
#'
#' @export
#' @importFrom rlang .data
effect_metrics_items_cor <- function(data, cols, cross, method = "pearson", labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, clean = clean)

  # 2. Calculate correlations
  result <- .effect_correlations(data, {{ cols }}, {{ cross }}, method = method, labels = labels)

  # 3. Labels
  prefix1 <- get_prefix(result$item1)

  if (labels) {
    prefix2 <- get_title(data, {{ cross }})
  } else {
    prefix2 <- rlang::as_string(rlang::ensym(cross))
  }

  result <- result %>%
    dplyr::mutate(item1 = trim_prefix(.data$item1, prefix1)) %>%
    dplyr::select(-tidyselect::all_of("item2"))

  # Rename first column
  if (prefix1 != "") {
    colnames(result)[1] <- paste0(prefix1, ": Correlation with ", prefix2)
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits = 2)
}


#' Output correlation coefficients for multiple items
#'
#' The correlation is calculated using \code{stats::\link[stats:cor.test]{cor.test}}.
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross Tidyselect item variables (e.g. starts_with...).
#' @param method The output metrics, pearson = Pearson's R, spearman = Spearman's rho.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker table containing correlations.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics_items_cor_items(
#'   data,
#'   starts_with("cg_adoption_adv"),
#'   starts_with("use"),
#'   metric = TRUE
#' )
#'
#' @export
#' @importFrom rlang .data
effect_metrics_items_cor_items <- function(data, cols, cross, method = "pearson", labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ cross }}, clean = clean)

  # 2. Calculate correlations
  result <- .effect_correlations(data, {{ cols }}, {{ cross }}, method = method, labels = labels)

  # 3. Labels
  prefix1 <- get_prefix(result$item1)
  prefix2 <- get_prefix(result$item2)

  result <- result %>%
    dplyr::mutate(item1 = trim_prefix(.data$item1, prefix1)) |>
    dplyr::mutate(item2 = trim_prefix(.data$item2, prefix2))

  if ((prefix1 == prefix2) && (prefix1 != "")) {
    prefix1 <- paste0("Item 1: ", prefix1)
    prefix2 <- paste0("Item 2: ", prefix2)
  }

  # Rename first column
  if (prefix1 != "") {
    colnames(result)[1] <- prefix1
  }

  # Rename second column
  if (prefix2 != "") {
    colnames(result)[2] <- prefix2
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits = 2)
}


#' Test whether correlations are different from zero
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The columns holding metric values.
#' @param cross The columns holding metric values to correlate.
#' @param method The output metrics, pearson = Pearson's R, spearman = Spearman's rho.
#'               The reported R square value is just squared Spearman's or Pearson's R.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @return A tibble with correlation results.
#' @importFrom rlang .data
.effect_correlations <- function(data, cols, cross, method = "pearson", labels = TRUE) {

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)

  # Check method
  check_is_param(method, c("spearman", "pearson"))

  result <- expand.grid(
    x = cols_eval, y = cross_eval, stringsAsFactors = FALSE
  ) %>%
    dplyr::mutate(x_name = names(.data$x), y_name = names(.data$y)) %>%
    dplyr::mutate(
      .test = purrr::map2(
        .data$x, .data$y,
        function(x, y) stats::cor.test(
          data[[x]], data[[y]],
          method = method,
          exact = method != "spearman"
        )
      )
    )

  if (method == "spearman") {
    # TODO: geht das eleganter? Make DRY!
    # TODO: round in print function, not here
    result <- result |>
      dplyr::mutate(
        n = nrow(data),
        "Spearman's rho" = purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$estimate),2)),
        "R squared" = purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$estimate^2),2)),
        s = sprintf("%.2f", purrr::map_dbl(.data$.test, function(x) round(x$statistic,2))),
        stars = purrr::map_chr(.data$.test, function(x) get_stars(x$p.value)),
        p = sprintf("%.3f", purrr::map_dbl(.data$.test, function(x) round(x$p.value,3))),
        ) %>%
      dplyr::select(
        item1 = "x_name", item2 = "y_name",
        "R squared", "n","Spearman's rho","s","p","stars"
      )

  } else {
    result <- result |>
      dplyr::mutate(
        n = nrow(data),
        "Pearson's r" = purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$estimate),2)),
        "R squared" = purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$estimate^2),2)),
        "ci low" = purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$conf.int[1]),2)),
        "ci high" = purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$conf.int[2]),2)),
        df = purrr::map_int(.data$.test, function(x) as.numeric(x$parameter)),
        t = sprintf("%.2f", purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$statistic),2))),
        stars = purrr::map_chr(.data$.test, function(x) get_stars(x$p.value)),
        p = sprintf("%.3f", purrr::map_dbl(.data$.test, function(x) round(x$p.value,3))),
      ) %>%
      dplyr::mutate(t = ifelse(.data$x_name == .data$y_name, "Inf", t)) |>
      dplyr::select(
        item1 = "x_name", item2 = "y_name",
        "R squared", "n","Pearson's r", "ci low", "ci high","df","t","p","stars"
      )
  }

  result <- dplyr::arrange(result, .data$item1, .data$item2)

  # Get variable caption from the attributes
  if (labels) {
    result <- labs_replace(result, "item1", codebook(data, {{ cols }}), col_from="item_name", col_to="item_label")
    result <- labs_replace(result, "item2", codebook(data, {{ cross }}), col_from="item_name", col_to="item_label" )
  }

  result
}

#' Calculate nmpi
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param cross The column to correlate.
#' @param smoothing Add pseudocount. Calculate the pseudocount based on the number of trials
#'        to apply Laplace's rule of succession.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
.effect_npmi <- function(data, col, cross, labels = TRUE, clean = TRUE, smoothing = 0, ...) {

  cols_eval <- tidyselect::eval_select(expr = enquo(col), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)


  # Calculate marginal probabilities
  result <- data %>%
    dplyr::count({{ col }}, {{ cross }}) %>%
    #tidyr::complete({{ col }}, {{ cross }}, fill=list(n=0)) |>

    dplyr::group_by({{ col }}) %>%
    dplyr::mutate(.total_x = sum(.data$n)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by({{ cross }}) %>%
    dplyr::mutate(.total_y = sum(.data$n)) %>%
    dplyr::ungroup() %>%

    # Calculate joint probablities
    dplyr::mutate(
      .total = sum(.data$n),
      p_xy = (.data$n + smoothing) / (.data$.total + dplyr::n_distinct({{ col }}) * smoothing + dplyr::n_distinct({{ cross }}) *  smoothing),
      p_x = (.data$.total_x + smoothing) / (.data$.total + (dplyr::n_distinct({{ col }}) * smoothing)),
      p_y = (.data$.total_y + smoothing) / (.data$.total + dplyr::n_distinct({{ cross }}) * smoothing),

      ratio = .data$p_xy / (.data$p_x * .data$p_y),
      pmi = dplyr::case_when(
        .data$p_xy == 0 ~ -Inf,
        TRUE ~ log2(.data$ratio)
      ),
      npmi = dplyr::case_when(
        .data$p_xy == 0 ~ -1,
        TRUE ~ .data$pmi / -log2(.data$p_xy)
      )
    )

    result
  }

#' Tidy lm results, replace categorical parameter names by their levels and add the reference level
#'
#' @keywords internal
#'
#' @param fit Result of a \code{\link[stats:lm]{lm}} call.
#' @author Created with the help of ChatGPT.
#' @return A tibble with regression parameters.
tidy_lm_levels <- function(fit) {
  lm_tidy <- broom::tidy(fit, conf.int = TRUE)
  lm_data <- fit$model

  # Initialize an empty data frame for reference rows
  ref_rows <- data.frame()

  # Work through each factor in the model frame
  for (var in names(lm_data)) {
    if (is.character(lm_data[[var]])) {
      lm_data[[var]] <- as.factor(lm_data[[var]])
    }
    if (is.factor(lm_data[[var]])) {
      levels <- levels(lm_data[[var]])

      # Rename the coefficients in tidy_data
      for (level in levels[-1]) {
        old_name <- paste0(var, level)
        new_name <- paste0(level)
        lm_tidy$term <- sub(paste0("^\\Q", var, level,"\\E"), new_name, lm_tidy$term)
      }

      # Create reference level row, assuming the first level is the reference
      reference <- levels[1]
      ref_row <- data.frame(term = paste0(reference, " (Reference)"))
      ref_rows <- dplyr::bind_rows(ref_rows, ref_row)
    }
  }

  # Insert the reference rows just below the intercept row
  intercept_index <- which(lm_tidy$term == "(Intercept)")
  lm_tidy <- dplyr::bind_rows(
    lm_tidy[1:intercept_index, ],
    ref_rows,
    lm_tidy[-(1:intercept_index), ]
  )

  lm_tidy
}

#' Calculate the Gini coefficient
#'
#' @keywords internal
#'
#' @param x A vector of counts or other values
#' @return The gini coefficient
get_gini <- function(x) {

  x <- sort(x)
  n <- length(x)

  gini <- sum(x * c(1:n))
  gini <- 2 * gini/sum(x) - (n + 1)
  gini <- gini/n

  return(gini)
}


#' Calculate ci values to be used for error bars on a plot
#'
#' @keywords internal
#'
#' @param x A numeric vector.
#' @param conf The confidence level.
#' @return A named list with values for y, ymin, and ymax.
get_ci <- function(x, conf = 0.95) {
  n <- length(x)
  m <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  error_margin <- stats::qt(conf + (1 - conf) / 2, df = n - 1) * se
  return(c(y = m, ymin = m - error_margin, ymax = m + error_margin))
}

#' Get significance stars from p values
#'
#' @keywords internal
#'
#' @param x A vector of p values.
#' @return A character vector with significance stars.
get_stars <- function(x) {
  sapply(x, function(p) {
    if (is.na(p)) {
      return(NA)
    } else if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else if (p < 0.1) {
      return(".")
    } else {
      return("")
    }
  })
}
