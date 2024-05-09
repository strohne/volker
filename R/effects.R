#' Output effect sizes for count data
#'
#' @description
#' The type of effect size depends on the number of selected columns:
#' - One column: see \link{effect_counts_one} (not yet implemented)
#' - Multiple columns: see \link{effect_counts_items} (not yet implemented)
#' - One column and one grouping column: see \link{effect_counts_one_grouped}
#' - Multiple columns and one grouping column: see \link{effect_counts_items_grouped} (not yet implemented)
#'
#' By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - One column and one metric column: see \link{effect_counts_one_cor} (not yet implemented)
#' - Multiple columns and one metric column: see \link{effect_counts_items_cor} (not yet implemented)
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
  if (!is_items && !(is_grouped ||is_multi) && !is_metric) {
    effect_counts_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_metric) {
    effect_counts_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }
  else if (!is_items && is_grouped && is_metric) {
    effect_counts_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !(is_grouped || is_multi) && !is_metric) {
    effect_counts_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_metric) {
    effect_counts_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && (is_grouped || is_multi) && is_metric) {
    effect_counts_items_cor(data, {{ cols }}, {{ cross }},  ...)
  }
  # Not found
  else {
    stop("Check your parameters: the column selection is not yet supported by volker functions.")
  }

}

#' Output effect sizes and regression model parameters
#'
#' @description
#' The regression type depends on the number of selected columns:
#' - One column: see \link{effect_metrics_one} (not yet implemented)
#' - Multiple columns: see \link{effect_metrics_items}
#' - One column and one grouping column: see \link{effect_metrics_one_grouped}
#' - Multiple columns and one grouping column: see \link{effect_metrics_items_grouped} (not yet implemented)
#'
#'By default, if you provide two column selections, the second column is treated as categorical.
#' Setting the metric-parameter to TRUE will call the appropriate functions for correlation analysis:
#'
#' - Two metric columns: see \link{effect_metrics_one_cor}
#' - Multiple columns: see \link{effect_metrics_items_cor}
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
  is_cor <- metric != FALSE

  # Single variables
  if (!is_items && !is_grouped && !is_multi && !is_cor) {
    effect_metrics_one(data, {{ cols }}, ...)
  }
  else if (!is_items && is_grouped && !is_cor) {
    effect_metrics_one_grouped(data, {{ cols }}, {{ cross }}, ...)
  }
  else if (!is_items && is_grouped && is_cor) {
    effect_metrics_one_cor(data, {{ cols }}, {{ cross }}, ...)
  }

  # Items
  else if (is_items && !is_grouped && !is_multi && !is_cor) {
    effect_metrics_items(data, {{ cols }} , ...)
  }
  else if (is_items && is_grouped && !is_cor) {
    effect_metrics_items_grouped(data, {{ cols }}, {{ cross }},  ...)
  }
  else if (is_items && (is_grouped || is_multi) && is_cor) {
    effect_metrics_items_cor(data, {{ cols }}, {{ cross }},  ...)
  }
  # Not found
  else {
    stop("Check your parameters: the column selection is not yet supported by volker functions.")
  }

}


#' Test whether the shares differ from homogeneity
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding factor values.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_counts_one <- function(data, col, labels = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}


#' Output test statistics and effect size for contingency tables
#'
#' Chi squared is calculated using  \code{stats::\link[stats:chisq.test]{chisq.test}}.
#' If any cell contains less than 5 observations, the exact-parameter is set.
#'
#' Phi is derived from the Chi squared value by \code{sqrt(fit$statistic / n)}.
#' Cramer's V is derived by \code{phi / sqrt(min(dim(contingency)[1], dim(contingency)[1]) - 1)}.
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

  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ col }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ col }}, {{ cross }}))


  # 4. Prepare data
  contingency <- data %>%
    dplyr::count({{ col }}, {{ cross }}) %>%
    tidyr::pivot_wider(
      names_from = {{ cross }},
      values_from = .data$n,
      values_fill = 0) %>%
    as.data.frame() %>%
    dplyr::select(-1) %>%
    as.matrix()


  # 5. Chi-squared test and Cramer's V
  exact <- any(contingency < 5)
  fit <- stats::chisq.test(contingency,simulate.p.value = exact)

  n <- sum(contingency)
  cells <- min(dim(contingency)[1], dim(contingency)[1]) - 1
  cramer_v <- round(fit$statistic / (n * sqrt(cells)), 2)

  # 6. Prepare output
  result <- tibble::tribble(
    ~Statistic, ~Value,
    "Number of cases", as.character(n),
    "Cramer's V", sprintf("%.2f", round(cramer_v, 2)),
    "Degrees of freedom", as.character(fit$parameter),
    "Chi-squared", sprintf("%.2f", round(fit$statistic, 2)),
    "p value", sprintf("%.3f", round(fit$p.value, 3)),
    "stars", get_stars(fit$p.value)
  )

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, caption=fit$method)
}

#' Output test statistics and effect size for categories correlated with a metric column
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
effect_counts_one_cor <- function(data, col, cross, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Test whether shares differ
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return  A volker tibble.
#' @importFrom rlang .data
effect_counts_items <- function(data, cols, labels = TRUE, clean = TRUE, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
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
effect_counts_items_grouped <- function(data, cols, cross, clean = T, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}


#' Correlate the values in multiple items and output effect sizes and tests
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The target columns or NULL to calculate correlations within the source columns.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_counts}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_counts_items_cor <- function(data, cols, cross, clean = T, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}


#' Test whether the mean is different from zero
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_metrics_one <- function(data, col, clean = T, ... ) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}

#' Output a regression table with estimates and macro statistics
#'
#' The regression output comes from \code{stats::\link[stats:lm]{lm}}.
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The column holding metric values.
#' @param cross The column holding groups to compare.
#' @param negative If FALSE (default), negative values are recoded as missing values.
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
effect_metrics_one_grouped <- function(data, col, cross, negative = FALSE, method = "lm", labels = TRUE, clean = TRUE, ...) {
  # 1. Checks
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

  # 5. Calculate
  result <- list()
  lm_data <- dplyr::select(data, av = {{ col }}, uv = {{ cross }})

  # t.test
  if (("t.test" %in% method) && (length(unique(lm_data$uv)) == 2)) {

    stats_shapiro <- stats::shapiro.test(lm_data$av)
    stats_levene <- car::leveneTest(lm_data$av, group = lm_data$uv)
    stats_varequal = stats_levene[["Pr(>F)"]][1] > 0.05
    #stats_cohen <- get_cohensd(lm_data$av, lm_data$uv, pooled_sd = stats_varequal)
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
        "cI low" = sprintf("%.2f", round(stats_t$conf.int[1], 2)),
        "cI high" = sprintf("%.2f",round(stats_t$conf.int[2], 2)),
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
  if ("lm" %in% method) {
    fit <- stats::lm(av ~ uv, data = lm_data)

    # Regression parameters
    lm_params <- tidy_lm_levels(fit)

    lm_params <- lm_params |>
      dplyr::mutate(
        Term = .data$term,
        stars = get_stars(.data$p.value),
        estimate = sprintf("%.2f",round(.data$estimate,2)),
        conf.low = sprintf("%.2f",round(.data$conf.low,2)),
        conf.high = sprintf("%.2f",round(.data$conf.high,2)),
        std.error = sprintf("%.2f",round(.data$std.error,2)),
        t = sprintf("%.2f", round(.data$statistic,2)),
        p = sprintf("%.3f",round(.data$p.value,3))
      ) |>
      dplyr::mutate(dplyr::across(tidyselect::all_of(
        c("estimate","conf.low","conf.high","std.error","t","p")
      ), function(x) ifelse(x == "NA","",x))) |>
      dplyr::select(tidyselect::all_of(c(
        "Term","estimate","conf.low","conf.high","std.error","t","p","stars"
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
      ), relevel = TRUE) |>
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
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param method The output metrics, TRUE or pearson = Pearson's R, spearman = Spearman's rho.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker table containing the requested statistics.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics_one_cor(data, sd_age, use_private)
#'
#' @export
#' @importFrom rlang .data
effect_metrics_one_cor <- function(data, col, cross, negative = FALSE, method = "pearson", labels = TRUE, clean = TRUE, ...) {

  # 1. Checks
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
    data <- data_rm_negatives(data, c({{ col }}, {{ cross }}))
  }

  # 5. Calculate
  result <- .effect_correlations(data, {{ col }}, {{ cross}}, method=method, labels = labels)

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

#' Output effect size and test statistics for paired samples
#'
#' The correlation is calculated using \code{stats::\link[stats:cor.test]{cor.test}}.
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols The column holding metric values.
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param method The output metrics, TRUE or pearson = Pearson's R, spearman = Spearman's rho.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker table containing correlations.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics_items(data, starts_with("cg_adoption"))
#'
#'
#' @importFrom rlang .data
#' @export
effect_metrics_items <- function(data, cols, negative = FALSE, method = "pearson", labels = TRUE, clean = TRUE, ...) {

  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, {{ cols }})

  # 4. Remove negatives
  if (!negative) {
    data <- data_rm_negatives(data, {{ cols }})
  }

  # 6. Calculate correlations
  result <- .effect_correlations(data, {{ cols }}, {{ cols }}, method = method, labels = labels)
  result <- dplyr::filter(result, .data$item1 != .data$item2)

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

  if (prefix == "") {
    title <- NULL
  } else {
    title <- prefix
  }

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits= 2, caption=title)
}


#' Output confidence intervals of group means, F-Statistics and effect size (Eta^2)
#'
#' \strong{Not yet implemented. The future will come.}
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross The column holding groups to compare.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker tibble.
#' @importFrom rlang .data
effect_metrics_items_grouped <- function(data, cols, cross, clean = T, ...) {
  warning("Not implemented yet. The future will come.", noBreaks. = TRUE)
}



#' Output correlation coefficients for items
#'
#' The correlation is calculated using \code{stats::\link[stats:cor.test]{cor.test}}.
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures.
#' @param cols Tidyselect item variables (e.g. starts_with...).
#' @param cross Tidyselect item variables to correlate (e.g. starts_with...).
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param method The output metrics, TRUE or pearson = Pearson's R, spearman = Spearman's rho.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker table containing correlations.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' effect_metrics_items_cor(data, starts_with("cg_adoption_adv"), starts_with("use_"))
#'
#'
#' @export
#' @importFrom rlang .data
effect_metrics_items_cor <- function(data, cols, cross, negative = FALSE, method = "pearson", labels = TRUE, clean = TRUE, ...) {

  # 1. Checks
  check_is_dataframe(data)
  check_has_column(data, {{ cols }})
  check_has_column(data, {{ cross }})

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, c({{ cols }}, {{ cross }}))

  # 4. Remove negatives
  if (! negative) {
    data <- data_rm_negatives(data, c({{ cols }}, {{ cross }}))
  }

  result <- .effect_correlations(data, {{ cols }}, {{ cross}}, method = method, labels = labels)

  # Remove common item prefix
  prefix <- get_prefix(c(result$item1, result$item2))
  result <- dplyr::mutate(result, item1 = trim_prefix(.data$item1, prefix))
  result <- dplyr::mutate(result, item2 = trim_prefix(.data$item2, prefix))

  prefix <- ifelse(prefix == "", "Item", prefix)
  if (prefix == "") {
    title <- NULL
  } else {
    title <- prefix
  }


  result <- result %>%
    dplyr::rename("Item 1" = tidyselect::all_of("item1")) |>
    dplyr::rename("Item 2" = tidyselect::all_of("item2"))

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_tab(result, digits= 2, caption=title)
}

#' Test whether correlations are different from zero
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The columns holding metric values.
#' @param cross The columns holding metric values to correlate.
#' @param method The output metrics, TRUE or pearson = Pearson's R, spearman = Spearman's rho.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @return A tibble with correlation results.
#' @importFrom rlang .data
.effect_correlations <- function(data, cols, cross, method = "pearson", labels = TRUE) {

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  cross_eval <- tidyselect::eval_select(expr = enquo(cross), data = data)


  # Calculate correlation
  #stats_cohen <- effectsize::cohens_d(lm_data$av, lm_data$uv, pooled_sd = stats_varequal)

  method <- ifelse(method == "spearman", "spearman", "pearson")
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
        s = sprintf("%.2f", purrr::map_dbl(.data$.test, function(x) round(x$statistic,2))),
        stars = purrr::map_chr(.data$.test, function(x) get_stars(x$p.value)),
        p = sprintf("%.3f", purrr::map_dbl(.data$.test, function(x) round(x$p.value,3))),
        ) %>%
      dplyr::select(
        item1 = "x_name", item2 = "y_name",
        "n","Spearman's rho","s","p","stars"
      )

  } else {
    result <- result |>
      dplyr::mutate(
        n = nrow(data),
        "Pearson's r" = purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$estimate),2)),
        ci.low = purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$conf.int[1]),2)),
        ci.high = purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$conf.int[2]),2)),
        df = purrr::map_int(.data$.test, function(x) as.numeric(x$parameter)),
        t = sprintf("%.2f", purrr::map_dbl(.data$.test, function(x) round(as.numeric(x$statistic),2))),
        stars = purrr::map_chr(.data$.test, function(x) get_stars(x$p.value)),
        p = sprintf("%.3f", purrr::map_dbl(.data$.test, function(x) round(x$p.value,3))),
      ) %>%
      dplyr::mutate(t = ifelse(.data$x_name == .data$y_name, "Inf", t)) |>
      dplyr::select(
        item1 = "x_name", item2 = "y_name",
        "n","Pearson's r","ci.low","ci.high","df","t","p","stars"
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

#' Tidy lm results, replace categorical parameter names by their levels and add the reference level
#'
#' @keywords internal
#'
#' @param fit Result of a \link{lm} call.
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

#' Get Cohen's d for unpaired samples
#'
#' TODO: NOT FINISHED YET
#'
#' @keywords internal
#'
#' @param values A numeric vector.
#' @param groups A vector indicating groups.
#' @param conf The confidence interval.
#' @param pooled Whether to pool variances.
#' @return A list with the elements d (Cohen's d), ci.low and ci.high (its confidence interval).
get_cohensd <- function(values, groups, conf=0.95, pooled = FALSE) {
  levels <- unique(stats::na.omit(groups))
  x <- stats::na.omit(values[groups == levels[1]])
  y <- stats::na.omit(values[groups == levels[2]])

  di <- mean(x) - mean(y)
  var_x <- stats::var(x)
  var_y <- stats::var(y)
  s_x <- stats::sd(x)
  s_y <- stats::sd(y)
  n_x <- length(x)
  n_y <- length(y)
  n <- n_x + n_y

  if (pooled) {
    s <- sqrt(((n_x - 1) * var_x + (n_y - 1) * var_y) / (n_x + n_y - 2))
    cohensd <- di / s
    df <- n - 2
    se <-  sqrt((n_x + n_y) / (n_x * n_y) + (cohensd^2) / (2 * df))
  }
  else {
    #s <- sqrt((var_x + var_y)/2)
    s <- sqrt((s_x^2 / n_x) + (s_y^2 / n_y))
    cohensd <- di / s
    # Welch-Satterthwaite equation
    df <- ((var_x / n_x + var_y / n_y)^2) /
      ((var_x / n_x)^2 / (n_x - 1) + (var_y / n_y)^2 / (n_y - 1))
    se <- sqrt((var_x / n_x) + (var_y / n_y))
    #se <-  sqrt((n_x + n_y) / (n_x * n_y) + (cohensd^2) / (2 * df))
  }

  t_critical <- stats::qt(1 - ((1-conf) / 2), df)

  list(
    d = cohensd,
    ci.low = cohensd - (t_critical * se),
    ci.high= cohensd + (t_critical * se)
  )
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
