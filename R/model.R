#' Output a regression table with estimates and macro statistics
#' for multiple categorical or metric independent variables
#'
#' @description
#' The regression output comes from \code{stats::\link[stats:lm]{lm}}.
#' The effect sizes are calculated by \code{heplots::\link[heplots:etasq]{etasq}}.
#' The variance inflation is calculated by \code{car::\link[car:vif]{vif}}.
#' The standardized beta (in the column standard beta) is calculated by
#' multiplying the estimate with the ratio `x_sd / y_sd` where x_sd contains
#' the standard deviation of the predictor values and y_sd the standard deviation of
#' the predicted value.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The target column holding metric values.
#' @param categorical A tidy column selection holding independet categorical variables.
#' @param metric A tidy column selection holding independent metric variables.
#' @param interactions A vector of interaction effects to calculate.
#'                     Each interaction effect should be provided as multiplication of the variables.
#'                     Example: `c(sd_gender * adopter)`.
#' @param adjust Performing multiple significance tests inflates the alpha error.
#'               Thus, p values need to be adjusted according to the number of tests.
#'               Set a method supported by  \code{stats::\link[stats:p.adjust]{p.adjust}},
#'               e.g. "fdr" (the default) or "bonferroni". Disable adjustment with FALSE.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker list object containing volker tables with the requested statistics.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' data |>
#'   filter(sd_gender != "diverse") |>
#'   model_metrics_tab(use_work, categorical = c(sd_gender, adopter), metric = sd_age)
#'
#' @export
#' @aliases model_tab
#' @importFrom rlang .data
model_metrics_tab <- function(data, col, categorical = NULL, metric = NULL, interactions = NULL, adjust = "fdr", labels = TRUE, clean = TRUE, ...) {

  model_col <- dplyr::select(data, {{ col }})
  fit <- attr(model_col[[1]], "lm.fit")

  # Check: if model was not yet calculated, add it and recall model_metrics_tab() on the new column
  if (is.null(fit)) {

    # Handle interactions term
    iexprs <- rlang::enexpr(interactions)
    if (rlang::is_call(iexprs, "enexpr")) {
      iexprs <- interactions
    }

    if (is.null(iexprs)) {
      interactions <- NULL
    }
    else if (is.character(iexprs)) {
      interactions <- iexprs
    }
    else if (rlang::is_call(iexprs, "c")) {
      interactions <- vapply(as.list(iexprs)[-1], rlang::expr_text, character(1))
      interaction_terms <- rlang::enquo(interaction_terms)
    } else {
      interactions <- rlang::expr_text(iexprs)
      interaction_terms <- rlang::enquo(interaction_terms)
    }

    scores <- add_model(data, {{ col }}, {{ categorical }}, {{ metric }}, !!interactions, labels = labels, clean = clean, ...)

    newcol <- setdiff(colnames(scores), colnames(data))
    result <- model_metrics_tab(scores, tidyselect::all_of(newcol), adjust = adjust, labels = labels, clean = clean, ...)
    return(result)
  }

  # Regression parameters
  lm_params <- tidy_lm_levels(fit)

  lm_params <- lm_params |>
    dplyr::mutate(
      Term = .data$term,
      "ci low" = .data$conf.low,
      "ci high" = .data$conf.high,
      "standard beta" = .data$beta_std,
      "standard error" = .data$std.error,
      t = .data$statistic,
      p = .data$p.value,
      stars = get_stars(.data$p.value),
    ) |>
    dplyr::select(tidyselect::all_of(c(
      "Term","estimate", "ci low","ci high", "standard beta", "standard error","t","p"
    )))


  # Adjust and round
  lm_params <- lm_params %>%
    adjust_p("p", method = adjust)

  # Effect sizes (replace labels by numbers to avoid the message
  # "one or more coefficients in the hypothesis include arithmetic operators in their names"
  fit_cleaned <- fit
  names(fit_cleaned$coefficients) <- c(1:length(fit_cleaned$coefficients))
  lm_effects <- heplots::etasq(fit_cleaned, anova = TRUE, partial = TRUE, type=2) |>
    tibble::as_tibble(rownames = "Item")

  colnames(lm_effects) <- c("Item", "Partial Eta Squared", "Sum of Squares", "Df", "F","p")

  # Adjust and round
  lm_effects <- lm_effects %>%
    adjust_p("p", method = adjust)

  if (nrow(lm_effects) > 2) {


    lm_vif <- suppressMessages(car::vif(fit, type = "terms") )|>
      tibble::as_tibble(rownames = "Item") |>
      dplyr::select(-tidyselect::any_of("Df"))

    if ("value" %in% colnames(lm_vif)) {
      colnames(lm_vif)[colnames(lm_vif) == "value"] <- "VIF"
    }

    lm_effects <- dplyr::left_join(lm_effects, lm_vif, by = "Item")
  }

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
        "adj.r.squared", "df", "df.residual",
        "AIC", "vif", "statistic", "p.value", "stars"
      ),
      value_label=c(
        "Adjusted R squared", "Degrees of freedom", "Residuals' degrees of freedom",
        "AIC", "VIF", "F", "p", "stars"
      )
    ), na.missing = TRUE) |>
    stats::na.omit() |>
    dplyr::arrange(.data$Statistic)

  result <- c(
    coefficients = list(.to_vlkr_tab(lm_params, digits=2)),
    effects = list(.to_vlkr_tab(lm_effects, digits=2)),
    model = list(.to_vlkr_tab(lm_model, digits=2))
  )

  result <- .attr_transfer(result, data, "missings")
  attr(result, "lm.fit") <- fit
  .to_vlkr_list(result)
}


#' Plot regression coefficients
#'
#' @description
#' The regression output comes from \code{stats::\link[stats:lm]{lm}}.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The target column holding metric values.
#' @param categorical A tidy column selection holding categorical variables.
#' @param metric A tidy column selection holding metric variables.
#' @param interactions A vector of interaction effects to calculate.
#'                     Each interaction effect should be provided as multiplication of the variables.
#'                     Example: `c(sd_gender * adopter)`.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return A volker list object containing volker plots
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' data |>
#'   filter(sd_gender != "diverse") |>
#'   model_metrics_plot(use_work, categorical = c(sd_gender, adopter), metric = sd_age)
#'
#' @export
#' @aliases model_plot
#' @importFrom rlang .data
model_metrics_plot <- function(data, col, categorical = NULL, metric = NULL, interactions = NULL, diagnostics = FALSE, labels = TRUE, clean = TRUE, ...) {

  # Handle interactions
  iexprs <- rlang::enexpr(interactions)
  if (rlang::is_call(iexprs, "enexpr")) {
    iexprs <- interactions
  }

  if (is.null(iexprs)) {
    interaction_terms <- NULL
  }
  else if (is.character(iexprs)) {
    interaction_terms <- iexprs
  }
  else if (rlang::is_call(iexprs, "c")) {
    interaction_terms <- vapply(as.list(iexprs)[-1], rlang::expr_text, character(1))
    interaction_terms <- rlang::enquo(interaction_terms)
  } else {
    interaction_terms <- rlang::expr_text(iexprs)
    interaction_terms <- rlang::enquo(interaction_terms)
  }


  model_data <- model_metrics_tab(data, {{ col }}, {{ categorical }}, {{ metric }}, interactions = !!interaction_terms, labels = labels, clean = clean, ...)

  coef_data <-  model_data$coefficients |>
    dplyr::filter(.data$Term != "(Intercept)") |>
    dplyr::filter(.data$estimate != "") |>
    dplyr::select(
      item = tidyselect::all_of("Term"),
      value = tidyselect::all_of("estimate"),
      low = tidyselect::all_of("ci low"),
      high = tidyselect::all_of("ci high")
    )

  # Keep order of coefficients in plot
  coef_levels <- unique(coef_data$item)
  coef_data$item <- factor(coef_data$item, levels = coef_levels)

  pl_coef <- .plot_cor(coef_data, ci = TRUE) +
    ggplot2::geom_hline(yintercept = 0, linewidth = 0.5, color= VLKR_COLOR_SMOOTH) +
    ggplot2::coord_flip()

  # Assemble list
  result <- c(
    coefficients = list(pl_coef)
  )

  if (diagnostics) {
    fit <- attr(model_data, "lm.fit")

    result <- c(
      result,
      residuals = list(.to_vlkr_plot(diagnostics_resid_fitted(fit), rows=8)),
      qq =        list(.to_vlkr_plot(diagnostics_qq(fit), rows=8)),
      location =  list(.to_vlkr_plot(diagnostics_scale_location(fit), rows=8)),
      cooksd =    list(.to_vlkr_plot(diagnostics_cooksd(fit), rows=4))
    )
  }

  result <- .attr_transfer(result, model_data, "missings")
  .to_vlkr_list(result)
}


#' @export
model_plot <- model_metrics_plot

#' @export
model_tab <- model_metrics_tab


#' Add a column with predicted values from a regression model
#'
#' @description
#' You can either provide variables in dedicated parameters or use a formula.
#'
#' The regression output comes from \code{stats::\link[stats:lm]{lm}}.
#' The effect sizes are calculated by \code{heplots::\link[heplots:etasq]{etasq}}.
#' The variance inflation is calculated by \code{car::\link[car:vif]{vif}}.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param col The target column holding metric values or a model formula.
#'            If you provide a formula, skip the parameters for independent variables.
#' @param categorical A tidy column selection holding categorical variables.
#' @param metric A tidy column selection holding metric variables.
#' @param interactions A vector of interaction effects to calculate.
#'                     Each interaction effect should be provided as multiplication of the variables.
#'                     The interaction effect can be provided as character value (e.g. `c("sd_gender * adopter")`)
#'                     or as unquoted column names (e.g. `c(sd_gender * adopter)`).
#' @param newcol Name of the new column with predicted values.
#'              Set to NULL (default) to use the outcome variable name, prefixed with "prd_".
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return The input tibble with one additional column.
#'         The new column will have an attribute "lm.fit" with the fit model.
#' @examples
#' library(volker)
#' data <- filter(volker::chatgpt, sd_gender != "diverse")
#'
#' data <- data |>
#'   add_model(use_work, categorical = c(sd_gender, adopter), metric = sd_age)
#'
#' @export
#' @importFrom rlang .data
add_model <- function(data, col, categorical = NULL, metric = NULL, interactions = NULL, newcol = NULL, labels = TRUE, clean = TRUE, ...) {

  col_q <- rlang::enquo(col)

  # Formula mode
  if (rlang::quo_is_call(col_q, "~")) {

    terms <- .extract_terms_from_formula(rlang::eval_tidy(col_q), data)

    lhs <- terms$lhs
    categorical_vars <- terms$categorical
    metric_vars <- terms$metric
    interaction_terms <- terms$interactions

  }

  # Tidyselect mode
  else {

    lhs <- rlang::as_label(col_q)
    categorical_vars <- names(tidyselect::eval_select(enquo(categorical), data = data))
    metric_vars <- names(tidyselect::eval_select(enquo(metric), data = data))
    interaction_terms <- .interactions_to_text(enquo(interactions))

  }

  # 1. Checks, clean, remove missings
  data <- data_prepare(
    data,
    !!rlang::sym(lhs),
    c(categorical_vars, metric_vars),
    cols.categorical = categorical_vars,
    cols.numeric = c(lhs, metric_vars),
    clean = clean
  )

  # 2. Regression

  rhs_terms <- .clean_terms(c(categorical_vars, metric_vars, interaction_terms))
  rhs_full <- paste(rhs_terms, collapse = " + ")
  formula_str <- paste0(lhs, " ~ ", rhs_full)

  fit <- stats::lm(stats::as.formula(formula_str), data = data)

  # Determine column name
  if (is.null(newcol)) {
    newcol <- paste0("prd_", lhs)
  }

  # Add column with fitted values and add the fit object as attribute
  data[[newcol]] <- fit$fitted.values

  base_label <- get_title(data, !!rlang::sym(lhs))
  attr(data[[newcol]], "comment") <- paste0("Predicted: ", base_label)
  attr(data[[newcol]], "lm.fit") <- fit

  data

}


#' Residuals vs Fitted plot
#'
#' @keywords internal
#' @param fit The lm fit object
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- filter(volker::chatgpt, sd_gender != "diverse")
#'
#' data <- add_model(data, use_work, metric = sd_age)
#'
#' fit <- attr(data$prd_use_work, "lm.fit")
#' diagnostics_resid_fitted(fit)
#' @export
diagnostics_resid_fitted <- function(fit) {

  df <- data.frame(
    Fitted = stats::fitted(fit),
    Residuals = stats::resid(fit)
  )

  # TODO: Keep but set to 0?
  df <- df[is.finite(df$Residuals),, drop = FALSE]

  ggplot2::ggplot(df, ggplot2::aes(x = .data$Fitted, y = .data$Residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", formula = stats::as.formula("y ~ x"), se = FALSE, linewidth = 1, col = VLKR_COLOR_SMOOTH) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    )
}

#' Normal Q-Q
#'
#' @keywords internal
#' @param fit The lm fit object
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- filter(volker::chatgpt, sd_gender != "diverse")
#'
#' data <- add_model(data, use_work, metric = sd_age)
#'
#' fit <- attr(data$prd_use_work, "lm.fit")
#' diagnostics_qq(fit)
#'
#' @importFrom rlang .data
#' @export
diagnostics_qq <- function(fit) {

  df <- data.frame(stdres = stats::rstandard(fit))
  # TODO: Keep but set to 0?
  df <- df[is.finite(df$stdres),, drop = FALSE]

  ggplot2::ggplot(df, ggplot2::aes(sample = .data$stdres)) +
    ggplot2::stat_qq() +
    ggplot2::stat_qq_line(color = VLKR_COLOR_SMOOTH, linewidth = 1) +
    ggplot2::labs(
      title = "Normal Q-Q",
      x = "Theoretical Quantiles",
      y = "Standardized Residuals"
    )
}


#' Scale-Location (Spread-Location)
#'
#' @keywords internal
#' @param fit The lm fit object
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- filter(volker::chatgpt, sd_gender != "diverse")
#'
#' data <- add_model(data, use_work, metric = sd_age)
#'
#' fit <- attr(data$prd_use_work, "lm.fit)
#' diagnostics_scale_location(fit")
#'
#' @importFrom rlang .data
#' @export
diagnostics_scale_location <- function(fit) {
  df <- data.frame(
    Fitted = stats::fitted(fit),
    Sqrt_Std_Resid = sqrt(abs(stats::rstandard(fit)))
  )

  # TODO: Keep but set to 0?
  df <- df[is.finite(df$Sqrt_Std_Resid),, drop = FALSE]

  ggplot2::ggplot(df, ggplot2::aes(x = .data$Fitted, y = .data$Sqrt_Std_Resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", formula = stats::as.formula("y ~ x"), se = FALSE, linewidth = 1, col = VLKR_COLOR_SMOOTH) +
    ggplot2::labs(
      title = "Scale-Location",
      x = "Fitted values",
      y = expression(sqrt("|Standardized residuals|"))
    )
}

#' Cook's distance plot
#'
#' @keywords internal
#' @param fit The lm fit object
#' @return A ggplot object
#' @examples
#' library(volker)
#' data <- filter(volker::chatgpt, sd_gender != "diverse")
#'
#' data <- add_model(data, use_work, metric = sd_age)
#'
#' fit <- attr(data$prd_use_work, "lm.fit")
#' diagnostics_cooksd(fit)
#'
#' @importFrom rlang .data
#' @export
diagnostics_cooksd <- function(fit) {

  cooksd <- stats::cooks.distance(fit)
  cooksd[!is.finite(cooksd)] <- 0

  df <- data.frame(
    Observation = seq_along(cooksd),
    CookD = cooksd
  )


  ggplot2::ggplot(df, ggplot2::aes(x = .data$Observation, y = .data$CookD)) +
    ggplot2::geom_bar(stat = "identity", width = 0.3, fill = vlkr_colors_discrete(1)) +
    ggplot2::labs(
      title = "Cook's distance",
      x = "Observation",
      y = "Cook's distance"
    )
}


.interactions_to_text <- function(q) {
  expr <- rlang::get_expr(q)

  if (is.null(expr)) return(character(0))
  if (is.character(expr)) return(expr)

  if (rlang::is_call(expr, "c")) {
    return(vapply(as.list(expr)[-1], rlang::expr_text, character(1)))
  }

  rlang::expr_text(expr)
}

.extract_terms_from_formula <- function(f, data) {
  lhs <- all.vars(f[[2]])
  rhs_terms <- attr(stats::terms(f), "term.labels")

  interaction_terms <- rhs_terms[grepl("[:*]", rhs_terms)]
  simple_terms <- setdiff(rhs_terms, interaction_terms)

  factor_vars <- simple_terms[vapply(data[simple_terms], is.factor, logical(1))]
  character_vars <- simple_terms[vapply(data[simple_terms], is.character, logical(1))]
  categorical_vars <- c(factor_vars, character_vars)
  metric_vars <- setdiff(simple_terms, categorical_vars)

  list(
    lhs = lhs,
    categorical = categorical_vars,
    metric = metric_vars,
    interactions = interaction_terms
  )
}

.clean_terms <- function(x) {
  x <- unlist(x, use.names = FALSE)
  x[!is.na(x) & nzchar(x)]
}
