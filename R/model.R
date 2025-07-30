#' Output a regression table with estimates and macro statistics
#' for multiple categorical or metric independent variables
#'
#' @description
#' The regression output comes from \code{stats::\link[stats:lm]{lm}}.
#' The effect sizes are calculated by \code{heplots::\link[heplots:etasq]{etasq}}.
#' The variance inflation is calculated by \code{car::\link[car:vif]{vif}}.
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
model_metrics_tab <- function(data, col, categorical, metric, interactions = NULL, labels = TRUE, clean = TRUE, ...) {


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
      interactions <- character(0)
    }
    else if (is.character(iexprs)) {
      interactions <- iexprs
    }
    else if (rlang::is_call(iexprs, "c")) {
      interactions <- vapply(as.list(iexprs)[-1], rlang::expr_text, character(1))
    } else {
      interactions <- rlang::expr_text(iexprs)
    }

    scores <- add_model(data, {{ col }}, {{ categorical }}, {{ metric }}, rlang::enexpr(interactions), labels, clean, ...)
    newcol <- setdiff(colnames(scores), colnames(data))
    result <- model_metrics_tab(scores, tidyselect::all_of(newcol), {{ categorical }}, {{ metric }}, rlang::enexpr(interactions), labels, clean, ...)
    return(result)
  }

  # Regression parameters
  lm_params <- tidy_lm_levels(fit)

  lm_params <- lm_params |>
    dplyr::mutate(
      Term = .data$term,
      stars = get_stars(.data$p.value),
      "ci low" = .data$conf.low,
      "ci high" = .data$conf.high,
      "standard error" = .data$std.error,
      t = .data$statistic,
      p = .data$p.value
    ) |>
    dplyr::mutate(dplyr::across(tidyselect::all_of(
      c("estimate","ci low", "ci high" , "standard error","t","p")
    ), function(x) ifelse(x == "NA","",x))) |>
    dplyr::select(tidyselect::all_of(c(
      "Term","estimate","ci low","ci high","standard error","t","p","stars"
    )))

  # Effect sizes
  lm_effects <- heplots::etasq(fit, anova = TRUE, partial = TRUE, type=2) |>
    tibble::as_tibble(rownames = "Item")

  colnames(lm_effects) <- c("Item", "Partial Eta Squared", "Sum of Squares", "Df", "F","p")
  lm_effects <- lm_effects |>
    mutate(stars = get_stars(.data$p))

  if (nrow(lm_effects) > 2) {

    lm_vif <- car::vif(fit, type = "terms") |>
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
model_metrics_plot <- function(data, col, categorical, metric, interactions = NULL, diagnostics = FALSE, labels = TRUE, clean = TRUE, ...) {

  # Handle interactions
  iexprs <- rlang::enexpr(interactions)
  if (rlang::is_call(iexprs, "enexpr")) {
    iexprs <- interactions
  }

  if (is.null(iexprs)) {
    interaction_terms <- character(0)
  }
  else if (is.character(iexprs)) {
    interaction_terms <- iexprs
  }
  else if (rlang::is_call(iexprs, "c")) {
    interaction_terms <- vapply(as.list(iexprs)[-1], rlang::expr_text, character(1))
  } else {
    interaction_terms <- rlang::expr_text(iexprs)
  }


  model_data <- model_metrics_tab(data, {{ col }}, {{ categorical }}, {{ metric }}, interactions = rlang::enexpr(interaction_terms), labels = labels, clean = clean, ...)

  coef_data <-  model_data$coefficients |>
    dplyr::filter(.data$Term != "(Intercept)") |>
    dplyr::filter(.data$estimate != "") |>
    dplyr::select(
      item = tidyselect::all_of("Term"),
      value = tidyselect::all_of("estimate"),
      low = tidyselect::all_of("ci low"),
      high = tidyselect::all_of("ci high")
    )

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
      residuals = list(.to_vlkr_plot(diagnostics_resid_fitted(fit))),
      qq =        list(.to_vlkr_plot(diagnostics_qq(fit))),
      location =  list(.to_vlkr_plot(diagnostics_scale_location(fit))),
      cooksd =    list(.to_vlkr_plot(diagnostics_cooksd(fit)))
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
#' The regression output comes from \code{stats::\link[stats:lm]{lm}}.
#' The effect sizes are calculated by \code{heplots::\link[heplots:etasq]{etasq}}.
#' The variance inflation is calculated by \code{car::\link[car:vif]{vif}}.
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
#'                     The interaction effect can be provided as character value (e.g. `c("sd_gender * adopter")`)
#'                     or as unquoted column names (e.g. `c(sd_gender * adopter)`).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{effect_metrics}.
#' @return The input tibble with one additional column.
#'         The new column name is derived from the target column, prefixed with "prd_".
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
add_model <- function(data, col, categorical, metric, interactions = NULL, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(
    data,
    {{ col }},
    c({{ categorical }}, {{ metric }}),
    cols.categorical = {{ categorical }},
    cols.numeric = c({{ col }}, {{ metric }}),
    clean = clean
  )

  # 2. Regression
  result <- list()

  # Construct formula
  categorical_vars <- names(tidyselect::eval_select(expr = rlang::enquo(categorical), data = data))
  metric_vars <-  names(tidyselect::eval_select(expr = rlang::enquo(metric), data = data))

  # Interaction terms to character vector
  iexprs <- rlang::enexpr(interactions)
  if (rlang::is_call(iexprs, "enexpr")) {
    iexprs <- interactions
  }

  if (is.null(iexprs)) {
    interaction_terms <- character(0)
  }
  else if (is.character(iexprs)) {
    interaction_terms <- iexprs
  }
  else if (rlang::is_call(iexprs, "c")) {
    interaction_terms <- vapply(as.list(iexprs)[-1], rlang::expr_text, character(1))
  } else {
    interaction_terms <- rlang::expr_text(iexprs)
  }


  rhs_terms <- c(categorical_vars, metric_vars, interaction_terms)
  rhs <- paste(rhs_terms, collapse = " + ")
  lhs <- rlang::as_label(rlang::enquo(col))
  formula_str <- paste0(lhs, " ~ ", rhs)

  # Fit
  fit <- stats::lm(stats::as.formula(formula_str), data = data)

  # Add column with fitted values and add the fit object as attribute
  newcol <- paste0("prd_", lhs)
  data[[newcol]] <- fit$fitted.values

  base_label <- get_title(data, {{ col }})
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

  ggplot2::ggplot(df, ggplot2::aes(x = .data$Fitted, y = .data$Residuals)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", se = FALSE, linewidth = 1, col = VLKR_COLOR_SMOOTH) +
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

  ggplot2::ggplot(df, ggplot2::aes(x = .data$Fitted, y = .data$Sqrt_Std_Resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_smooth(method = "loess", se = FALSE, linewidth = 1, col = VLKR_COLOR_SMOOTH) +
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

