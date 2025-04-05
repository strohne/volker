#' Output a regression table with estimates and macro statistics
#' for multiple categorical or metric independent variables
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
#' @importFrom rlang .data
model_metrics_tab <- function(data, col, categorical, metric, labels = TRUE, clean = TRUE, ...) {
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
  lm_data <- dplyr::select(data, av = {{ col }}, {{ categorical }}, {{ metric }})
  fit <- stats::lm(av ~ ., data = lm_data)

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

  lm_vif <- car::vif(fit) |>
    tibble::as_tibble(rownames = "Item") |>
    dplyr::select(-tidyselect::all_of("Df"))

  lm_effects <- dplyr::left_join(lm_effects, lm_vif, by = "Item")

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
    result,
    coefficients = list(.to_vlkr_tab(lm_params, digits=2)),
    effects = list(.to_vlkr_tab(lm_effects, digits=2)),
    model = list(.to_vlkr_tab(lm_model, digits=2))
  )

  result <- .attr_transfer(result, data, "missings")
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
#' @importFrom rlang .data
model_metrics_plot <- function(data, col, categorical, metric, labels = TRUE, clean = TRUE, ...) {

  # TODO: implement model_metrics_add() and reuse fitted model if already present
  model_data <- model_metrics_tab(data, {{ col }}, {{ categorical }}, {{ metric }}, labels = labels, clean = clean, ...)

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
    ggplot2::geom_hline(yintercept = 0, color= "darkred") +
    ggplot2::coord_flip()

  # Assemble list
  result <- c(
    coefficients = list(pl_coef)
  )

  result <- .attr_transfer(result, model_data, "missings")
  .to_vlkr_list(result)
}
