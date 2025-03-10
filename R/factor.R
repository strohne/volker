#
# Functions to calculate principal components and factors
#

#' Get tables with factor analysis results
#'
#' @keywords internal
#'
#' @description
#' PCA is performed using \link{add_factors}.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A dataframe.
#' @param cols A tidy selection of item columns.
#'
#'             If the first column already contains a pca result from \link{add_factors},
#'             the result is used. Other parameters are ignored.
#'
#'             If there is no pca result yet, it is calculated by \link{add_factors} first.
#' @param k Number of factors to calculate.
#'          Set to NULL to report eigenvalues for all components up to the number of items
#'          and automatically choose k. Eigenvalues and the decision on k are calculated by
#'          \code{psych::\link[psych:fa.parallel]{fa.parallel}}.
#' @param newcols Names of the new factor columns as a character vector.
#'                Must be the same length as k or NULL.
#'                Set to NULL (default) to automatically build a name
#'                from the common column prefix, prefixed with "fct_", postfixed with the factor number.
#' @param method The method as character value. Currently, only pca is supported.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker list with with three volker tabs: loadings, variances and diagnostics.
#' @examples
#' library(volker)
#' ds <- volker::chatgpt
#'
#' volker::factor_tab(ds, starts_with("cg_adoption"), k = 3)
#' @export
#' @importFrom rlang .data
factor_tab <- function(data, cols, newcols = NULL, k = 2, method = "pca", labels = TRUE, clean = TRUE, ...) {

  fct_items <- dplyr::select(data, {{ cols }})
  fit <- attr(fct_items[[1]], "psych.pca.fit")
  items <- attr(fct_items[[1]], "psych.pca.items")

  # Check: if factors were not yet calculated, add them and recall factor_tab() on the new columns
  if (is.null(fit)) {
    scores <- add_factors(fct_items, {{ cols }}, newcols = newcols, k = k, method = method, clean = clean, ...)
    newcols <- setdiff(colnames(scores), colnames(fct_items))
    result <- factor_tab(scores, tidyselect::all_of(newcols), newcols, k, method, labels, clean, ...)
    return(result)
  }

  # Get communalities and loadings from the fit object
  fit_loadings <- tibble::as_tibble(unclass(fit$loadings), rownames = "item")
  fit_communal <- tibble::as_tibble(as.data.frame(fit$communality), rownames = "item")

  fit_loadings <- fit_loadings %>%
    dplyr::left_join(fit_communal, by = "item")

  fct_colnames <- colnames(fct_items)
  fct_labels <- unique(codebook(data, {{ cols }})$item_label)
  if (labels) {
    colnames(fit_loadings) <- c("item", fct_labels, "communality")
  } else {
    colnames(fit_loadings) <- c("item", fct_colnames, "communality")
  }


  if (labels) {
    # Replace item labels
    fit_loadings <- labs_replace(
      fit_loadings, "item",
      codebook(data, tidyselect::any_of(items)),
      "item_name", "item_label"
    )

    # Remove common item prefix and rename first column
    # TODO: make dry
    prefix <- get_prefix(fit_loadings$item, trim = TRUE)
    fit_loadings <- dplyr::mutate(fit_loadings, item = trim_prefix(.data$item, prefix))
    colnames(fit_loadings)[1] <- ifelse(prefix == "", "Item", prefix)
  }

  # Add baseline
  fit_loadings <- .attr_transfer(fit_loadings, data, "missings")


  # Add eigenvalues and variance
  fit_var <- data.frame(t(fit$Vaccounted))
  fit_var$Cumulative.Var <- dplyr::coalesce(fit_var$Cumulative.Var, fit_var$Proportion.Var)
  fit_var <- dplyr::select(fit_var, tidyselect::all_of(c("SS.loadings", "Proportion.Var", "Cumulative.Var")))
  colnames(fit_var) <- c("Eigenvalue", "Proportion of variance", "Cumulative proportion of variance")

  rownames(fit_var) <- fct_labels
  if (labels) {
    rownames(fit_var) <- fct_labels
  } else {
    rownames(fit_var) <- fct_colnames
  }
  fit_var <- tibble::as_tibble(fit_var,rownames  = "Component")


  # Additional model statistics
  fit_stats <- attr(fct_items[[1]], "psych.kmo.bartlett")
  fit_parallel <- attr(fct_items[[1]], "psych.fa.parallel")

  # Prepare output list
  result <- c(
    "loadings" = list(.to_vlkr_tab(fit_loadings)),
    "components" = list(.to_vlkr_tab(fit_var)),
    "model" = list(.to_vlkr_tab(fit_stats))
  )

  if (!is.null(fit_parallel)) {
    result <- c(result, "eigenvalues" = list(.to_vlkr_tab(fit_parallel, caption = "Eigenvalues for scree plot")))
  }
  .to_vlkr_list(result)
}

#' Get plot with factor analysis result
#'
#' @keywords internal
#'
#' @description
#' PCA is performed using \link{add_factors}.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A dataframe.
#' @param cols A tidy selection of item columns.
#'             If the first column already contains a pca from add_factors, the result is used. Other parameters are ignored.
#'             If there is no pca result yet, it is calculated by \link{add_factors} first.
#' @param k Number of factors to calculate.
#'          Set to NULL to generate a scree plot with eigenvalues for all components up to the number of items
#'          and automatically choose k. Eigenvalues and the decision on k are calculated by
#'          \code{psych::\link[psych:fa.parallel]{fa.parallel}}.
#' @param newcols Names of the factor columns as a character vector.
#'                Must be the same length as k or NULL.
#'                Set to NULL (default) to automatically build a name
#'                from the common column prefix, prefixed with "fct_", postfixed with the factor number.
#' @param method The method as character value. Currently, only pca is supported.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' ds <- volker::chatgpt
#'
#' volker::factor_plot(ds, starts_with("cg_adoption"), k = 3)
#' @export
#' @importFrom rlang .data
factor_plot <- function(data, cols, newcols = NULL, k = 2, method = "pca", labels = TRUE, clean = TRUE, ...) {

  # Get loading and scree data
  tabs <- factor_tab(data, {{ cols }}, newcols, k, method, labels = FALSE, clean, ...)

  # 1. Loading plot
  loadings <- tabs$loadings
  fctcols <- colnames(loadings)[-c(1, ncol(loadings))]
  itemcols <- loadings$item

  loadings <- tidyr::pivot_longer(
    loadings,
    tidyselect::all_of(fctcols),
    names_to=".cross",
    values_to="value"
  )

  # Item and value labels
  if (labels) {
    loadings <- labs_replace(loadings, "item", codebook(data, tidyselect::any_of(itemcols)), "item_name", "item_label")

    # TODO: Better pass labels through factor_tab instead of building new labels here
    fct_labels <- tibble::tibble(
       item_name = tabs$components[[1]],
       item_label = paste0("Component ", c(1:length(tabs$components[[1]])))
    )

    loadings <- labs_replace(loadings, ".cross", fct_labels)
  }

  prefix <- get_prefix(loadings$item, trim = TRUE)
  loadings <- dplyr::mutate(loadings, item = trim_prefix(.data$item, prefix))
  title <- ifelse(prefix == "", "Item", prefix)

  colnames(loadings)[1] <- "item"
  limits <- get_limits(loadings, tidyselect::all_of("value"))
  base_n <-tabs$model$value[tabs$model$Statistic == "Cases"]
  loadings <- .attr_transfer(loadings, tabs$loadings, "missings")

  plot_loadings <- .plot_lines(
    loadings,
    limits = limits,
    scale = limits,
    title = title,
    base = paste0("n=", base_n)
  )

  # Prepare output list
  result <- c(
    "loadings" = list(plot_loadings)
  )

  # 2. Conditionally add scree plot
  if (!is.null(tabs$eigenvalues)) {
    scree <- .plot_scree(
      tabs$eigenvalues,
      k = nrow(tabs$components),
      lab_x = "Component",
      lab_y = "Eigenvalue"
      )
    result <- c(result, "scree" = list(scree))
  }

  .to_vlkr_list(result)

}


#' Add PCA columns along with summary statistics (KMO and Bartlett test) to a data frame
#'
#' @description
#' PCA is performed using \code{psych::\link[psych:pca]{pca}} usind varimax rotation.
#' Bartlett's test for sphericity is calculated with \code{psych::\link[psych:cortest.bartlett]{cortest.bartlett}}.
#' The Kaiser-Meyer-Olkin (KMO) measure is computed using \code{psych::\link[psych:KMO]{KMO}}.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A dataframe.
#' @param cols A tidy selection of item columns.
#' @param k Number of factors to calculate.
#'          Set to NULL to calculate eigenvalues for all components up to the number of items
#'          and automatically choose k. Eigenvalues and the decision on k are calculated by
#'          \code{psych::\link[psych:fa.parallel]{fa.parallel}}.
#' @param newcols Names of the factor columns as a character vector.
#'                Must be the same length as k or NULL.
#'                Set to NULL (default) to automatically build a name
#'                from the common column prefix, prefixed with "fct_", postfixed with the factor number.
#' @param method The method as character value. Currently, only pca is supported.
#' @param clean Prepare data by \link{data_clean}.
#' @return The input tibble with additional columns containing factor values.
#'         The new columns are prefixed with "fct_".
#'         The first new column contains the fit result in the attribute psych.pca.fit.
#'         The names of the items used for factor analysis are stored in the attribute psych.pca.items.
#'         The summary diagnostics (Bartlett test and KMO) are stored in the attribute psych.kmo.bartlett.
#' @examples
#' library(volker)
#' ds <- volker::chatgpt
#'
#' volker::add_factors(ds, starts_with("cg_adoption"))
#' @export
#' @importFrom rlang .data
add_factors <- function(data, cols, newcols = NULL, k = 2, method = "pca", clean = TRUE) {
  # Check, clean, remove missings
  data <- data_prepare(data, {{ cols }}, cols.numeric = {{ cols }}, clean = clean)

  # Select columns
  items <- dplyr::select(data, {{ cols }})

  # Get the limits
  limits <- get_limits(data, {{ cols }})

  # Cases-to-variables ratio
  fit_ratio <- dplyr::summarise(items, cases = dplyr::n())
  fit_ratio$variables <- ncol(items)
  fit_ratio$ratio <-  fit_ratio$cases / fit_ratio$variables

  # KMO
  fit_kmo <- items %>%
    stats::cor(use="p") %>%
    psych::KMO()

  # Bartlett-Test
  fit_bartlett <- items %>%
    stats::cor(use="p") %>%
    psych::cortest.bartlett(n = nrow(items))

  fit_stats <- tibble::tribble(
    ~"Test", ~"Results",
    "KMO Test", list(
      "Cases" = fit_ratio$cases,
      "Variables" = fit_ratio$variables,
      "Cases-to-Variables Ratio" = sprintf("%.2f", round(fit_ratio$ratio, 2)),
      "Overall MSA" = sprintf("%.2f", round(fit_kmo$MSA, 2))
    ),
    "Bartlett Test", list(
      "Chi-squared" = sprintf("%.2f", round(fit_bartlett$chisq, 2)),
      "df" = round(fit_bartlett$df, 2),
      "p" = sprintf("%.3f", round(fit_bartlett$p.value, 3)),
      "stars" = get_stars(fit_bartlett$p.value)
    )
  )

  fit_stats <- fit_stats |>
    tidyr::unnest_longer(
      tidyselect::all_of("Results"),
      indices_to="Statistic",
      values_to="value",
      transform=as.character
    ) |>
    dplyr::select("Test", "Statistic", "value")


  # Select k if not provided
  fit_parallel <- NULL
  if (is.null(k)) {
    # Calculate eigen values (supress output of fa.parallel)
    utils::capture.output(
      {
        fit_parallel <- psych::fa.parallel(items, fa = "pc", plot = FALSE)
      },
      file = NULL
    )

    k <- fit_parallel$ncomp

    fit_parallel <- tibble::tibble(
      "Component" = c(1:length(fit_parallel$pc.values)),
      "Eigenvalue" = fit_parallel$pc.values
    )

    # Attribute auto k
    attr(fit_parallel, "auto") <- list(
      k = k,
      msg = paste0("Automatically selected k=", k, " by comparing eigenvalues with random data.")
    )

  }

  if (is.null(k)) {
    stop("Could not automatically determine number of factors. Provide a k value, please.")
  }

  # Calculate the factors
  fit <- psych::pca(r = items, nfactors = k, rotate = "varimax", scores = TRUE) %>%
    unclass()

  # Determine column names
  # TODO: What if columns with the new names do already exist?
  prefix <- get_prefix(colnames(items), FALSE, TRUE)
  if (is.null(newcols)) {
    newcols <- paste0("fct_", prefix, "_", c(1:fit$factors))
  }

  # Determine labels
  # newlabel <- codebook(items) %>%
  #   dplyr::distinct(dplyr::across(tidyselect::all_of("item_label"))) %>%
  #   stats::na.omit() %>%
  #   dplyr::pull(.data$item_label) %>%
  #   get_prefix(ignore.case = FALSE, trim = TRUE)

  # if (is.na(newlabel)) {
  #   newlabel <- prefix
  # }
  newlabels <- paste0("Component ", paste0(1:fit$factors))

  # Bind scores
  fit_scores <- tibble::as_tibble(fit$scores)
  colnames(fit_scores) <- newcols
  for (i in c(1:fit$factors)) {
    attr(fit_scores[[i]], "comment") <- newlabels[i]
  }

  data <- dplyr::bind_cols(data, fit_scores)

  # Add fit result to first column attribute
  attr(data[[newcols[1]]], "psych.pca.fit") <- fit
  attr(data[[newcols[1]]], "psych.pca.items") <- colnames(items)
  attr(data[[newcols[1]]], "psych.kmo.bartlett") <- fit_stats
  attr(data[[newcols[1]]], "psych.fa.parallel") <- fit_parallel

  data
}
