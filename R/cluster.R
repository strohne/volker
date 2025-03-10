#
# Functions to calculate cluster
#

#' Get tables for clustering result
#'
#' @keywords internal
#'
#' @description
#' Kmeans clustering is performed using \link{add_clusters}.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A tibble.
#' @param cols A tidy selection of item columns or a single column with cluster values as a factor.
#'             If the column already contains a cluster result from \link{add_clusters}, it is used, and other parameters are ignored.
#'             If no cluster result exists, it is calculated with \link{add_clusters}.
#' @param k Number of clusters to calculate.
#'        Set to NULL to output a scree plot for up to 10 clusters
#'        and automatically choose the number of clusters based on the elbow criterion.
#'        The within-sums of squares for the scree plot are calculated by
#'        \code{stats::\link[stats:kmeans]{kmeans}}.
#' @param newcol Name of the new cluster column as a character vector.
#'                Set to NULL (default) to automatically build a name
#'                from the common column prefix, prefixed with "cls_".
#' @param method The method as character value. Currently, only kmeans is supported.
#'               All items are scaled before performing the cluster analysis using
#'               \code{base::\link[base:scale]{scale}}.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker list with with three volker tabs: cluster centers, cluster counts, and clustering diagnostics.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' cluster_tab(data, starts_with("cg_adoption"), k = 2)
#'
#' @export
#' @importFrom rlang .data
cluster_tab <- function(data, cols, newcol = NULL, k = NULL, method = "kmeans", labels = TRUE, clean = TRUE, ...) {

  clst_col <- dplyr::select(data, {{ cols }})
  fit <- attr(clst_col[[1]], "stats.kmeans.fit")

  # Add cluster
  if (is.null(fit)) {
    scores <- add_clusters(data, {{ cols }}, newcol = newcol, k = k, method = method, clean = clean, ...)
    newcol <- setdiff(colnames(scores), colnames(data))
    result <- cluster_tab(scores,!!sym(newcol), labels = labels, ...)
    return(result)
  }

  # Sum of squares
  # Within-Cluster Sum of Squares & Between-Cluster Sum of Squares
  fit_sos <- tibble::tribble(
    ~Statistic, ~Value,
    "Within-Cluster Sum of Squares", sprintf("%.2f", round(fit$tot.withinss, 2)),
    "Between-Cluster Sum of Squares", sprintf("%.2f", round(fit$betweenss, 2))
  )

  # Cluster means
  cols_items <- attr(clst_col[[1]], "stats.kmeans.items")
  fit_centers <- tab_metrics(data, tidyselect::any_of(cols_items), {{ cols }}, labels = labels, ...)

  # Count cluster
  fit_count <- tab_counts_one(data, {{ cols }}, labels = FALSE)
  colnames(fit_count)[1] <- "Cluster"
  attr(fit_count, "missings") <- NULL

  result <- c(
    "centers" = list(.to_vlkr_tab(fit_centers)),
    "clusters" = list(.to_vlkr_tab(fit_count)),
    "sos" = list(.to_vlkr_tab(fit_sos))
  )

  # Add WSS for scree plot
  fit_wss <- attr(clst_col[[1]], "stats.kmeans.wss")
  if (!is.null(fit_wss)) {
    result <- c(result, "wss" = list(.to_vlkr_tab(fit_wss, caption = "Within-Cluster Sum of Squares for Scree Plot")))
  }

  .to_vlkr_list(result)
}


#' Get plot for clustering result
#'
#' @keywords internal
#'
#' @description
#' Kmeans clustering is performed using \link{add_clusters}.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A tibble.
#' @param cols A tidy selection of item columns or a single column with cluster values as a factor.
#'             If the column already contains a cluster result from \link{add_clusters}, it is used, and other parameters are ignored.
#'             If no cluster result exists, it is calculated with \link{add_clusters}.
#' @param k Number of clusters to calculate.
#'        Set to NULL to output a scree plot for up to 10 clusters
#'        and automatically choose the number of clusters based on the elbow criterion.
#'        The within-sums of squares for the scree plot are calculated by
#'        \code{stats::\link[stats:kmeans]{kmeans}}.
#' @param newcol Name of the new cluster column as a character vector.
#'                Set to NULL (default) to automatically build a name
#'                from the common column prefix, prefixed with "cls_".
#' @param method The method as character value. Currently, only kmeans is supported.
#'               All items are scaled before performing the cluster analysis using
#'               \code{base::\link[base:scale]{scale}}.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' cluster_plot(data, starts_with("cg_adoption"), k = 2)
#'
#' @export
#' @importFrom rlang .data
cluster_plot <- function(data, cols, newcol = NULL, k = NULL, method = NULL, labels = TRUE, clean = TRUE, ...) {

  clst_col <- dplyr::select(data, {{ cols }})
  fit <- attr(clst_col[[1]], "stats.kmeans.fit")

  # Add cluster
  if (is.null(fit)) {
    scores <- add_clusters(data, {{ cols }}, newcol = newcol, k = k, method = method, clean = clean, ...)
    newcol <- setdiff(colnames(scores), colnames(data))
    result <- cluster_plot(scores,!!sym(newcol), labels = labels, ...)
    return(result)
  }

  # Cluster mean plot
  cols_items <- attr(clst_col[[1]], "stats.kmeans.items")
  plot_centers <- plot_metrics(data, tidyselect::all_of(cols_items), {{ cols }}, labels = labels, ...)

  # Prepare output list
  result <- c(
    "centers" = list(plot_centers)
  )

  # 2. Conditionally add scree plot
  fit_wss <- attr(clst_col[[1]], "stats.kmeans.wss")
  if (!is.null(fit_wss)) {
    scree <- .plot_scree(
      fit_wss, k = length(fit$size),
      lab_x = "Number of Clusters k",
      lab_y = "Within-Cluster Sum of Squares"
    )
    result <- c(result, "scree" = list(scree))
  }


  .to_vlkr_list(result)

}

#' Add cluster number to a data frame
#'
#' @description
#' Clustering is performed using \code{stats::\link[stats:kmeans]{kmeans}}.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A dataframe.
#' @param cols A tidy selection of item columns.
#' @param k Number of clusters to calculate.
#'        Set to NULL to output a scree plot for up to 10 clusters
#'        and automatically choose the number of clusters based on the elbow criterion.
#'        The within-sums of squares for the scree plot are calculated by
#'        \code{stats::\link[stats:kmeans]{kmeans}}.
#' @param newcol Name of the new cluster column as a character vector.
#'                Set to NULL (default) to automatically build a name
#'                from the common column prefix, prefixed with "cls_".
#' @param method The method as character value. Currently, only kmeans is supported.
#'               All items are scaled before performing the cluster analysis using
#'               \code{base::\link[base:scale]{scale}}.
#' @param clean Prepare data by \link{data_clean}.
#' @return The input tibble with additional column
#'         containing cluster values as a factor.
#'         The new column is prefixed with "cls_".
#'         The new column contains the fit result in the attribute stats.kmeans.fit.
#'         The names of the items used for clustering are stored in the attribute stats.kmeans.items.
#'         The clustering diagnostics (Within-Cluster and Between-Cluster Sum of Squares) are stored in the attribute stats.kmeans.wss.
#' @examples
#' library(volker)
#' ds <- volker::chatgpt
#'
#' volker::add_clusters(ds, starts_with("cg_adoption"), k = 3)
#' @export
#' @importFrom rlang .data
add_clusters <- function(data, cols, newcol = NULL, k = 2, method = "kmeans", clean = TRUE) {
  # Check, clean, remove missings
  data <- data_prepare(data, {{ cols }}, cols.numeric = {{ cols }}, clean = clean)

  # select columns
  items <- data %>%
    dplyr::select({{ cols }})

  # Determine column name
  prefix <- get_prefix(colnames(items), FALSE, TRUE)
  if (is.null(newcol)) {
    newcol <- paste0("cls_", prefix)
  }

  # Create a label
  newlabel <- codebook(items) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of("item_label"))) %>%
    stats::na.omit() %>%
    dplyr::pull(.data$item_label) %>%
    get_prefix(ignore.case = FALSE, trim = TRUE)

  if (is.na(newlabel)) {
    newlabel <- prefix
  }

  newlabel <- paste0("Cluster: ", prefix)

  # Get the limits
  limits <- get_limits(data, {{ cols }})

  # Select k if not provided
  if (is.null(k)) {
    k <- c(1:10)
  }

  # Perform k-means clustering
  fitlist <- list()
  fit_wss <- c()
  items <- scale(items)
  for (i in k) {
    fit <- stats::kmeans(items, centers = i, iter.max = 10)
    fitlist[[i]] <- fit
    fit_wss <- c(fit_wss, fit$tot.withinss)
  }

  if (length(k) > 1) {
    # Find the elbow: index of the maximum second difference of wss
    if (length(k) > 2) {
      second_diff <- diff(diff(fit_wss))
      k.selected <- k[which.max(abs(second_diff)) + 1]
    } else {
      k.selected <- k[length(k)]
    }

    fit_wss <- tibble::tibble(
      "Clusters k" = k,
      "WSS" = fit_wss
    )

    # Attribute selected k
    attr(fit_wss, "auto") <- list(
      k = k.selected,
      msg = paste0("Automatically selected k=", k.selected, " by the elbow criterion.")
    )

  } else {
    k.selected <- k
    fit_wss <- NULL
  }

  fit <- fitlist[[k.selected]]

  # Add fit result to column attribute
  data[[newcol]] <- factor(paste0("Cluster ", fit$cluster))
  attr(data[[newcol]], "stats.kmeans.fit") <- fit
  attr(data[[newcol]], "stats.kmeans.items") <- colnames(items)
  attr(data[[newcol]], "stats.kmeans.wss") <- fit_wss
  attr(data[[newcol]], "comment") <- newlabel

  # Add limits
  attr(data[[newcol]], "limits") <- limits

  # Add scale
  attr(data[[newcol]], "scale") <- data %>%
    codebook({{ cols }}) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))

  data
}

