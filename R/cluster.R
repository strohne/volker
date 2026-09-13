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


  # Within-Cluster Sum of Squares & Between-Cluster Sum of Squares
  method <- dplyr::coalesce(attr(clst_col[[1]], "stats.cluster.method"), "kmeans")
  ss_label <- if (method == "kmeans") "Sum of Squares" else "Sum of Squares (Gower distance)"

  fit_sos <- tibble::tribble(
    ~Statistic, ~Value,
    paste0("Within-Cluster ", ss_label), sprintf("%.2f", round(fit$tot.withinss, 2)),
    paste0("Between-Cluster ", ss_label), sprintf("%.2f", round(fit$betweenss, 2))
  )

  # Cluster means
  cols_items <- attr(clst_col[[1]], "stats.kmeans.items")
  if (method == "kmeans") {
    fit_centers <- tab_metrics(data, tidyselect::any_of(cols_items), {{ cols }}, labels = labels, ...)
  } else {
    fit_centers <- tab_counts(data, tidyselect::any_of(cols_items), {{ cols }}, labels = labels, ...)
  }

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
#' @param reorder Reorder items to minimize line crossings,
#'   Either `TRUE` to automatically select a
#'   method (`"olo"` if \pkg{seriation} is installed, otherwise `"min"`), or
#'   one of the character values `"max"`, `"min"`, `"spread"`, `"gw"`, or
#'   `"olo"`. Defaults to `FALSE` which disables reordering.
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
cluster_plot <- function(data, cols, newcol = NULL, k = NULL, method = NULL, reorder = TRUE, labels = TRUE, clean = TRUE, ...) {

  clst_col <- dplyr::select(data, {{ cols }})
  fit <- attr(clst_col[[1]], "stats.kmeans.fit")

  # Add cluster
  if (is.null(fit)) {
    scores <- add_clusters(data, {{ cols }}, newcol = newcol, k = k, method = method, clean = clean, ...)
    newcol <- setdiff(colnames(scores), colnames(data))
    result <- cluster_plot(scores,!!sym(newcol), reorder = reorder, labels = labels, ...)
    return(result)
  }

  # Method
  method <- dplyr::coalesce(attr(clst_col[[1]], "stats.cluster.method"), "kmeans")

  # Cluster mean plot
  cols_items <- attr(clst_col[[1]], "stats.kmeans.items")
  if (method == "kmeans") {
    plot_centers <- plot_metrics(data, tidyselect::all_of(cols_items), {{ cols }}, reorder = reorder, labels = labels, ...)
  } else {
    plot_centers <- plot_counts(data, tidyselect::all_of(cols_items), {{ cols }}, reorder = reorder, labels = labels, ...)
  }

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
#' Clustering is performed using \code{stats::\link[stats:kmeans]{kmeans}}
#' (method = "kmeans") or \code{cluster::\link[cluster:pam]{pam}} on a Gower
#' dissimilarity matrix computed by \code{cluster::\link[cluster:daisy]{daisy}}
#' (method = "pam").
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A dataframe.
#' @param cols A tidy selection of item columns.
#' @param k Number of clusters to calculate.
#'        Set to NULL to output a scree plot for up to 10 clusters
#'        and automatically choose the number of clusters based on the elbow criterion.
#' @param newcol Name of the new cluster column as a character vector.
#'                Set to NULL (default) to automatically build a name
#'                from the common column prefix, prefixed with "cls_".
#' @param method The method as character value. One of "kmeans" (default) or "pam".
#'               For "kmeans" all items are scaled using
#'               \code{base::\link[base:scale]{scale}} and euclidean distance is used.
#'               For "pam" a Gower dissimilarity matrix is used, which supports
#'               mixed data types (numeric and categorical) and normalises each
#'               variable internally, so no scaling is applied.
#' @param labels Whether to get the label of the cluster column from the common prefix of item column labels.
#' @param clean Prepare data by \link{data_clean}.
#' @return The input tibble with an additional cluster column (factor, prefixed "cls_").
#'         The fit result is stored in the attribute stats.kmeans.fit, the item names in
#'         stats.kmeans.items, the scree-plot data in stats.kmeans.wss and the method in
#'         stats.cluster.method.
#' @examples
#' library(volker)
#' ds <- volker::chatgpt
#'
#' volker::add_clusters(ds, starts_with("cg_adoption"), k = 3)
#' volker::add_clusters(ds, starts_with("cg_adoption"), k = 3, method = "pam")
#' @export
#' @importFrom rlang .data
#' @importFrom cluster daisy pam
add_clusters <- function(data, cols, newcol = NULL, k = 2, method = "kmeans", labels = TRUE, clean = TRUE) {

  method <- match.arg(method, c("kmeans", "pam"))

  # Check, clean, remove missings
  # For kmeans all items must be numeric; for pam/gower mixed types are allowed.
  if (method == "kmeans") {
    data <- data_prepare(data, {{ cols }}, cols.numeric = {{ cols }}, clean = clean)
  } else {
    data <- data_prepare(data, {{ cols }}, cols.categorical = {{ cols }}, clean = clean)
  }

  # For cluster analysis, always remove missings
  if (!dplyr::coalesce(getOption("vlkr.na.omit"), VLKR_NA_OMIT)) {
    data <- data_rm_missings(data, {{ cols }}, force = TRUE)
  }

  # Select columns
  items <- data %>%
    dplyr::select({{ cols }})

  # Determine column name
  prefix <- get_prefix(colnames(items), FALSE, TRUE)
  if (is.null(newcol)) {
    newcol <- paste0("cls_", prefix)
  }

  # Create a label
  newlabel <- NA
  if (labels) {
    newlabel <- codebook(items) %>%
      dplyr::distinct(dplyr::across(tidyselect::all_of("item_label"))) %>%
      stats::na.omit() %>%
      dplyr::pull(.data$item_label) %>%
      get_prefix(ignore.case = FALSE, trim = TRUE)
  }

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

  # Prepare the feature representation depending on the method
  if (method == "kmeans") {
    features <- scale(items)
    itemnames <- colnames(features)
  } else if (method == "pam") {
    # Treat all items as categorical: convert numerics to factors
    items <- dplyr::mutate(items, dplyr::across(tidyselect::everything(), as.factor))

    # Gower distance
    features <- cluster::daisy(items, metric = "gower")
    itemnames <- colnames(items)
  }

  # Fit for each requested number of clusters
  fitlist <- vector("list", max(k))
  fit_wss <- c()

  for (i in k) {

    if (method == "kmeans") {
      fit <- stats::kmeans(features, centers = i, iter.max = 10)
    } else if (method == "pam") {
      fit <- .cluster_pam(features, k = i)
    }

    fitlist[[i]] <- fit
    fit_wss <- c(fit_wss, fit$tot.withinss)
  }

  # Select k
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
  # (attribute names kept as "stats.kmeans.*" for compatibility with cluster_tab/cluster_plot)
  data[[newcol]] <- factor(paste0("Cluster ", fit$cluster))
  attr(data[[newcol]], "stats.kmeans.fit")    <- fit
  attr(data[[newcol]], "stats.kmeans.items")  <- itemnames
  attr(data[[newcol]], "stats.kmeans.wss")    <- fit_wss
  attr(data[[newcol]], "stats.cluster.method") <- method
  attr(data[[newcol]], "comment") <- newlabel

  # Add limits
  attr(data[[newcol]], "limits") <- limits

  # Add scale
  attr(data[[newcol]], "scale") <- data %>%
    codebook({{ cols }}) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))

  data
}

#' Sum-of-squares decomposition for a dissimilarity matrix
#'
#' @keywords internal
#'
#' @description
#' Uses the Huygens decomposition of total dispersion for an arbitrary
#' dissimilarity matrix, so that within- and between-cluster "sum of squares"
#' can be reported for distance-based methods such as PAM with Gower distance.
#'
#' @param dissim A dist object.
#' @param clustering An integer vector with cluster assignments.
#' @return A list with tot.withinss, betweenss and totss.
.cluster_dist_ss <- function(dissim, clustering) {
  d2 <- as.matrix(dissim)^2
  n <- nrow(d2)
  totss <- sum(d2[upper.tri(d2)]) / n

  withinss <- 0
  for (cl in unique(clustering)) {
    idx <- which(clustering == cl)
    nk <- length(idx)
    if (nk > 1) {
      dk <- d2[idx, idx, drop = FALSE]
      withinss <- withinss + sum(dk[upper.tri(dk)]) / nk
    }
  }

  list(
    tot.withinss = withinss,
    betweenss = totss - withinss,
    totss = totss
  )
}

#' Fit PAM clustering with a kmeans-like return structure
#'
#' @keywords internal
#'
#' @description
#' Wraps \code{cluster::\link[cluster:pam]{pam}} on a precomputed dissimilarity
#' matrix and normalises the result to the structure returned by
#' \code{stats::\link[stats:kmeans]{kmeans}}, so it can be used interchangeably
#' in the clustering workflow. For k = 1 a trivial one-cluster solution is built
#' (PAM itself requires k >= 2), which allows the scree plot to start at k = 1.
#'
#' Within- and between-cluster sum of squares are derived from the dissimilarity
#' matrix via \link{.cluster_dist_ss}.
#'
#' @param dissim A dist object (e.g. from \code{cluster::\link[cluster:daisy]{daisy}}).
#' @param k Number of clusters.
#' @return A list mimicking a kmeans fit with elements cluster, size,
#'         tot.withinss, betweenss and totss. For k >= 2 it additionally
#'         contains medoids, avg.silwidth and the raw pam object.
#' @importFrom cluster pam
.cluster_pam <- function(dissim, k) {

  n <- attr(dissim, "Size")

  if (k == 1) {
    ss <- .cluster_dist_ss(dissim, rep(1L, n))
    return(list(
      cluster      = rep(1L, n),
      size         = n,
      tot.withinss = ss$tot.withinss,
      betweenss    = ss$betweenss,
      totss        = ss$totss
    ))
  }

  fit_pam <- cluster::pam(dissim, k = k, diss = TRUE)
  ss <- .cluster_dist_ss(dissim, fit_pam$clustering)

  list(
    cluster      = as.integer(fit_pam$clustering),
    size         = as.integer(fit_pam$clusinfo[, "size"]),
    tot.withinss = ss$tot.withinss,
    betweenss    = ss$betweenss,
    totss        = ss$totss,
    medoids      = fit_pam$medoids,
    avg.silwidth = fit_pam$silinfo$avg.width,
    pam          = fit_pam
  )
}
