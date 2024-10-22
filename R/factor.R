#' Output a Principal component analysis along with KMO and Bartlett test
#'
#' PCA is performed using \code{psych::\link[psych:pca]{pca}}.
#' Bartlett's test for sphericity is calculated with \code{psych::\link[psych:cortest.bartlett]{cortest.bartlett}}.
#' The Kaiser-Meyer-Olkin (KMO) measure is computed using \code{psych::\link[psych:KMO]{KMO}}.
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The columns holding metric values.
#' @param factors The desired number of factors.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{plot_metrics}.
#' @return A ggplot object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_factor(data, starts_with("cg_adoption"), factors = 3)
#'
#' @export
#' @importFrom rlang .data
tab_factor <- function(data, cols, factors = NULL, labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, clean = clean)

  data <- data %>%
    dplyr::select({{ cols }})

  # 2. Assumptions
  if (is.null(factors)) {
    stop("Check your parameters: Number of factors is missing.")
  }

  # Cases-to-variables ratio
  ratio <- data %>%
    dplyr::summarise(
      cases = dplyr::n(),
      variables = ncol(.),
      ratio = cases / variables
    )

  # KMO
  KMO <- data %>%
    stats::cor(use="p") %>%
    psych::KMO()

  KMO <- tibble::tribble(
    ~Statistic, ~Value,
    "Cases-to-Variables Ratio", sprintf("%.2f", round(ratio$ratio, 2)),
    "Overall MSA", sprintf("%.2f", round(KMO$MSA, 2))
  )

  # Bartlett-Test
  bartlett <- data %>%
    stats::cor(use="p") %>%
    psych::cortest.bartlett(n = nrow(data))

  bartlett <- bartlett %>%
    tibble::enframe(
      name = "Bartlett Test",
      value = "value"
    )

  bartlett <- bartlett %>%
    dplyr::mutate(value = purrr::map_dbl(value, ~ round(.x, 2)))

  # 3. Perform  Factor analysis
  result <- data %>%
    dplyr::select({{ cols }}) %>%
    psych::pca(r = ., nfactors = factors, rotate = "varimax") %>%
    unclass()

  # Extract labels
  communal <- tibble::as_tibble(as.data.frame(result$communality), rownames = "item")
  loadings <- tibble::as_tibble(unclass(result$loadings), rownames = "item")

  result <- loadings %>%
    dplyr::left_join(communal, by = "item") %>%
    dplyr::rename(communality = `result$communality`) %>%
    dplyr::arrange(dplyr::across(tidyselect::starts_with("RC"), ~ dplyr::desc(abs(.))))

  # 4. Get labels
  if (labels) {
    result <- labs_replace(
      result, "item",
      codebook(data, {{ cols }}),
      "item_name", "item_label"
    )
  }

  prefix <- get_prefix(result$item, trim=T)
  result <- dplyr::mutate(result, item = trim_prefix(.data$item, prefix))

  # Rename first columns
  if (prefix == "") {
    prefix <- "Item"
  }

  colnames(result)[1] <- prefix

  result <- c(
    list(.to_vlkr_tab(bartlett)),
    list(.to_vlkr_tab(KMO)),
    list(.to_vlkr_tab(result))
  )

  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_list(result)
}
