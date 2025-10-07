#
# Functions for agreement metrics (reliability, classification performance) ----
#


#' Agreement for multiple items
#'
#' Two types of comparing categories are provided:
#'
#' - Reliability: Compare codings of two or more raters in content analysis.
#'                Usual reliability measures are percent agreement, also known as Hoslti,
#'                Fleiss' or Cohen's Kappa, Krippendorff's Alpha or Gwets AC.
#' - Classification: Compare true and predicted categories from classification methods.
#'                   Common performance metrics include accuracy, precision, recall and F1.
#'
#' @keywords internal
#'
#' @param data A tibble containing item measures, coders and case IDs.
#' @param cols A tidy selection of item variables (e.g. starts_with...) with ratings.
#' @param coders The column holding coders or methods to compare.
#' @param ids The column with case IDs.
#' @param category For classification performance indicators, if no category is provided, macro statistics are returned (along with the number of categories in the output).
#'                 Provide a category to get the statistics for this category only.
#'                 If values are boolean (TRUE / FALSE) and no category is provided, the category is always assumed to be "TRUE".
#' @param method The output metrics, one of `reliability` or `classification`. You can abbreviate it, e.g. `reli` or `class`.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{report_counts}.
#' @return A volker tibble with one row for each item.
#'  The item name is returned in the first column.
#'  For the reliability method, the following columns are returned:
#'
#'  - **n**: Number of cases (each case id is only counted once).
#'  - **Coders**: Number of coders.
#'  - **Categories**: Number of categories.
#'  - **Holsti**: Percent agreement (same as accuracy).
#'  - **Krippendorff' Alpha**: Chance-corrected reliability score.
#'  - **Kappa**: Depending on the number of coders either Cohen's Kappa (two coders) or Fleiss' Kappa (more coders).
#'  - **Gwet's AC1**: Gwet's agreement coefficient.
#'
#'  For the classification method, the following columns are returned:
#'
#'  - **n**: Number of cases (each case id is only counted once)
#'  - **Categories**: Number of categories
#'  - **Accuracy**: Share of correct classifications.
#'  - **Precision**: Share of true cases in all detected true cases.
#'  - **Recall**: Share of true cases detected from all true cases.
#'  - **F1**: Harmonic mean of precision and recall.
#'
#' @examples
#' library(dplyr)
#' library(volker)
#'
#' data <- volker::chatgpt
#'
#' # Prepare example data.
#' # First, recode "x" to TRUE/FALSE for the first coder's sample.
#' data_coder1 <- data |>
#'   mutate(across(starts_with("cg_act_"), ~ ifelse(is.na(.), FALSE, TRUE))) %>%
#'   mutate(coder = "coder one")
#'
#' # Second, recode using a dictionary approach for the second coder's sample.
#' data_coder2 <- data |>
#'   mutate(across(starts_with("cg_act_"), ~ ifelse(is.na(.), FALSE, TRUE))) %>%
#'   mutate(cg_act_write = grepl("write|text|translate", tolower(cg_activities))) %>%
#'   mutate(coder="coder two")
#'
#' data_coded <- bind_rows(
#'   data_coder1,
#'   data_coder2
#' )
#'
#' # Reliability coefficients are strictly only appropriate for manual codings
#' agree_tab(data_coded, cg_act_write,  coder, case, method = "reli")
#'
#' # Better use classification performance indicators to compare the
#' # dictionary approach with human coding
#' agree_tab(data_coded, cg_act_write,  coder, case, method = "class")
#'
#' @export
#' @importFrom rlang .data
agree_tab <- function(data, cols, coders, ids = NULL, category = NULL, method = "reliability", labels = TRUE, clean = TRUE, ...) {
  # 1. Checks, clean, remove missings
  data <- data_prepare(data, {{ cols }}, {{ coders }}, cols.categorical = c({{ cols }}, {{ coders }}), clean = clean)
  method <- check_is_param(method, c("reliability", "classification"), expand = TRUE)

  # 2. Calculate agreement
  result_coef <- .agree_items(data, {{ cols }}, {{ coders }}, ids = {{ ids }}, category = category, method = method, labels = labels)

  # 3. Labels
  result_coef$item2 <- NULL
  if (nrow(result_coef) > 1) {
    result_coef <- .replace_first_col_labels(result_coef)
  } else {
    colnames(result_coef)[1] <- "item"
  }


  # 4. Confusion matrix
  result_conf <- .agree_confusion(data, {{ cols }}, {{ coders }}, ids = {{ ids }}, category = category, labels = labels)

  # 5. Assemble result list
  result <- c(
    "coefficients" = list(.to_vlkr_tab(result_coef, digits = 2)),
    "confusion" = list(.to_vlkr_tab(result_conf, digits = 2))
  )
  result <- .attr_transfer(result, data, "missings")
  .to_vlkr_list(result)
}

#' Generate confusion matrix
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The columns holding codings.
#' @param coders The column holding coders.
#' @param ids The column holding case identifiers.
#' @param category Focus category or null.
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @return A tibble representing a confusion matrix.
#' @importFrom rlang .data
.agree_confusion <- function(data, cols, coders, ids = NULL, category = NULL, labels = TRUE) {

  df_long <- data %>%
    dplyr::select({{ ids }}, {{cols }}, {{coders }}) %>%
    tidyr::pivot_longer({{ cols }}, names_to = ".item", values_to = ".value")

  df_agree <- df_long %>%
    tidyr::pivot_wider(names_from = {{ coders }} , values_from = ".value") %>%

    dplyr::count(dplyr::across(- {{ ids }})) %>%
    dplyr::group_by(.data$.item) %>%
    dplyr::mutate(p = .data$n / sum(.data$n)) %>%
    dplyr::ungroup() %>%

    dplyr::rowwise() %>%
    mutate(.agree = dplyr::n_distinct(dplyr::c_across(-tidyselect::all_of(c(".item", "n", "p")))) == 1) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(.data$.item,-.data$.agree) %>%

    dplyr::select(item = ".item", agree = ".agree", tidyselect::everything())



  df_agree


}

#' Calculate agreement coefficients for multiple items
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @param cols The columns holding codings.
#' @param coders The column holding coders.
#' @param ids The column holding case identifiers.
#' @param category If no category is provided, macro statistics are returned (along with the number of categories in the output).
#'                 Provide a category to get the statistics for this category only.
#'                 If values are boolean (TRUE / FALSE) and no category is provided, the category is always assumed to be "TRUE".
#' @param method The output metrics, one of reliability (reliability scores)
#'               or classification (accuracy, precision, recall, f1).
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @return A tibble with agreement coefficients.
#' @importFrom rlang .data
.agree_items <- function(data, cols, coders, ids = NULL, category = NULL, method = "reliability", labels = TRUE) {

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  coders_eval <- tidyselect::eval_select(expr = enquo(coders), data = data)
  ids_eval <- tidyselect::eval_select(expr = enquo(ids), data = data)


  # Checks
  check_is_param(method, c("reliability", "classification"))
  if (length(ids_eval) != 1) {
    stop(paste0("The ids parameter can only contain one column"), call. = FALSE)
  }

  # Calculate for all items
  result <- expand.grid(
    x = cols_eval, y = coders_eval, z = ids_eval,
    stringsAsFactors = FALSE
  )

  if (method == "reliability") {

    result <- result |>
      dplyr::mutate(
        .test = purrr::pmap(
          list(.data$x, .data$y, .data$z),
          \(x, y, z) .agree_reliability(data[[x]], data[[y]], ids = data[[z]])
        )
      )

  } else if (method == "classification") {
    result <- result |>
      dplyr::mutate(
        .test = purrr::pmap(
          list(.data$x, .data$y, .data$z),
          \(x, y, z) .agree_classification(data[[x]], data[[y]], ids = data[[z]], category = category)
        )
      )
  }

  result <- result %>%
    dplyr::mutate(
      x_name = names(.data$x),
      y_name = names(.data$y)
    ) %>%
    tidyr::unnest_wider(".test") |>
    dplyr::select(-tidyselect::any_of(c("x", "y", "z")))

  result <- dplyr::select(result, item1 = "x_name", item2 = "y_name", tidyselect::everything())
  result <- dplyr::arrange(result, .data$item1, .data$item2)

  # Get variable caption from the attributes
  if (labels) {
    result <- labs_replace(result, "item1", codebook(data, {{ cols }}), col_from="item_name", col_to="item_label")
    result <- labs_replace(result, "item2", codebook(data, {{ coders }}), col_from="item_name", col_to="item_label" )
  }

  result
}


#' Calculate reliability scores
#'
#' @keywords internal
#'
#' @param x Vector with codings.
#' @param y Vector with coders.
#' @param ids Vector with case IDs.
#' @return A list with reliability coefficients.
.agree_reliability <- function(x, y, ids = NULL) {

  # TODO: round in print function, not here

  # For debugging
  #ids <- data_coded$case
  #y <- data_coded$coder
  #x <- data_coded$topic_write

  result <- list()
  df <- data.frame(id = ids, coder = y, value = x, stringsAsFactors = FALSE)

  # Wide format
  mat <- tidyr::pivot_wider(df, names_from = "coder", values_from = "value", names_sort = TRUE)
  mat <- mat[ , -1, drop = FALSE]
  mat <- as.matrix(mat)
  groundtruth <- colnames(mat)[1]

  n_cases <- nrow(mat)
  n_coders <- ncol(mat)
  n_categories <- length(unique(as.vector(mat)))

  holsti <- NA
  if (n_coders == 2) {
    matches <- mat[,1] == mat[,2]
    holsti <- mean(matches, na.rm = TRUE)
  }

  kappa <- NA
  if (n_coders == 2) {
    # Cohen’s Kappa
    tab <- table(mat[,1], mat[,2])
    Po <- sum(diag(tab)) / sum(tab)
    Pe <- sum(rowSums(tab) * colSums(tab)) / (sum(tab)^2)
    kappa <- (Po - Pe) / (1 - Pe)
  } else if (n_coders > 2) {
    # Fleiss’ Kappa
    cats <- unique(as.vector(mat))
    cats <- cats[!is.na(cats)]
    k <- length(cats)

    # rating counts per subject
    n_ij <- sapply(cats, function(c) rowSums(mat == c, na.rm = TRUE))
    N <- nrow(n_ij)
    n <- n_coders

    P_i <- (rowSums(n_ij^2) - n) / (n * (n - 1))
    Po <- mean(P_i)

    p_j <- colSums(n_ij) / (N * n)
    Pe <- sum(p_j^2)

    kappa <- (Po - Pe) / (1 - Pe)
  }

  gwet <- NA
  if (n_coders >= 2) {
    Po_vec <- apply(mat, 1, .pair_agreement)
    Po <- mean(Po_vec, na.rm = TRUE)

    # compute category proportions (π_k)
    all_ratings <- as.vector(mat)
    all_ratings <- all_ratings[!is.na(all_ratings)]
    tab <- table(all_ratings)
    pk <- as.numeric(tab) / sum(tab)   # vector of π_k
    q <- length(pk)

    # Degenerate cases
    if (q == 0) {
      warning("No ratings found.")
    } else {

      # only one category -> no chance-agreement; agreement is trivially 1 if Po==1
      if (q == 1) {
        Pe <- 0
      }
      else {
        Pe <- sum(pk * (1 - pk)) / (q - 1)
      }
      gwet <- (Po - Pe) / (1 - Pe)
    }

  }

  kripp <- NA
  if (n_coders >= 2) {
    Do_vec <- apply(mat, 1, .pair_disagreement)
    Do <- mean(Do_vec, na.rm = TRUE)

    all_vals <- as.vector(mat)
    all_vals <- all_vals[!is.na(all_vals)]
    p <- table(all_vals) / length(all_vals)
    De <- 1 - sum(p^2)

    kripp <- if (!is.na(Do) && De > 0) 1 - Do / De else NA
  }

  result <- list(
    "n" = n_cases,
    "Coders" = n_coders,
    "Categories" = n_categories,
    "Holsti" = ifelse(is.na(holsti), NA, round(holsti, 2)),
    "Krippendorff's Alpha" = ifelse(is.na(kripp), NA, round(kripp, 2)),
    "Kappa" = ifelse(is.na(kappa), NA, round(kappa, 2)),
    "Gwet's AC1" = ifelse(is.na(gwet), NA, round(gwet, 2))
  )

  return (result)
}


#' Calculate classification performance indicators such as precision and recall.
#'
#' @keywords internal
#'
#' @param x Vactor with values.
#' @param y Vector with sources, the first value is taken as ground truth source.
#'          To change the ground truth, use a factor and set the first factor level to the appropriate value.
#' @param ids Vector of  Case IDs.
#' @param category If no category is provided, macro statistics are returned (along with the number of categories in the output).
#'                 Provide a category to get the statistics for this category only.
#'                 If values are boolean (TRUE / FALSE) and no category is provided, the category is always assumed to be "TRUE".
#' @return A list with classification performance indicators.
.agree_classification <- function(x, y, ids = NULL, category = NULL) {

  # TODO: round in print function, not here

  # For debugging
  #ids <- data_coded$case
  #y <- data_coded$coder
  #x <- data_coded$topic_write

  result <- list()
  df <- data.frame(id = ids, coder = y, value = x, stringsAsFactors = FALSE)

  # Wide format
  mat <- tidyr::pivot_wider(df, names_from = "coder", values_from = "value", names_sort = TRUE)
  mat <- mat[ , -1, drop = FALSE]
  mat <- as.matrix(mat)
  groundtruth <- colnames(mat)[1]

  n_cases <- nrow(mat)
  n_coders <- ncol(mat)
  n_categories <- length(unique(as.vector(mat)))



  # TODO: Implement multiple classification sources
  if (n_coders != 2) {
    stop(paste0("Classification performance indicators can only be calculated for exactly two methods. Check your cross column."), call. = FALSE)
  }

  contingency <- table(mat[,1], mat[,2])

  TP <- sum(diag(contingency))
  total <- sum(contingency)

  # Calculate Precision, Recall, F1 for each class and average (macro)
  acc_vec <- precision_vec <- recall_vec <- f1_vec <- numeric(length = nrow(contingency))
  cat_vec <- character(length = nrow(contingency))

  for (i in 1:nrow(contingency)) {
    cat_vec[i] <- rownames(contingency)[i]
    TP_i <- contingency[i, i]
    FP_i <- sum(contingency[-i, i])
    FN_i <- sum(contingency[i, -i])

    precision_vec[i] <- if ((TP_i + FP_i) > 0) TP_i / (TP_i + FP_i) else 0
    recall_vec[i] <- if ((TP_i + FN_i) > 0) TP_i / (TP_i + FN_i) else 0


    if ((precision_vec[i] + recall_vec[i]) > 0) {
      f1_vec[i] <- 2 * precision_vec[i] * recall_vec[i] / (precision_vec[i] + recall_vec[i])
    } else {
      f1_vec[i] <- 0
    }
  }

  categories <- n_categories

  if (is.null(category) & all(as.character(cat_vec) %in% c("TRUE", "FALSE"))) {
    category <- "TRUE"
  }

  if (!is.null(category)) {
    precision_vec <- precision_vec[as.character(cat_vec) == category]
    recall_vec <- recall_vec[as.character(cat_vec) == category]
    f1_vec <- f1_vec[as.character(cat_vec) == category]
    categories <- category
  }


  accuracy <- ifelse(length(f1_vec) > 0, TP / total, NA)
  precision <- mean(precision_vec)
  recall <- mean(recall_vec)
  f1 <- mean(f1_vec)

  result <- list(
    "n" = n_cases,
    #"Coders" = n_coders,
    "Ground truth" = groundtruth,
    "Categories" = categories,
    "Accuracy" = round(accuracy, 2),
    "Precision" = round(precision, 2),
    "Recall" = round(recall, 2),
    "F1" = round(f1, 2)
  )

  return (result)
}


#' Helper: mean of pairwise agreements (nominal)
#'
#' @keywords internal
#'
#' @param vals A matrix with two columns for coders and ratings as values
#' @return The share of agreement.
.pair_agreement <- function(vals) {
  vals <- stats::na.omit(vals)
  m <- length(vals)
  if (m < 2) return(NA)

  pairs <- utils::combn(vals, 2)
  agree <- pairs[1, ] == pairs[2, ]
  mean(agree)
}


#' Helper: mean of pairwise disagreements (nominal)
#'
#' @keywords internal
#'
#' @param vals A matrix with two columns for coders and ratings as values
#' @return The share of disagreement.

.pair_disagreement <- function(vals) {
  vals <- stats::na.omit(vals)
  m <- length(vals)
  if (m < 2) return(NA)

  pairs <- utils::combn(vals, 2)
  dis <- pairs[1, ] != pairs[2, ]
  mean(dis)
}

#' Helper: Remove common prefix from the first column
#'
#' @keywords internal
#'
#' @param result A tibble with item names in the first column
#' @return A tibble with the first column renamed to the prefix and the prefix removed from column values.
.replace_first_col_labels <- function(result) {

  prefix1 <- get_prefix(result[[1]])
  result[[1]] = trim_prefix(result[[1]], prefix1)

  # Rename first column
  if (prefix1 != "") {
    colnames(result)[1] <- trim_label(prefix1)
  }

  result
}
