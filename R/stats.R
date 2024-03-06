#' Output a five point summary for groups
#'
#' @keywords internal
#'
#' @param data A tibble
#' @param col The column holding metric values
#' @param col_group The column holding groups to compare
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param digits The number of digits to print
#' @param labels If TRUE (default) extracts labels from the attributes, see \link{codebook}.
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Placeholder to allow calling the method with unused parameters from \link{tab_metrics}.
#' @return A volker tibble
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' tab_metrics_one_grouped(data, sd_age, sd_gender)
#'
#' @export
#' @importFrom rlang .data
stats_metrics_one_grouped <- function(data, col, col_group, negative = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
  # 1. Check parameters
  check_is_dataframe(data)


  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # Remove missings
  # TODO: output a warning
  data <- data %>%
    tidyr::drop_na({{ col_group }})

  # Remove negative values
  # TODO: warn if any negative values were recoded
  if (!negative) {
    data <- dplyr::mutate(data, dplyr::across({{ col }}, ~ dplyr::if_else(. < 0, NA, .)))
  }

  data <- data |>
    dplyr::select(av = {{ col }}, uv ={{ col_group }})

  fit <- stats::lm(av ~ uv, data= data)

  result <- broom::tidy(fit)


  # result_grouped <- data %>%
  #   dplyr::group_by({{ col_group }}) %>%
  #   skim_metrics({{ col }}) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::mutate(
  #     {{ col_group }} := tidyr::replace_na(as.character({{ col_group }}), "Missing")
  #   ) %>%
  #   dplyr::select(-tidyselect::all_of(c("skim_variable","skim_type")))
  #
  # result_total <- data %>%
  #   skim_metrics({{ col }}) %>%
  #   dplyr::mutate({{ col_group }} := "Total")
  #
  # result <- dplyr::bind_rows(
  #   result_grouped,
  #   result_total
  # ) %>%
  #   dplyr::select(
  #     {{ col_group }},
  #     min = "numeric.min",
  #     q1 = "numeric.q1",
  #     median = "numeric.median",
  #     q3 = "numeric.q3",
  #     max = "numeric.max",
  #     m = "numeric.mean",
  #     sd = "numeric.sd",
  #     "missing",
  #     "n",
  #     items = "numeric.items",
  #     alpha = "numeric.alpha"
  #   )
  #
  # # Remove items and alpha if not and index
  # if (all(is.na(result$items)) || all(is.na(result$alpha))) {
  #   result$items <- NULL
  #   result$alpha <- NULL
  # } else {
  #   result <- result %>%
  #     dplyr::mutate(dplyr::across(tidyselect::all_of("items"), ~ as.character(round(., 0)))) %>%
  #     dplyr::mutate(dplyr::across(tidyselect::all_of("alpha"), ~ as.character(round(., 2))))
  # }
  #
  # # Get item label from the attributes
  # if (labels) {
  #   codes <- data %>%
  #     codebook({{ col_group }}) %>%
  #     dplyr::distinct(dplyr::across(tidyselect::all_of(c("item_name", "item_label")))) %>%
  #     stats::na.omit()
  #
  #   if (nrow(codes) > 0) {
  #     label <- codes$item_label[1]
  #     result <- result %>%
  #       dplyr::rename({{ label }} := {{ col_group }})
  #   }
  #
  #   scale <- attr(dplyr::pull(data, {{ col }}), "scale")
  #   if (is.null(scale)) {
  #     scale <- data %>%
  #       codebook({{ col }}) %>%
  #       dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))
  #   }
  #   attr(result, "scale")
  # }

  .to_vlkr_tab(result, digits= digits)
}
