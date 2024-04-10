#' Output a regression table
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
stat_metrics_one_grouped <- function(data, col, col_group, negative = FALSE, digits = 1, labels = TRUE, clean = TRUE, ...) {
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

  lm <- data |>
    dplyr::select(av = {{ col }}, uv ={{ col_group }})

  fit <- stats::lm(av ~ uv, data = lm)

  result <- broom::tidy(fit)

  # Add macro statistics
  # TODO: .

  # result_macro <- broom::glance(fit)

  # Rename result labels
  # TODO: .

  .to_vlkr_tab(result,digits= digits)

}
