#
# Functions to handle indexes
#

#' Calculate the mean value of multiple items
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A dataframe.
#' @param cols A tidy selection of item columns.
#' @param newcol Name of the index as a character value.
#'              Set to NULL (default) to automatically build a name
#'              from the common column prefix, prefixed with "idx_".
#' @param clean Prepare data by \link{data_clean}.
#' @return The input tibble with an additional column that contains the index values.
#'         The column contains the result of the alpha calculation in the attribute named "psych.alpha".
#' @examples
#' ds <- volker::chatgpt
#' volker::add_index(ds, starts_with("cg_adoption"))
#' @export
#' @importFrom rlang .data
add_index <- function(data, cols, newcol = NULL, clean = TRUE) {

  # 1. Checks
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }

  # 3. Remove missings
  data <- data_rm_missings(data, {{ cols }})

  # Select columns
  idx <- data %>%
    dplyr::select({{ cols }})

  # Determine column name
  prefix <- get_prefix(colnames(idx), FALSE, TRUE)
  if (is.null(newcol)) {
    newcol <- paste0("idx_", prefix)
  }

  # Get limits
  limits <- get_limits(data, {{ cols }})

  # Create a label
  newlabel <- codebook(idx) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of("item_label"))) %>%
    stats::na.omit() %>%
    dplyr::pull(.data$item_label) %>%
    get_prefix(ignore.case = FALSE, trim = TRUE)

  if (is.na(newlabel)) {
    newlabel <- prefix
  }
  newlabel <- paste0("Index: ", prefix)

  # Calculate the index
  idx <- psych::alpha(idx, check.keys = FALSE, warnings = FALSE)

  data[[newcol]] <- idx$scores
  attr(data[[newcol]], "psych.alpha") <- idx
  attr(data[[newcol]], "comment") <- newlabel

  # Add limits
  attr(data[[newcol]], "limits") <- limits

  # Add scale
  attr(data[[newcol]], "scale") <- data %>%
    codebook({{ cols }}) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))

  data
}

#' Get number of items and Cronbach's alpha of a scale added by add_index()
#'
#' TODO: Rename to index_tab, return volker list as in factor_tab()
#'
#' @keywords internal
#'
#' @param data A data frame column.
#' @return A named list with with the keys "items" and "alpha".
get_alpha <- function(data) {
  idx <- attr(data, "psych.alpha")
  if (!is.null(idx)) {
    return(list("items" = idx$nvar, "alpha" = idx$total$std.alpha))
  }
  return(list("items" = NA, "alpha" = NA))
}


#' Deprecated Alias for `add_index`
#'
#' This function is a deprecated alias for `add_index`.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `idx_add()` was renamed to `add_index()`.
#'
#' @keywords internal
#' @export
idx_add <- function(data, cols, newcol = NULL, reverse = NULL, clean = TRUE) {

  lifecycle::deprecate_warn("3.0.0", "idx_add()", "add_index()")

  # Reverse items
  if (!missing(reverse)) {
    data <- data_rev(data, reverse)
  }

  add_index(
    data = data,
    cols = tidyselect::all_of(cols),
    newcol = newcol,
    clean = clean)

}
