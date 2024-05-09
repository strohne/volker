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
#' @param negative If FALSE (default), negative values are recoded as missing values.
#' @param clean Prepare data by \link{data_clean}.
#' @return The input tibble with an additional column that contains the index values.
#'         The column contains the result of the alpha calculation in the attribute named "psych.alpha".
#' @examples
#' ds <- volker::chatgpt
#' volker::idx_add(ds, starts_with("cg_adoption"))
#' @export
#' @importFrom rlang .data
idx_add <- function(data, cols, newcol = NULL, negative = FALSE, clean = TRUE) {

  # 1. Checks
  check_is_dataframe(data)

  # 2. Clean
  if (clean) {
    data <- data_clean(data)
  }


  # Remove missings
  data <- data_rm_missings(data, {{ cols }})

  # Remove negative values
  if (!negative) {
    data <- data_rm_negatives(data, {{ cols }})
  }


  idx <- data %>%
    dplyr::select({{ cols }})

  prefix <- get_prefix(colnames(idx), FALSE, TRUE)
  if (is.null(newcol)) {
    newcol <- paste0("idx_", prefix)
  }

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

  idx <- idx %>%
    psych::alpha(check.keys = TRUE)

  data[[newcol]] <- idx$scores
  attr(data[[newcol]], "psych.alpha") <- idx
  attr(data[[newcol]], "comment") <- newlabel

  # Add limits
  attr(data[[newcol]], "limits") <- get_limits(data, {{ cols }}, negative)

  # Add scale
  attr(data[[newcol]], "scale") <- data %>%
    codebook({{ cols }}) %>%
    dplyr::distinct(dplyr::across(tidyselect::all_of(c("value_name", "value_label"))))

  data
}

#' Get number of items and Cronbach's alpha of a scale added by idx_add()
#'
#' @keywords internal
#'
#' @param data A data frame column.
#' @return A named list with with the keys "items" and "alpha".
idx_alpha <- function(data) {
  idx <- attr(data, "psych.alpha")
  if (!is.null(idx)) {
    return(list("items" = idx$nvar, "alpha" = idx$total$std.alpha))
  }
  return(list("items" = NA, "alpha" = NA))
}
