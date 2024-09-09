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
#' @param reverse A tidy selection of columns with reversed codings.
#'                For example, if you want to calculate an index of the
#'                two items "I feel bad about this" and "I like it",
#'                both coded with 1=not at all to 5=fully agree,
#'                you need to reverse one of them to make the codings compatible.
#' @param clean Prepare data by \link{data_clean}.
#' @return The input tibble with an additional column that contains the index values.
#'         The column contains the result of the alpha calculation in the attribute named "psych.alpha".
#' @examples
#' ds <- volker::chatgpt
#' volker::idx_add(ds, starts_with("cg_adoption"))
#' @export
#' @importFrom rlang .data
idx_add <- function(data, cols, newcol = NULL, reverse = NULL, clean = TRUE) {

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

  # Get the limits
  limits <- get_limits(data, {{ cols }})

  # Reverse items
  #cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = idx)
  rev_eval <- tidyselect::eval_select(expr = enquo(reverse), data = idx)
  for (rev_col in rev_eval) {
    idx[[rev_col]] <- (limits[2] - idx[[rev_col]]) + limits[1]
  }

  # Calculate the index
  idx <- psych::alpha(idx, check.keys = FALSE, warnings = FALSE)

  data[[newcol]] <- idx$scores
  attr(data[[newcol]], "psych.alpha") <- idx
  attr(data[[newcol]], "reversed") <- names(rev_eval)
  attr(data[[newcol]], "comment") <- newlabel

  # Add limits
  attr(data[[newcol]], "limits") <- limits

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
