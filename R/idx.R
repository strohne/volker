#' Calculate the mean value of multiple items
#'
#' @param data A dataframe
#' @param cols A tidy selection of item columns
#' @param newcol Name of the index as a character value.
#'              Set to NULL (default) to automatically build a name
#'              from the common column prefix, prefixed with "idx_"
#' @param .negative If TRUE, all negative values are recoded to NA
#' @export
add_idx <- function(data, cols, newcol = NULL, .negative = FALSE) {
  idx <- data %>%
    dplyr::select({{ cols }})

  prefix <- get_prefix(colnames(idx), F, T)
  if (is.null(newcol)) {
    newcol <- paste0("idx_", prefix)
  }

  # Create a label
  newlabel <- get_labels(idx) %>%
    distinct(item_label) %>%
    na.omit() %>%
    pull(item_label) %>%
    get_prefix(F, T) %>%
    paste0(" (Index ", prefix, ")")


  # TODO: warn if any negative values were recoded
  if (!.negative) {
    idx <- dplyr::mutate(idx, across(where(is.numeric), ~ ifelse(. < 0, NA, .)))
  }

  idx <- idx %>%
    psych::alpha(check.keys = T)

  data[[newcol]] <- idx$scores
  attr(data[[newcol]], "psych.alpha") <- idx
  attr(data[[newcol]], "comment") <- newlabel

  # Add limits
  attr(data[[newcol]], "limits") <- get_limits(data, !!cols, .negative)

  # Add scale
  attr(data[[newcol]], "scale") <- data %>%
    get_labels(!!cols) %>%
    distinct(value_name, value_label)

  data
}

#' Get number of items and Cronbach's alpha of a scale added by add_idx()
#'
#' @param data A data frame column
#' @return A named list with with the keys "items" and "alpha"
#' @export
get_idx_alpha <- function(data) {
  idx <- attr(data, "psych.alpha")
  if (!is.null(idx)) {
    return(list("items" = idx$nvar, "alpha" = idx$total$std.alpha))
  }
  return(list("items" = NA, "alpha" = NA))
}
