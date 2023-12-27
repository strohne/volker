
#' Calculate the mean value of multiple items
#'
#' @param data A dataframe
#' @param newcol Name of the index as a character value
#' @param cols A tidy selection of item columns
#' @param .negative If TRUE, all negative values are recoded to NA
#' @export
add_idx <- function(data, newcol, cols, .negative=FALSE) {

  idx <- data %>%
    dplyr::select({{cols}})

  # TODO: warn if any negative values were recoded
  if (!.negative) {
    idx <- dplyr::mutate(idx, across(where(is.numeric), ~ ifelse(. < 0, NA, .)))
  }

  idx <- idx %>%
    psych::alpha()

  data[[newcol]] <- idx$scores
  attr(data[[newcol]],"psych.alpha") <- idx

  data
}

#' Get number of items and Cronbach's alpha of a scale added by add_idx()
#'
#' @param data A data frame column
#' @return A named list with with the keys "items" and "alpha"
#' @export
get_idx_alpha <- function(data) {
  idx <- attr(data,"psych.alpha")
  if (!is.null(idx)) {
    return (list("items"=idx$nvar, "alpha"=idx$total$std.alpha))
  }
  return (list("items"=NA, "alpha"=NA))
}
