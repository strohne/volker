#' Prepare dataframe for tabs, plots, and index operations
#'
#' The tibble remembers whether it was already cleaned and
#' the cleaning plan is only performed once in the first call.
#'
#' @param data Data frame
#' @param plan The cleaning plan. By now, only "sosci" is supported. See \link{data_clean_sosci}.
#' @param ... Other parameters passed to the appropriate cleaning function
#' @return Cleaned data frame with vlkr_df class
#' @examples
#' ds <- volker::chatgpt
#' ds <- data_clean(ds)
#' @export
data_clean <- function(data, plan="sosci", ...) {
  if (plan == "sosci") {
    data <- data_clean_sosci(data,...)
  }
  data
}

#' Prepare data originating from SoSci Survey
#'
#' The tibble remembers whether it was already prepared and
#' the operations are only performed once in the first call.
#'
#' Prepares SoSci Survey data:
#' - Remove the avector class from all columns
#'   (comes from SoSci and prevents combining vectors)
#' - Recode residual factor values to NA (e.g. "[NA] nicht beantwortet")
#' - Recode residual numeric values to NA (e.g. -9)
#' - Add whitespace after slashes to better label breaks
#'
#' @param data Data frame
#' @param remove.na.levels Remove residual values from factor columns.
#'                      Either a character vector with residual values or TRUE to use defaults in \link{VLKR_NA_LEVELS}
#' @param remove.na.numbers Remove residual values from numeric columns.
#'                      Either a numeric vector with residual values or TRUE to use defaults in \link{VLKR_NA_NUMERIC}
#' @param add.whitespace Add whitespace after slashes for improved label breaks
#' @return Data frame with vlkr_df class (the class is used to prevent double preparation)
#' @examples
#' ds <- volker::chatgpt
#' ds <- data_clean_sosci(ds)
#' @export
data_clean_sosci <- function(data, remove.na.levels = T, remove.na.numbers = T, add.whitespace=T) {

  # Prepare only once
  if ("vlkr_df" %in% class(data)) {
    return (data)
  }

  # Remove avector class
  for (i in c(1:ncol(data))) {
    class(data[[i]]) <- setdiff(class(data[[i]]), "avector")
  }

  # Store codebook before mutate operations
  data <- labs_store(data)

  # Remove residual levels such as "[NA] nicht beantwortet"
  if (remove.na.levels != FALSE) {
    if (is.logical(remove.na.levels)) {
      remove.na.levels <- VLKR_NA_LEVELS
    }

    data <- dplyr::mutate(
      data,
      dplyr::across(
        tidyselect::where(~ is.factor(.)),
        ~ replace(., . %in% remove.na.levels, NA)
      )
    )
  }

  # Remove residual numbers such as -9
  if (remove.na.numbers != FALSE) {
    if (is.logical(remove.na.numbers)) {
      remove.na.numbers <- VLKR_NA_NUMERIC
    }

    data <- dplyr::mutate(
      data,
      dplyr::across(
        dplyr::where(is.numeric),
        ~ dplyr::if_else(. %in% remove.na.numbers, NA, .)
      )
    )
  }

  # Add whitespace for better breaks
  if (add.whitespace) {
    data <- dplyr::mutate(
      data,
      dplyr::across(
        tidyselect::where(is.character),
        ~ stringr::str_replace_all(., stringr::fixed("/"), "/\u200B")
      )
    )

    data <- dplyr::mutate(
      data,
      dplyr::across(
        tidyselect::where(is.factor),
        ~ forcats::fct_relabel(., ~ stringr::str_replace_all(., stringr::fixed("/"), "/\u200B"))
      )
    )
  }

  # Restore codebook
  data <- labs_restore(data)

  .to_vlkr_df(data)
}


#' Add vlkr_df class - that means, the data frame has been prepared
#'
#' @keywords internal
#'
#' @param data A tibble
#' @return A tibble of class vlkr_df
.to_vlkr_df <- function(data, digits=NULL) {
  data <- dplyr::as_tibble(data)
  class(data) <- c("vlkr_df", setdiff(class(data), "vlkr_df"))
  data
}
