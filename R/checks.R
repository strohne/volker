#
# Helper functions to check that parameters conform the expectations
#

#' Check whether the object is a dataframe
#'
#' @param obj The object to test
#' @param msg Optional, a custom error message
#' @param stopit Whether to stop execution with an error message
#' @return boolean Whether the object is a data.frame object
check_dataframe <- function(obj, msg=NULL, stopit=T) {
  check <-tryCatch(
    {
      is.data.frame(obj)
    },
    error = function(e) FALSE
  )

  if (!check && stopit) {
    msg <- dplyr::coalesce(msg, "Check your params: Did you provide a data frame?")
    stop(msg, call. = F)
  }

  check
}

#' Check whether a column exist and stop if not
#'
#' @param data A data frame
#' @param col A column name
#' @param msg A custom error message if the check fails
#' @param stopit If true, the execution stops when a column is missing
#' @return boolean Whether the column exists
has_column <- function(data, col, msg=NULL, stopit = T) {
  colname <- as.character(rlang::get_expr(rlang::enquo(col)))
  check <- colname %in% colnames(data)

  if (!check && stopit) {
    msg <- dplyr::coalesce(msg, "The column ", colname, " does not exist, check your parameters.")
    stop(msg, call. = F)
  }

  check
}
