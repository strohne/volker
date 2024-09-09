#
# Helper functions to check that parameters conform the expectations
#

#' Check whether the object is a dataframe
#'
#' @keywords internal
#'
#' @param obj The object to test.
#' @param msg Optional, a custom error message.
#' @param stopit Whether to stop execution with an error message.
#' @return boolean Whether the object is a data.frame object.
check_is_dataframe <- function(obj, msg = NULL, stopit = TRUE) {
  check <-tryCatch(
    {
      is.data.frame(obj)
    },
    error = function(e) { FALSE }
  )

  if (!check && stopit) {
    msg <- dplyr::coalesce(msg, "Check your params: Did you provide a data frame?")
    stop(msg, call. = FALSE)
  }

  check <- (nrow(obj) * ncol(obj)) > 0

  if (!check && stopit) {
    msg <- dplyr::coalesce(msg, "Check your data: Are they empty?")
    stop(msg, call. = FALSE)
  }


  check
}

#' Check whether a column exist and stop if not
#'
#' @keywords internal
#'
#' @param data A data frame.
#' @param cols A tidyselection of columns.
#' @param msg A custom error message if the check fails.
#' @return boolean Whether the column exists.
check_has_column <- function(data, cols, msg = NULL) {

  colexpr <- as.character(rlang::get_expr(rlang::enquo(cols)))
  if (!all(colexpr != "")) {
    msg <- dplyr::coalesce(msg, paste0("Did you miss to say which column to use?"))
    stop(msg, call. = FALSE)
  }

  coleval <- tryCatch(
    {
      tidyselect::eval_select(expr = rlang::enquo(cols), data = data)
    },
    error = function(e) {
      msg <- dplyr::coalesce(msg, paste0("The column selection is not valid, check your parameters."))
      stop(msg, call. = FALSE)

    }
  )

  if (length(coleval) == 0) {
    msg <- dplyr::coalesce(msg, paste0("The column selection does not exist, check your parameters."))
    stop(msg, call. = FALSE)
  }

  TRUE
}

#' Check whether a parameter value is from a valid set
#'
#' @keywords internal
#'
#' @param value A character value.
#' @param allowed Allowed values.
#' @param stopit Whether to stop execution if the value is invalid.
#' @param msg A custom error message if the check fails.
#' @return logical whether method is valid.
check_is_param <- function(value, allowed, stopit = TRUE, msg = NULL) {

  check <- tryCatch(
    {
      value %in% allowed
    },
    error = function(e) { FALSE }
  )

  if (!check && stopit) {
    msg <- dplyr::coalesce(
      msg,
      paste0("Check your parameters: The value '", value, "' is not supported. Supported values are ", paste0(allowed, collapse=", "), ".")
    )
    stop(msg, call. = FALSE)
  }

  check
}
