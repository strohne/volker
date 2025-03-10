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

#' Check whether a column selection is numeric
#'
#' @keywords internal
#'
#' @param data A data frame.
#' @param cols A tidyselection of columns.
#' @param msg A custom error message if the check fails.
#' @return boolean Whether the columns are numeric.
check_is_numeric <- function(data, cols, msg = NULL) {

  cols <- tidyselect::eval_select(expr = rlang::enquo(cols), data = data)
  vals <- dplyr::select(data, tidyselect::all_of(cols))
  all_numeric <- all(sapply(vals, is.numeric))

  if (!all_numeric) {
    msg <- dplyr::coalesce(msg, paste0("The column selection contains non-numeric values. Did you forget to convert them?"))
    stop(msg, call. = FALSE)
  }

  TRUE
}

#' Check whether a column selection is categorical
#'
#' @keywords internal
#'
#' @param data A data frame.
#' @param cols A tidyselection of columns.
#' @param msg A custom error message if the check fails.
#' @return boolean Whether the columns are categorical
check_is_categorical <- function(data, cols, msg = NULL) {

  cols <- tidyselect::eval_select(expr = rlang::enquo(cols), data = data)
  vals <- dplyr::select(data, tidyselect::all_of(cols))

  all_nonfraction <- all(sapply(vals, function(column) {
    !is.numeric(column) || all(column == floor(column), na.rm = TRUE)
  }))

  if (!all_nonfraction) {
    msg <- dplyr::coalesce(msg, paste0("The column selection contains numeric values with commas. Did you mean to use a metric function?"))
    stop(msg, call. = FALSE)
  }

  all_numeric <- all(sapply(vals, is.numeric))
  categories <- unique(unlist(vals, use.names = FALSE))

  max_categories <- getOption("vlkr.max.categories")
  if (is.null(max_categories)) {
    max_categories <- VLKR_MAX_CATEGORIES
  }

  n_categories <- length(categories)
  if (all_numeric && (n_categories > max_categories)) {
    msg <- dplyr::coalesce(
      msg,
      paste0(
        "The column selection contains more than ",
        max_categories," distinct numeric values.\n",
        "Did you mean to use a metric function?\n",
        "Otherwise you can increase the number of reported categories by ",
        "`options(vlkr.max.categories=", n_categories ,")`."
      )
    )
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
#' @param allownull Whether to allow NULL values.
#' @param allowmultiple Whether to allow multiple values.
#' @param stopit Whether to stop execution if the value is invalid.
#' @param msg A custom error message if the check fails.
#' @return logical whether method is valid.
check_is_param <- function(value, allowed, allownull = FALSE, allowmultiple = FALSE, stopit = TRUE, msg = NULL) {

  # Check for null
  if (is.null(value)) {
    if (allownull) {
      return(TRUE)
    }

    if (stopit) {
      arg <- deparse(substitute(value))
      stop(paste0("The parameter '", arg, "' cannot be NULL."), call. = FALSE)
    }
    return(FALSE)
  }

  # Check for multiple values
  if (!allowmultiple && length(value) > 1) {
    if (stopit) {
      stop(paste0("Only a single value is allowed, but multiple values were provided: ", paste(value, collapse = ", "), "."), call. = FALSE)
    }
    return(FALSE)
  }

  check <- tryCatch(
    {
      value %in% allowed
    },
    error = function(e) { FALSE }
  )

  # Check if all values are valid
  if (!all(check)) {
    if (stopit) {
      invalid_values <- paste(value[!check], collapse = ", ")
      msg <- dplyr::coalesce(
        msg,
        paste0("Check your parameters: The value(s) '", invalid_values, "' are not supported. Supported values are: ", paste0(allowed, collapse = ", "), ".")
      )
      stop(msg, call. = FALSE)
    }
    return(FALSE)
  }

  TRUE
}

