#' Prepare dataframe for tabs, plots, and index operations
#'
#' The tibble remembers whether it was already cleaned and
#' the cleaning plan is only performed once in the first call.
#'
#' @keywords internal
#'
#' @param data Data frame.
#' @param plan The cleaning plan. By now, only "sosci" is supported. See \link{data_clean_sosci}.
#' @param ... Other parameters passed to the appropriate cleaning function.
#' @return Cleaned data frame with vlkr_df class.
#' @examples
#' ds <- volker::chatgpt
#' ds <- data_clean(ds)
#' @export
data_clean <- function(data, plan = "sosci", ...) {
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
#' @keywords internal
#'
#' @param data Data frame
#' @param remove.na.levels Remove residual values from factor columns.
#'                      Either a character vector with residual values or TRUE to use defaults in \link{VLKR_NA_LEVELS}.
#'                      You can also define or disable residual levels by setting the global option vlkr.na.levels
#'                      (e.g. `options(vlkr.na.levels=c("Not answered"))` or to disable `options(vlkr.na.levels=FALSE)`).
#' @param remove.na.numbers Remove residual values from numeric columns.
#'                      Either a numeric vector with residual values or TRUE to use defaults in \link{VLKR_NA_NUMERIC}.
#'                      You can also define or disable residual values by setting the global option vlkr.na.numbers
#'                      (e.g. `options(vlkr.na.numbers=c(-2,-9))` or to disable `options(vlkr.na.numbers=FALSE)`).
#' @param add.whitespace Add whitespace after slashes for improved label breaks.
#' @return Data frame with vlkr_df class (the class is used to prevent double preparation).
#' @examples
#' ds <- volker::chatgpt
#' ds <- data_clean_sosci(ds)
#' @export
data_clean_sosci <- function(data, remove.na.levels = TRUE, remove.na.numbers = TRUE, add.whitespace = TRUE) {

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
      remove.na.levels <- getOption("vlkr.na.levels")
      if (is.null(remove.na.levels)) {
        remove.na.levels <- VLKR_NA_LEVELS
      } else if (all(remove.na.levels == FALSE)) {
        remove.na.levels <- c()
      }
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
      remove.na.numbers <- getOption("vlkr.na.numbers")
      if (is.null(remove.na.numbers)) {
        remove.na.numbers <- VLKR_NA_NUMERIC
      } else if (all(remove.na.numbers == FALSE)) {
        remove.na.numbers <- c()
      }
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
        function(x) gsub("/", "/\u200B", x, fixed = TRUE)
      )
    )

    data <- dplyr::mutate(
      data,
      dplyr::across(
        tidyselect::where(is.factor),
        function(x) {
          levels(x) <- gsub("/","/\u200B", levels(x), fixed=T)
          return (x)
        }
      )
    )
  }

  # Restore codebook
  data <- labs_restore(data)

  .to_vlkr_df(data)
}

#' Remove missings and output a message
#'
#' @keywords internal
#'
#' @param data Data frame.
#' @param cols A tidy column selection.
#' @return Data frame.
data_rm_missings <- function(data, cols) {

  cases <- sum(is.na(dplyr::select(data, {{ cols }})))

  if (cases > 0) {
    data <- tidyr::drop_na(data, {{ cols }})

    colnames <- rlang::as_label(rlang::enquo(cols))
    data <- .attr_insert(data, "missings", "na", list("cols" = colnames, "n"=cases))
  }

  data
}

#' Remove zero values, drop missings and output a message
#'
#' @keywords internal
#'
#' @param data Data frame.
#' @param cols A tidy column selection.
#' @return Data frame.
data_rm_zeros <- function(data, cols) {

  cases <- sum(dplyr::select(data, {{ cols }}) == 0)

  if (cases > 0) {
    data <- data |>
      labs_store() |>
      dplyr::mutate(dplyr::across({{ cols }}, ~ dplyr::if_else(. == 0, NA, .))) |>
      labs_restore()

    data <- tidyr::drop_na(data, {{ cols }})

    colnames <- rlang::as_label(rlang::enquo(cols))
    data <- .attr_insert(data, "missings", "zero", list("cols" = colnames, "n"=cases))
  }

  data
}

#' Remove negatives and output a warning
#'
#' @keywords internal
#'
#' @param data Data frame
#' @param cols A tidy column selection
#' @return Data frame
data_rm_negatives <- function(data, cols) {

    cases <- sum(dplyr::select(data, {{ cols }}) < 0, na.rm=TRUE)

    if (cases > 0) {
      data <- data |>
        labs_store() |>
        dplyr::mutate(dplyr::across({{ cols }}, ~ ifelse(. < 0, NA, .))) |>
        labs_restore()

      data <- tidyr::drop_na(data, {{ cols }})

      colnames <- rlang::as_label(rlang::enquo(cols))
      data <- .attr_insert(data, "missings", "negative", list("cols" = colnames, "n"=cases))
    }

    data
}

#' Get a formatted baseline for removed zero, negative, and missing cases
#'
#' @keywords internal
#'
#' @param obj An object with the missings attribute.
#' @return A formatted message or NULL if the missings attribute is not present.
get_baseline <- function(obj) {
  missings <- attr(obj, "missings", exact=TRUE)
  if (!is.null(missings)) {
    baseline <- c()
    cols <- c()

    if (!is.null(missings$na)) {
      baseline <- c(baseline, paste0(missings$na$n," missing"))
      cols <- c(cols, missings$na$cols)
    }

    if (!is.null(missings$zero)) {
      baseline <- c(baseline, paste0(missings$zero$n," zero"))
      cols <- c(cols, missings$zero$cols)
    }

    if (!is.null(missings$negative)) {
      baseline <- c(baseline, paste0(missings$negative$n," negative"))
      cols <- c(cols, missings$negative$cols)
    }

    baseline <- paste0(
      paste0(baseline, collapse=", "),
      " case(s) ommited."
    )
  } else {
    baseline <- NULL
  }

  baseline
}

#' Add vlkr_df class - that means, the data frame has been prepared
#'
#' @keywords internal
#'
#' @param data A tibble.
#' @return A tibble of class vlkr_df.
.to_vlkr_df <- function(data, digits = NULL) {
  data <- dplyr::as_tibble(data)
  class(data) <- c("vlkr_df", setdiff(class(data), "vlkr_df"))
  data
}
