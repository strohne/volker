#' Combine two identically shaped data frames
#' by adding values of each column from the second data frame
#' into the corresponding column in the first dataframe using parentheses
#'
#' @keywords internal
#'
#' @param x The first data frame.
#' @param y The second data frame.
#' @param newline Whether to add a new line character between the values (default: TRUE).
#' @param brackets Whether to set the secondary values in brackets (default: FALSE).
#' @return A combined data frame.
zip_tables <- function(x, y, newline = TRUE, brackets = FALSE) {
  newline <- newline && !is.null(knitr::pandoc_to())
  sep <- ifelse(newline, "\n", " ")

  if (brackets) {
    prefix <- "("
    postfix <- ")"
  } else {
    prefix <- ""
    postfix <- ""
  }

  for (i in 2:ncol(x)) {
    x[[i]] <- paste0(x[[i]], sep, prefix, y[[i]], postfix)
  }
  x
}

#' Insert a name-value-pair into an object attribute
#'
#' @keywords internal
#'
#' @param obj The object.
#' @param key The attribute key.
#' @param name The name of a list item within the attribute.
#' @param value The value of the list item.
#' @return The object with new attributes.
.attr_insert <- function(obj, key, name, value) {
  data <- attr(obj, key, exact = TRUE)
  data[[name]] <- value
  attr(obj, key) <- data
  obj
}

#' Transfer attributes from one to another object
#'
#' @keywords internal
#'
#' @param to The target object.
#' @param from The source object.
#' @param keys A character vector of attribute keys
#' @return The target object with the updated attributes.
.attr_transfer <- function(to, from, keys) {
  for (key in keys) {
    attr(to, key) <- attr(from, key, exact=TRUE)
  }
  to
}

#' Set an attribute value on selected columns of a data frame
#'
#' @keywords internal
#'
#' @param x A data frame containing the columns to modify.
#' @param cols A tidyselect expression specifying which columns to modify
#'   (e.g., \code{c(var1, var2)} or \code{starts_with("score")}).
#' @param attr_name A character string giving the name of the attribute to set.
#' @param attr_value The value to assign to the attribute for all selected columns.
#' @return The data frame with the modified column attributes.
.attr_setcolumn <- function(x, cols, attr_name, attr_value) {
  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = x)

  for (col in names(cols_eval)) {
    attr(x[[col]], attr_name) <- attr_value
  }

  return(x)
}



#' Create a factor vector and preserve all attributes
#'
#' @keywords internal
#'
#' @param x The source value, usually a character vector
#' @param levels The new levels
#' @return A factor vector with the new levels
.factor_with_attr <- function(x, levels=NULL) {
  # Save attributes
  old <- attributes(x)

  # Reorder levels
  x <- factor(x, levels=levels)

  # Restore column label
  old$levels <- levels(x)
  attributes(x) <- old

  x
}

#' Safely convert character to quosure for tidy-eval
#' @param x character vector
#' @return symbol, list of symbols or NULL
.char_to_quosure <- function(x, zerovalue = NULL) {
  if (length(x) == 0) return(zerovalue)
  if (length(x) == 1) return(rlang::sym(x))
  return(rlang::syms(x))
}

.cols_to_symbols <- function(x) {
  # x: character vector
  if (length(x) == 0) return(list())
  rlang::syms(x)   # returns list of symbols
}


#' Convert a named vector to a list
#'
#' @keywords internal
#'
#' @param x A named vector or a list
#' @return Lists are returned as is. Vectors are converted to lists with names as list names.
named.to.list <- function(x) {

  if (is.list(x)) {
    return (x)
  }

  x_names <- as.character(x)
  x_values <- as.character(names(x))
  y = as.list(x_values)
  names(y) <- x_names
  y
}
