#' Combine two identically shaped data frames
#' by adding values of each column from the second data frame
#' into the corresponding column in the first dataframe using parentheses
#'
#' TODO: implement newline parameter
#'
#' @keywords internal
#'
#' @param x The first data frame
#' @param y The second data frame
#' @param newline Whether to add a new line character between the values (default: TRUE).
#' @param brackets Whether to set the secondary values in brackets (default: FALSE).
#' @return A combined data frame
zip_tables <- function(x, y, newline = TRUE, brackets = FALSE) {
  newline <- newline && (knitr::is_html_output() || knitr::is_latex_output())
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

#' Transfer an attribute from one to another object
#'
#' @keywords internal
#'
#' @param to The target object.
#' @param from The source object.
#' @param key The attribute key as character value.
#' @return The target object with the updated attribute.
.attr_transfer <- function(to, from, key) {
  attr(to, key) <- attr(from, key, exact=TRUE)
  to
}
