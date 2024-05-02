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
