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

#' Get the p value of an lm model
#'
#' @keywords internal
#'
#' @param fit The result of an lm() call
#' @return The p value of the model
lm_pvalue <- function(fit) {
  fstat <- summary(fit)$fstatistic
  stats::pf(fstat[1], fstat[2], fstat[3], lower.tail = FALSE)
}

#' Get significance stars from p values
#'
#' @keywords internal
#'
#' @param x A vector of p values
#' @return A character vector with significance stars
get_stars <- function(x) {
  sapply(x, function(p) {
    if (p < 0.001) {
      return("***")
    } else if (p < 0.01) {
      return("**")
    } else if (p < 0.05) {
      return("*")
    } else if (p < 0.1) {
      return(".")
    } else {
      return("")
    }
  })
}


