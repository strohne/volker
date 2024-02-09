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
#' @export
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
#' @return A character vector with stars
#' @export
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

#' Calculate the unbiased Gini coefficient
#'
#' This function computes the unbiased Gini coefficient for a given vector of data.
#'
#' @param x A numeric vector representing the data for which the Gini coefficient is to be calculated.
#' @return The unbiased Gini coefficient for the input data.
#'
#' @details The Gini coefficient is a measure of statistical dispersion representing the inequality of a distribution.
#' The unbiased Gini coefficient is a corrected version that provides a less biased estimate, especially for small sample sizes.
#' @author Made with the help of ChatGPT
gini <- function(x) {

  # Return NA for negative values and NA values
  if (any(is.na(x)) || any(x < 0))
    return(NA_real_)

  # Init weights
  # (Not used by now, can be added as function parameter in future versions if necessary)
  w <- rep(1, length(x))
  w <- w / sum(w)

  # Order values and weight indexes
  x <- x[id <- order(x)]
  w <- w[id]

  f.hat <- w / 2 + c(0, cumsum(w[-length(w)]))
  wm <- stats::weighted.mean(x, w)

  # Calculate gini g
  g <- 2 / wm * sum(w * (x - wm) * (f.hat - stats::weighted.mean(f.hat, w)))

  # Bias corrections
  g <- g * 1 / (1 - sum(w^2))


  return(g)
}

#' Calculate relevance of terms for a topic
#'
#' Relevance is p for lambda = 1 and the probability shift for lambda = 0.
#'
#' See section 3 in https://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
#' See http://vis.stanford.edu/files/2012-Termite-AVI.pdf and
#'     https://nlp.stanford.edu/events/illvi2014/papers/sievert-illvi2014.pdf
#'
#' @param p The matrix of topic-term-probabilities.
#'          Each row is a topic. Each column is a term.
#' @return A matrix of the same shape as p with relevance scores
#' @author Inspired by Chantal Gaertner
relevance <- function(p, lambda=0.6) {
  # TODO: Which version is correct?

  # Version 1:
  # TODO: is p_w = col_sums(p) correct here?
  # lambda * log(p) + (1- lambda) * log (p / p_w)
  lambda * log(p) + (1 - lambda) * log(p / slam::col_sums(p))

  # Version 2:
  #lambda * p + (1 - lambda) * (p / slam::col_sums(p))
}


