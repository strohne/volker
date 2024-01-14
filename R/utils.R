#' Check whether a column exist and stop if not
#'
#' @param data A data frame
#' @param col A column name
#' @param stopit If true, the execution stops when a column is missing
#' @return boolean
has_column <- function(data, col, stopit = T) {
  colname <- as.character(rlang::get_expr(rlang::enquo(col)))

  if (! (colname %in% colnames(data))) {
    stop("The column ", colname, " does not exist, check your parameters.", call.=F)
  }

  colname %in% colnames(data)
}


#' Prepare metadata and types of dataframe columns
#'
#' - Removes the avector class from all columns
#'   (comes from Sosci and prevents combining vectors)
#' - Recode residual factor values to NA ("[NA] nicht beantwortet")
#'
#' @param data Data frame
#' @param remove.na Whether residual values should be replaced by NA
#' @return Data frame
#' @export
clean_columns <- function(data, remove.na=T) {
  for (i in c(1:ncol(data))) {
    class(data[[i]]) <- setdiff(class(data[[i]]),"avector")
  }


  # Recode missings

  if (remove.na != FALSE) {
    if (is.logical(remove.na)) {
      remove.na <- "[NA] nicht beantwortet"
    }

    data <- dplyr::mutate(
      data,
      dplyr::across(
        tidyselect::where(
          ~ is.factor(.) & (remove.na %in% levels(.))
        ),
        ~ na_if(.,remove.na)
      )
    )
  }

  # Add whitespace for better breaks
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

  data
}


#' A reduced skimmer for metric variables
#' Returns a five point summary, mean and sd, items count and alpha for scales added by add_idx()
#' @export
skim_metrics <- skimr::skim_with(
  numeric = skimr::sfl(
    min = ~ base::min(., na.rm = T),
    q1 = ~ stats::quantile(., probs = 0.25, na.rm = TRUE, names = FALSE),
    median = ~ stats::median(., na.rm = T),
    q3 = ~stats::quantile(., probs = 0.75, na.rm = TRUE, names = FALSE),
    max = ~ base::max(., na.rm = T),
    mean = ~ base::mean(., na.rm=T),
    sd = ~ stats::sd(., na.rm=T),
    items = ~ get_idx_alpha(.)$items,
    alpha = ~ get_idx_alpha(.)$alpha
  ),
  base = skimr::sfl(
    n = length,
    missing = skimr::n_missing
  ),
  append = FALSE
)

#' Combine two identically shaped data frames
#' by adding values of each column from the second data frame
#' into the corresponding column in the first dataframe using parentheses
#'
#' TODO: implement newline parameter
#'
#' @param x The first data frame
#' @param y The second data frame
#' @param newline Whether to add a new line character between the values (default: TRUE).
#' @param brackets Whether to set the secondary values in brackets (default: FALSE).
#' @return A combined data frame
#' @export
zip_tables <- function(x, y, newline=TRUE, brackets=FALSE) {

  newline <- newline && (knitr::is_html_output() || knitr::is_latex_output())
  sep <- ifelse(newline,"\n"," ")

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
#' @param fit The result of an lm() call
#' @return The p value of the model
lm_pvalue <- function(fit) {
  fstat <- summary(fit)$fstatistic
  pf(fstat[1], fstat[2], fstat[3], lower.tail=FALSE)
}

#' Get significance stars from p values
#'
#' @param x A vector of p values
#' @return A character vector with stars
#' @export
get_stars <- function(x) {
  sapply(x, function(p) {
    if (p < 0.001)
      return ("***")
    else if (p < 0.01)
      return ("**")
    else if (p < 0.05)
      return ("*")
    else if (p < 0.1)
      return (".")
    else return ("")
  })
}
