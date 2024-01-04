#' Prepare metadata and types of dataframe columns
#'
#' - Removes the avector class from all columns
#'   (comes from Sosci and prevents combining vectors)
#'
#' @param data Data frame
#' @return Data frame
#' @export
clean_columns <- function(data) {
  for (i in c(1:ncol(data))) {
    class(data[[i]]) <- setdiff(class(data[[i]]),"avector")
  }
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
#' @return A combined data frame
#' @export
zip_tables <- function(x, y, newline=TRUE) {

  newline <- newline && (knitr::is_html_output() || knitr::is_latex_output())
  sep <- ifelse(newline,"\n"," ")

  for (i in 2:ncol(x)) {
    x[[i]] <- paste0(x[[i]], sep, "(", y[[i]], ")")
  }
  x
}


#' Knit table with defaults for the package
#'
#' @param df Data frame
#' @return Formatted table
knit_table <- function(df, ...){

  if (knitr::is_html_output()) {

    # Replace \n by <br>
    df <- df %>%
      dplyr::mutate(across(dplyr::where(is.character), ~ gsub("\n", "<br>", .))) %>%
      knitr::kable("html", escape = F, align=c("l", rep("r",ncol(df) - 1)), ...) %>%
      kableExtra::kable_styling()


  } else if (knitr::is_latex_output()) {

    df <- df %>%
      dplyr::mutate_all(kableExtra::linebreak) %>%
      knitr::kable("latex", booktabs = T, escape = F, align=c("l", rep("r",ncol(df) - 1)), ...)

  } else {

    df <- df %>%
      knitr::kable("pipe" , align=c("l", rep("r",ncol(df) - 1)), ...)

  }

  df
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
