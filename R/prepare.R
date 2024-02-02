#' Prepare metadata and types of dataframe columns
#'
#' Prepares Sosci data:
#' - Remove the avector class from all columns
#'   (comes from Sosci and prevents combining vectors)
#' - Recode residual factor values to NA ("[NA] nicht beantwortet")
#' - Recode -9 values to NA
#'
#' @param data Data frame
#' @param remove.na Whether residual values should be replaced by NA
#' @return Data frame
#' @export
prepare <- function(data, remove.na = T) {
  for (i in c(1:ncol(data))) {
    class(data[[i]]) <- setdiff(class(data[[i]]), "avector")
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
        ~ na_if(., remove.na)
      )
    )

    data <- dplyr::mutate(data, across(where(is.numeric), ~ na_if(., -9)))
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

  dplyr::as_tibble(data)
}
