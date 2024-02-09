#' Create table and plot for metric variables
#'
#' Depending on your column selection, different types of plots and tables are generated.
#' See \link{plot_metrics} and \link{tab_metrics}.
#'
#' For item batteries, an index is calculated and reported.
#' When used in combination with the Markdown-template "html_report",
#' the different parts of the report are grouped under a tabsheet selector.
#'
#' @param data A data frame
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param col_group Optional, a grouping column (without quotes).
#' @param ... Parameters passed to the plot and tab functions.
#' @param index When the cols contain items on a metric scale
#'              (as determined by \link{get_direction}),
#'              an index will be calculated using the psych-package.
#'              Set to FALSE to suppress index generation.
#' @param title A character providing the heading or TRUE (default) to output a heading.
#'               Classes for tabset pills will be added.
#' @param close Whether to close the last tab (default value TRUE) or to keep it open.
#'              Keep it open to add further custom tabs by adding headers on the fifth level
#'              in Markdown (e.g. ##### Method)
#' @export
report_metrics <- function(data, cols, col_group = NULL, ..., index=T, title = T, close=T) {
  chunks <- list()

  # Add title
  if (!is.character(title) && (title == TRUE)) {
    title <- get_title(data, {{ cols }})
  } else if (!is.character(title)) {
    title <- ""
  }

  if (is.character(title) && knitr::is_html_output()) {
    chunks <- .add_to_vlkr_rprt(paste0("\n#### ", title, " {.tabset .tabset-pills}  \n"), chunks)
    plot_title <- F
  } else {
    plot_title <- T
  }


  # Add Plot
  chunks <- plot_metrics(data, {{ cols}}, {{ col_group }}, ..., title = plot_title) %>%
    .add_to_vlkr_rprt(chunks, "Plot")

  # Add table
  chunks <- tab_metrics(data, {{ cols}}, {{ col_group }}, ...) %>%
    .add_to_vlkr_rprt(chunks, "Table")

  # Add index
  if (index) {
    idx <- .report_idx(data, {{ cols}}, {{ col_group }}, title = plot_title)
    chunks <- append(chunks,idx)
  }

  # Close tabs
  if (close) {
    if (knitr::is_html_output()) {
      chunks <- .add_to_vlkr_rprt(paste0("\n##### {-}  \n"), chunks)
    }
    if (is.character(title) && knitr::is_html_output()) {
      chunks <- .add_to_vlkr_rprt(paste0("\n#### {-}  \n"), chunks)
    }
  }

  .to_vlkr_rprt(chunks)
}

#' Create table and plot for categorical variables
#'
#' Depending on your column selection, different types of plots and tables are generated.
#' See \link{plot_counts} and \link{tab_counts}.
#'
#' For item batteries, an index is calculated and reported.
#' When used in combination with the Markdown-template "html_report",
#' the different parts of the report are grouped under a tabsheet selector.
#'
#' @param data A data frame
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param col_group Optional, a grouping column (without quotes).
#' @param index When the cols contain items on a metric scale
#'              (as determined by \link{get_direction}),
#'              an index will be calculated using the psych-package.
#'              Set to FALSE to suppress index generation.
#' @param numbers The numbers to print on the bars: "n" (frequency), "p" (percentage) or both.
#'                Set to NULL to remove numbers.
#' @param title A character providing the heading or TRUE (default) to output a heading.
#'               Classes for tabset pills will be added.
#' @param close Whether to close the last tab (default value TRUE) or to keep it open.
#'              Keep it open to add further custom tabs by adding headers on the fifth level
#'              in Markdown (e.g. ##### Method)
#' @param ... Parameters passed to the plot and tab functions.
#' @return A list of class vlkr_rprt containing the parts of the report
#' @export
report_counts <- function(data, cols, col_group = NULL, index=T, numbers=NULL, title = T, close=T, ...) {
  #, prop = "total", numbers = "p", missings = F, ordered = NULL, index=T,
  chunks <- list()

  # Add title
  if (!is.character(title) && (title == TRUE)) {
    title <- get_title(data, {{ cols }})
  } else if (!is.character(title)) {
    title <- ""
  }

  if (is.character(title) && knitr::is_html_output()) {
    chunks <- .add_to_vlkr_rprt(paste0("\n#### ", title, " {.tabset .tabset-pills}  \n"), chunks)
    plot_title <- F
  } else {
    plot_title <- T
  }


  # Add Plot
  chunks <- plot_counts(data, {{ cols }}, {{ col_group }}, ..., title = plot_title, numbers=numbers) %>%
    .add_to_vlkr_rprt(chunks, "Plot")

  # Add table
  chunks <- tab_counts(data, {{ cols }}, {{ col_group }}, ...) %>%
    .add_to_vlkr_rprt(chunks, "Table")

  # Add index
  if (index) {
    idx <- .report_idx(data, {{ cols }}, {{ col_group }}, title = plot_title)
    chunks <- append(chunks,idx)
  }

  # Close tabs
  if (close) {
    if (knitr::is_html_output()) {
      chunks <- .add_to_vlkr_rprt(paste0("\n##### {-}  \n"), chunks)
    }
    if (is.character(title) && knitr::is_html_output()) {
      chunks <- .add_to_vlkr_rprt(paste0("\n#### {-}  \n"), chunks)
    }
  }

  .to_vlkr_rprt(chunks)
}

#' Generate an index table and plot
#'
#' @keywords internal
#'
#' @param data A data frame
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param col_group Optional, a grouping column (without quotes).
#' @param title Add a plot title (default = TRUE)
#' @return A list containing a table and a plot
.report_idx <- function(data, cols, col_group, title=T) {
  chunks <- list()

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  is_items <- length(cols_eval) > 1
  is_scale <- get_direction(data, {{ cols }}, F)

  if (is_items && (is_scale != 0)) {

    idx <- add_idx(data, {{ cols }}, newcol = ".idx")
    idx_name <- setdiff(colnames(idx), colnames(data))

    if (length(idx_name) > 0) {

      chunks <- idx %>%
        plot_metrics(!!rlang::sym(idx_name), {{ col_group }}, title = title) %>%
        .add_to_vlkr_rprt(chunks, "Index: Plot")

      chunks <- idx %>%
        tab_metrics(!!rlang::sym(idx_name), {{ col_group }}) %>%
        .add_to_vlkr_rprt(chunks, "Index: Table")
    }
  }

  chunks
}

#' Add the vlkr_rprt class to an object
#'
#' Adding the class makes sure the appropriate printing function
#' is applied in markdown reports.
#'
#' @keywords internal
#'
#' @param chunks A list of character strings
#' @return A list of character strings with the vlkr_rprt class
.to_vlkr_rprt <- function(chunks) {
  class(chunks) <- c("vlkr_rprt", setdiff(class(chunks),"vlkr_rprt"))
  chunks
}

#' Add an object to the report list
#'
#' @keywords internal
#'
#' @param obj A new chunk (volker table, volker plot or character value)
#' @param chunks The current report list
#' @param tab A tabsheet name or NULL
#' @return A list with volker tables or plots
.add_to_vlkr_rprt <- function(obj, chunks, tab = NULL) {

  if (knitr::is_html_output()) {
    # Tab
    if (!is.null(tab)) {
      tab <- paste0("\n##### ", tab, "  \n")
      chunks <- .add_to_vlkr_rprt(tab, chunks)
    }

    # Objects
    if (inherits(obj, "vlkr_tbl")) {
      newchunk <- knit_table(obj)
    } else if (inherits(obj, "vlkr_plt")) {
      newchunk <- knit_plot(obj)
    } else if (is.character(obj)) {
      newchunk <- obj
    } else {
      warning("Could not determine the volker report chunk type")
    }

    chunks <- append(chunks, newchunk)
  } else {
    if (!is.null(tab)) {
      attr(obj,"comment") <- tab
    }
    chunks <- append(chunks, list(obj))
  }

  .to_vlkr_rprt(chunks)
}

#' Printing method for volker reports.
#'
#' @keywords internal
#'
#' @param x The volker report
#' @param ... Further parameters passed to print
#' @importFrom rlang .data
#' @export
print.vlkr_rprt <- function(x, ...) {
  if (knitr::is_html_output()) {
    x %>%
      unlist() %>%
      paste0(collapse = "\n") %>%
      knitr::asis_output() %>%
      knitr::knit_print()
  } else {
    for (part in x) {
      print(part, ...)
    }
  }
}



#' Volker style HTML document format
#'
#' Based on the standard theme, tweaks the pill navigation
#' to switch between tables and plots.
#' To use the format, in the header of your Markdown document,
#' set `output: volker::html_report`.
#'
#' @param ... Additional arguments passed to html_document
#' @return R Markdown output format
#' @export
html_report <- function(...) {
  cssfile <-  paste0(system.file("extdata", package = "volker"),"/styles.css")
  rmarkdown::html_document(
    css = cssfile,
    ...
  )
}
