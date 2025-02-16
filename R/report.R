#' Create table and plot for metric variables
#'
#' Depending on your column selection, different types of plots and tables are generated.
#' See \link{plot_metrics} and \link{tab_metrics}.
#'
#' For item batteries, an index is calculated and reported.
#' When used in combination with the Markdown-template "html_report",
#' the different parts of the report are grouped under a tabsheet selector.
#'
#' `r lifecycle::badge("experimental")`
#'
#' @param data A data frame.
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param cross Optional, a grouping or correlation column (without quotes).
#' @param metric When crossing variables, the cross column parameter can contain categorical or metric values.
#'            By default, the cross column selection is treated as categorical data.
#'            Set metric to TRUE, to treat it as metric and calculate correlations.
#' @param ... Parameters passed to the \link{plot_metrics} and \link{tab_metrics} and \link{effect_metrics} functions.
#' @param index When the cols contain items on a metric scale
#'              (as determined by \link{get_direction}),
#'              an index will be calculated using the 'psych' package.
#'              Set to FALSE to suppress index generation.
#' @param factors The number of factors to calculate.
#'              Set to FALSE to suppress factor analysis.
#'              Set to TRUE to output a scree plot and automatically choose the number of factors.
#'              When the cols contain items on a metric scale
#'              (as determined by \link{get_direction}),
#'              factors will be calculated using the 'psych' package.
#'              See \link{add_factors}.
#' @param clusters The number of clusters to calculate.
#'                 Cluster are determined using kmeans after scaling the items.
#'                Set to FALSE to suppress cluster analysis.
#'                Set to TRUE to output a scree plot and automatically choose the number of clusters based on the elbow criterion.
#'                 See \link{add_clusters}.
#' @param effect Whether to report statistical tests and effect sizes. See \link{effect_counts} for further parameters.
#' @param title A character providing the heading or TRUE (default) to output a heading.
#'               Classes for tabset pills will be added.
#' @param close Whether to close the last tab (default value TRUE) or to keep it open.
#'              Keep it open to add further custom tabs by adding headers on the fifth level
#'              in Markdown (e.g. ##### Method).
#' @param clean Prepare data by \link{data_clean}.
#' @return A volker report object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' report_metrics(data, sd_age)
#'
#' @export
report_metrics <- function(data, cols, cross = NULL, metric = FALSE, ..., index = FALSE, factors = FALSE, clusters = FALSE, effect = FALSE, title = TRUE, close = TRUE, clean = TRUE) {

  if (clean) {
    data <- data_clean(data, clean)
  }

  chunks <- list()

  # Add title
  if (!is.null(knitr::pandoc_to())) {
    if (!is.character(title) && (title == TRUE)) {
      title <- get_title(data, {{ cols }})
    } else if (!is.character(title)) {
      title <- ""
    }

    chunks <- .add_to_vlkr_rprt(paste0("\n#### ", title, " {.tabset .tabset-pills}  \n"), chunks)
    plot_title <- FALSE
  } else {
    plot_title <- title
  }


  # Add Plot
  chunks <- plot_metrics(data, {{ cols }}, {{ cross }}, metric = metric, effect = effect, clean = clean, ..., title = plot_title) %>%
    .add_to_vlkr_rprt(chunks, "Plot")

  # Add table
  chunks <- tab_metrics(data, {{ cols}}, {{ cross }}, metric = metric, effect = effect, clean=clean, ...) %>%
    .add_to_vlkr_rprt(chunks, "Table")

  # Add effect sizes
  if (effect) {
    chunks <- effect_metrics(data, {{ cols }}, {{ cross }}, metric = metric, effect = effect, clean=clean, ...) %>%
      .add_to_vlkr_rprt(chunks, "Effects")
  }

  # Add index
  if (index) {
    idx <- .report_idx(data, {{ cols }}, {{ cross }}, metric = metric, ..., effect = effect, title = plot_title)
    chunks <- append(chunks, idx)
  }

  # Add factors
  if (!isFALSE(factors)) {
    if (factors == TRUE) {
      factors <- NULL
    }
    fct <- .report_fct(data, {{ cols }}, {{ cross }}, metric = metric, k = factors, ..., effect = effect, title = plot_title)
    chunks <- append(chunks, fct)
  }

  # Add clusters
  if (!any(isFALSE(clusters))) {
    if (all(clusters == TRUE)) {
      clusters <- NULL
    }
    fct <- .report_cls(data, {{ cols }}, {{ cross }}, metric = metric, k = clusters, ..., effect = effect, title = plot_title)
    chunks <- append(chunks, fct)
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
#' `r lifecycle::badge("experimental")`
#'
#' @param data A data frame.
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param cross Optional, a grouping column (without quotes).
#' @param metric When crossing variables, the cross column parameter can contain categorical or metric values.
#'            By default, the cross column selection is treated as categorical data.
#'            Set metric to TRUE, to treat it as metric and calculate correlations.
#' @param index When the cols contain items on a metric scale
#'              (as determined by \link{get_direction}),
#'              an index will be calculated using the 'psych' package.
#'              Set to FALSE to suppress index generation.
#' @param effect Whether to report statistical tests and effect sizes. See \link{effect_counts} for further parameters.
#' @param numbers The numbers to print on the bars: "n" (frequency), "p" (percentage) or both.
#'                Set to NULL to remove numbers.
#' @param title A character providing the heading or TRUE (default) to output a heading.
#'               Classes for tabset pills will be added.
#' @param close Whether to close the last tab (default value TRUE) or to keep it open.
#'              Keep it open to add further custom tabs by adding headers on the fifth level
#'              in Markdown (e.g. ##### Method).
#' @param clean Prepare data by \link{data_clean}.
#' @param ... Parameters passed to the \link{plot_counts} and \link{tab_counts} and \link{effect_counts} functions.
#' @return A volker report object.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' report_counts(data, sd_gender)
#'
#' @export
report_counts <- function(data, cols, cross = NULL, metric = FALSE, index = FALSE, effect = FALSE, numbers = NULL, title = TRUE, close = TRUE, clean = TRUE, ...) {

  if (clean) {
    data <- data_clean(data, clean)
  }

  chunks <- list()

  # Add title
  if (!is.null(knitr::pandoc_to())) {
    if (!is.character(title) && (title == TRUE)) {
      title <- get_title(data, {{ cols }})
    } else if (!is.character(title)) {
      title <- ""
    }

    chunks <- .add_to_vlkr_rprt(paste0("\n#### ", title, " {.tabset .tabset-pills}  \n"), chunks)
    plot_title <- FALSE
  } else {
    plot_title <- title
  }


  # Add Plot
  chunks <- plot_counts(data, {{ cols }}, {{ cross }}, metric = metric, effect = effect, title = plot_title, numbers=numbers, clean=clean, ...) %>%
    .add_to_vlkr_rprt(chunks, "Plot")

  # Add table
  chunks <- tab_counts(data, {{ cols }}, {{ cross }}, metric = metric, effect = effect, clean=clean, ...) %>%
    .add_to_vlkr_rprt(chunks, "Table")

  # Add effect sizes
  if (effect) {
    chunks <- effect_counts(data, {{ cols }}, {{ cross }}, metric = metric, effect = effect, clean=clean, ...) %>%
      .add_to_vlkr_rprt(chunks, "Effects")
  }

  # Add index
  if (index) {
    idx <- .report_idx(data, {{ cols }}, {{ cross }}, metric = metric, effect = effect, title = plot_title)
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
#' @param data A data frame.
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param cross Optional, a grouping column (without quotes).
#' @param metric When crossing variables, the cross column parameter can contain categorical or metric values.
#'            By default, the cross column selection is treated as categorical data.
#'            Set metric to TRUE, to treat it as metric and calculate correlations.
#' @param effect Whether to report statistical tests and effect sizes.
#' @param title Add a plot title (default = TRUE).
#' @return A list containing a table and a plot volker report chunk.
.report_idx <- function(data, cols, cross, metric = FALSE, ..., effect = FALSE, title = TRUE) {
  chunks <- list()

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  is_items <- length(cols_eval) > 1
  is_scale <- get_direction(data, {{ cols }}, FALSE)

  if (is_items && (is_scale != 0)) {

    idx <- add_index(data, {{ cols }}, newcol = ".idx")
    idx_name <- setdiff(colnames(idx), colnames(data))

    if (length(idx_name) > 0) {

      chunks <- idx %>%
        plot_metrics(!!rlang::sym(idx_name), {{ cross }}, ..., title = title) %>%
        .add_to_vlkr_rprt(chunks, "Index: Plot")

      chunks <- idx %>%
        tab_metrics(!!rlang::sym(idx_name), {{ cross }}, metric = metric, ...) %>%
        .add_to_vlkr_rprt(chunks, "Index: Table")

      if (effect) {
        chunks <- idx %>%
          effect_metrics(!!rlang::sym(idx_name), {{ cross }}, metric = metric, ...) %>%
          .add_to_vlkr_rprt(chunks, "Index: Effects")
      }
    }
  }

  chunks
}

#' Generate an factor table and plot
#'
#' @keywords internal
#'
#' @param data A data frame.
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param cross Not yet implementedt. Optional, a grouping column (without quotes).
#' @param metric Not yet implemented. When crossing variables, the cross column parameter can contain categorical or metric values.
#'            By default, the cross column selection is treated as categorical data.
#'            Set metric to TRUE, to treat it as metric and calculate correlations.
#' @param k Number of factors to calculate.
#' @param effect Not yet implemented. Whether to report statistical tests and effect sizes.
#' @param title Add a plot title (default = TRUE).
#' @return A list containing a table and a plot volker report chunk.
.report_fct <- function(data, cols, cross, metric = FALSE, ..., k = 2, effect = FALSE, title = TRUE) {
  chunks <- list()

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  is_items <- length(cols_eval) > 1
  is_scale <- get_direction(data, {{ cols }}, FALSE)

  if (is_items && (is_scale != 0)) {

    scores <- add_factors(data, {{ cols }}, k = k, ...)
    newcols <- setdiff(colnames(scores), colnames(data))

    plt <- factor_plot(scores, tidyselect::all_of(newcols), k = k, ...)
    chunks <- .add_to_vlkr_rprt(plt, chunks, "Factors: Plot")

    tab <- factor_tab(scores, tidyselect::all_of(newcols), k = k, ...)
    chunks <- .add_to_vlkr_rprt(tab ,chunks, "Factors: Table")
  }

  chunks
}

#' Generate an cluster table and plot
#'
#' @keywords internal
#'
#' @param data A data frame.
#' @param cols A tidy column selection,
#'             e.g. a single column (without quotes)
#'             or multiple columns selected by methods such as starts_with().
#' @param cross Not yet implemented. Optional, a grouping column (without quotes).
#' @param metric Not yet implemented. When crossing variables, the cross column parameter can contain categorical or metric values.
#'            By default, the cross column selection is treated as categorical data.
#'            Set metric to TRUE, to treat it as metric and calculate correlations.
#' @param k Number of clusters to calculate.
#' @param effect Not yet implemented. Whether to report statistical tests and effect sizes.
#' @param title Add a plot title (default = TRUE).
#' @return A list containing a table and a plot volker report chunk.
.report_cls <- function(data, cols, cross, metric = FALSE, ..., k = 2, effect = FALSE, title = TRUE) {
  chunks <- list()

  cols_eval <- tidyselect::eval_select(expr = enquo(cols), data = data)
  is_items <- length(cols_eval) > 1
  is_scale <- get_direction(data, {{ cols }}, FALSE)

  if (is_items && (is_scale != 0)) {

    scores <- add_clusters(data, {{ cols }}, k = k, ...)
    newcol <- setdiff(colnames(scores), colnames(data))

    plt <- cluster_plot(scores, !!sym(newcol), k = k,  ...)
    chunks <- .add_to_vlkr_rprt(plt, chunks, "Clusters: Plot")

    tab <- cluster_tab(scores, !!sym(newcol), k = k, ...)
    chunks <- .add_to_vlkr_rprt(tab ,chunks, "Clusters: Table")
  }

  chunks
}


#' Add vlkr_list class
#'
#' Used to collect multiple tables in a list,
#' e.g. from regression outputs
#'
#' @keywords internal
#'
#' @param data A list.
#' @param baseline Whether to get the baseline.
#' @return A volker list.
.to_vlkr_list <- function(data, baseline = TRUE) {

  if (baseline == TRUE) {
    baseline <- get_baseline(data)
  } else if (baseline == FALSE) {
    baseline <- NULL
  }

  if (!is.null(baseline)) {
    attr(data, "baseline") <- baseline
  }

  class(data) <- c("vlkr_list", setdiff(class(data), "vlkr_list"))
  data
}

#' Printing method for volker lists
#'
#' @keywords internal
#'
#' @param x The volker list.
#' @param ... Further parameters passed to print.
#' @return No return value.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' rp <- report_metrics(data, sd_age, sd_gender, effect = TRUE)
#' print(rp)
#'
#' @export
print.vlkr_list <- function(x, ...) {
  if (!is.null(knitr::pandoc_to())) {

    output <- list()
    for (part in x) {

      caption <- attr(part, "caption", exact=TRUE)
      if (!is.null(caption)) {
        caption <- paste0("\n###### ", caption, "  \n")
        output <- append(output, caption)
      }


      if (inherits(part, "vlkr_tbl")) {
        output <- append(output, .knit_table(part))
      } else if (inherits(part, "vlkr_plt")) {
        output <- append(output, .knit_plot(part))
      } else if (is.character(part)) {
        output <- append(output, part)
      } else {
        warning("Could not determine the volker list item type")
      }
    }

    baseline <- attr(x, "baseline", exact=TRUE)
    if (!is.null(baseline)) {
      output <- append(output, baseline)
    }

    output |>
      unlist() %>%
      paste0(collapse = "\n") %>%
      knitr::asis_output() %>%
      knitr::knit_print()

  } else {
    for (part in x) {
      caption <- attr(part, "caption", exact=TRUE)
      if (!is.null(caption)) {
        cat("\n", caption, sep="")
      }
      print(part, ...)
    }

    baseline <- attr(x, "baseline", exact=TRUE)
    if (!is.null(baseline)) {
      cat("\n", baseline, "\n\n", sep="")
    }
  }
}

#' Add the vlkr_rprt class to an object
#'
#' Adding the class makes sure the appropriate printing function
#' is applied in markdown reports.
#'
#' @keywords internal
#'
#' @param chunks A list of character strings.
#' @return A volker report object: List of character strings with the vlkr_rprt class
#'         containing the parts of the report.
.to_vlkr_rprt <- function(chunks) {
  class(chunks) <- c("vlkr_rprt", setdiff(class(chunks),"vlkr_rprt"))
  chunks
}

#' Add an object to the report list
#'
#' @keywords internal
#'
#' @param obj A new chunk (volker table, volker plot or character value).
#' @param chunks The current report list.
#' @param tab A tabsheet name or NULL.
#' @return A volker report object.
.add_to_vlkr_rprt <- function(obj, chunks, tab = NULL) {

  if (!is.null(knitr::pandoc_to())) {
  #if (knitr::is_html_output()) {
    # Tab
    if (!is.null(tab)) {
      tab <- paste0("\n##### ", tab, "  \n")
      chunks <- .add_to_vlkr_rprt(tab, chunks)
    }

    # Nested objects
    if (inherits(obj, "vlkr_list")) {
      baseline <- attr(obj, "baseline", exact=TRUE)
      for (childobj in obj) {

        caption <- attr(childobj, "caption", exact=TRUE)
        if (!is.null(caption)) {
          newchunk <- paste0("\n###### ", caption, "  \n")
          chunks <- append(chunks, newchunk)
        }

        chunks <- .add_to_vlkr_rprt(childobj, chunks)
      }
    }

    # Objects
    else {
      if (inherits(obj, "vlkr_tbl")) {
        newchunk <- .knit_table(obj)
      } else if (inherits(obj, "vlkr_plt")) {
        newchunk <- .knit_plot(obj)

      } else if (is.character(obj)) {
        newchunk <- obj
      } else {
        warning("Could not determine the volker report chunk type")
        newchunk <- "???"
      }

      baseline <- attr(newchunk, "baseline", exact=TRUE)
      chunks <- append(chunks, newchunk)
    }

    # Add baseline
    if (!is.null(baseline)) {
      newchunk <- paste0("\n*", baseline, "*  \n")
      chunks <- append(chunks, newchunk)
    }

  } else {
    if (!is.null(tab) && !is.null(obj)) {
      attr(obj,"comment") <- tab
    }
    chunks <- append(chunks, list(obj))
  }


  .to_vlkr_rprt(chunks)
}

#' Printing method for volker reports
#'
#' @keywords internal
#'
#' @param x The volker report object.
#' @param ... Further parameters passed to print.
#' @return No return value.
#' @examples
#' library(volker)
#' data <- volker::chatgpt
#'
#' rp <- report_metrics(data, sd_age)
#' print(rp)
#'
#' @export
print.vlkr_rprt <- function(x, ...) {
  if (!is.null(knitr::pandoc_to())) {
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
#' @param ... Additional arguments passed to html_document.
#' @return R Markdown output format.
#' @examples
#' \dontrun{
#' # Add `volker::html_report` to the output options of your Markdown document:
#' #
#' # ```
#' # ---
#' # title: "How to create reports?"
#' # output: volker::html_report
#' # ---
#' # ```
#' }
#' @export
html_report <- function(...) {
  cssfile <-  paste0(system.file("extdata", package = "volker"),"/styles.css")
  rmarkdown::html_document(
    css = cssfile,
    #df_print = "kable",
    ...
  )
}

#' Volker style PDF document format
#'
#' Based on the standard theme, tweaks tex headers.
#' To use the format, in the header of your Markdown document,
#' set `output: volker::pdf_report`.
#'
#' @param ... Additional arguments passed to pdf_document.
#' @return R Markdown output format.
#' @examples
#' \dontrun{
#' # Add `volker::pdf_report` to the output options of your Markdown document:
#' #
#' # ```
#' # ---
#' # title: "How to create reports?"
#' # output: volker::pdf_report
#' # ---
#' # ```
#' }
#' @export
pdf_report <- function(...) {
  headerfile <-  system.file("extdata/header.tex", package = "volker")
  rmarkdown::pdf_document(
    includes = rmarkdown::includes(in_header=headerfile),
    ...
  )
}
