#' Automatically detect the table type and output table and plot
#'
#' For each column prefix:
#' - If it matches a single column, use tab_var_counts and plot_var_counts
#' - If it matches multiple columns, use tab_items_counts and plot_item_counts
#'
#' TODO: Check whether the matched items are present
#'
#' @param data A tibble
#' @param scopes A list of column prefixes, e.g. "UM" or "WB" (character).
#' @param col_group A tidyselect column holding groups to compare, e.g. sd_gender (column name without quotes)
#' @param prop The basis of percent calculation when comparing groups: total (the default), cols, or rows
#' @param numbers The numbers plotted on bars: p (the default), n, both or NULL.
#' @param missings Include missing values (default FALSE)
#' @param ordered Values can be nominal (0) or ordered ascending (1) descending (-1).
#'                By default (NULL), the ordering is automatically detected.
#'                An appropriate color scale should be choosen depending on the ordering.
#'                For unordered values, the default scale is used.
#'                For ordered values, shades of the VLKR_FILLGRADIENT option are used.
#' @param index When multiple metric items are matched by a scope, an index will be calculated using the psych-package.
#'              Set to FALSE to suppress index generation.
#' @param title A character providing the heading or TRUE (default) to output a heading.
#'               Classes for tabset pills will be added.
#' @param close Whether to close the last tab (default value TRUE) or to keep it open.
#'              Keep it open to add further custom tabs by adding headers on the fifth level
#'              in Markdown (e.g. ##### Method)
#' @export
report <- function(data, scopes, col_group = NULL, prop = "total", numbers = "p", missings = F, ordered = NULL, index=T, title = T, close=T) {
  chunks <- list()

  # Get item label from the attributes
  labels <- data %>%
    get_labels()

  for (scope in scopes) {
    # Get column candidates
    items <- labels %>%
      dplyr::filter(stringr::str_starts(item_name, scope)) %>%
      dplyr::distinct(item_group, item_name, item_class, item_label)

    # Only keep dominant item type
    if (nrow(items) > 1) {
      item_types <- items %>%
        dplyr::count(item_class, sort = T) %>%
        dplyr::slice_head(n = 1)

      items <- dplyr::semi_join(items, item_types, by = c("item_class"))
    }

    is_items <- nrow(items) > 1
    is_var <- !is_items && (scope %in% colnames(data))
    is_scale <- get_scale(data, tidyselect::starts_with(scope), F)


    # Get title
    if (is.character(title)) {
      scope_title <- title
    } else if (title == TRUE) {
      if (is_var) {
        scope_title <- get_title(data, !!rlang::sym(scope))
      } else {
        scope_title <- get_title(data, tidyselect::starts_with(scope))
      }
    } else {
      scope_title <- ""
    }

    if (is.character(scope_title) && knitr::is_html_output()) {
      chunks <- report_add(paste0("\n#### ", scope_title, " {.tabset .tabset-pills}  \n"), chunks)
      plot_title <- F
    } else {
      plot_title <- T
    }


    # A single categorical variable
    if (is_var && is_scale == 0 && is.null(col_group)) {
      chunks <- plot_var_counts(data, !!rlang::sym(scope), numbers = numbers, title = plot_title) %>%
        report_add(chunks, "Plot")

      chunks <- tab_var_counts(data, !!rlang::sym(scope)) %>%
        report_add(chunks, "Table")
    }

    # A single metric variable
    else if (is_var && is_scale != 0 && is.null(col_group)) {
      chunks <- plot_var_metrics(data, !!rlang::sym(scope), title = plot_title) %>%
        report_add(chunks, "Plot")

      chunks <- tab_var_metrics(data, !!rlang::sym(scope)) %>%
        report_add(chunks, "Table")
    }

    # A single categorical variable by group
    else if (is_var && is_scale == 0 && !is.null(col_group)) {
      chunks <- plot_group_counts(data, !!rlang::sym(scope), !!rlang::sym(col_group), prop = prop, numbers = numbers, ordered = ordered, missings = missings, title = plot_title) %>%
        report_add(chunks, "Plot")

      chunks <- tab_group_counts(data, !!rlang::sym(scope), !!rlang::sym(col_group), prop = prop, missings = missings) %>%
        report_add(chunks, "Table")
    }

    # A single metric variable by group
    else if (is_var && is_scale != 0 && !is.null(col_group)) {
      chunks <- plot_group_metrics(data, !!rlang::sym(scope), !!rlang::sym(col_group), title = plot_title) %>%
        report_add(chunks, "Plot")

      chunks <- tab_group_metrics(data, !!rlang::sym(scope), !!rlang::sym(col_group)) %>%
        report_add(chunks, "Table")
    }

    # Multiple items
    else if (is_items && is.null(col_group)) {
      chunks <- plot_item_counts(data, tidyselect::starts_with(scope), numbers = numbers, ordered = ordered, title = plot_title) %>%
        report_add(chunks, "Plot")

      chunks <- tab_item_counts(data, tidyselect::starts_with(scope)) %>%
        report_add(chunks, "Table")
    }

    # Multiple items by group
    else if (is_items && !is.null(col_group)) {
      chunks <- plot_multi_means(data, tidyselect::starts_with(scope), !!rlang::sym(col_group), title = plot_title) %>%
        report_add(chunks, "Plot")

      chunks <- tab_multi_means(data, tidyselect::starts_with(scope), !!rlang::sym(col_group)) %>%
        report_add(chunks, "Table")
    } else {
      warning("Could't find columns to autodetect the table type for the scope ", scope, ". Check your parameters.")
    }


    # Output index
    if (index && is_items && (is_scale != 0)) {
      idx <- add_idx(data, tidyselect::starts_with(scope), newcol = ".idx")
      idx_name <- setdiff(colnames(idx), colnames(data))

      if ((length(idx_name) > 0) && is.null(col_group)) {

        chunks <- idx %>%
          plot_var_metrics(!!rlang::sym(idx_name), title = plot_title) %>%
          report_add(chunks, "Index: Plot")

        chunks <- idx %>%
          tab_var_metrics(!!rlang::sym(idx_name)) %>%
          report_add(chunks, "Index: Table")

      } else if (length(idx_name) > 0) {
        chunks <- idx %>%
          plot_group_metrics(!!rlang::sym(idx_name), !!rlang::sym(col_group), title = plot_title) %>%
          report_add(chunks, "Index: Plot")

        chunks <- idx %>%
          tab_group_metrics(!!rlang::sym(idx_name), !!rlang::sym(col_group)) %>%
          report_add(chunks, "Index: Table")
      }
    }



    # Close tabs
    if (close) {
      if (knitr::is_html_output()) {
        chunks <- report_add(paste0("\n##### {-}  \n"), chunks)
      }
      if (is.character(scope_title) && knitr::is_html_output()) {
        chunks <- report_add(paste0("\n#### {-}  \n"), chunks)
      }
    }

  }

  class(chunks) <- c("vlkr_rprt", class(chunks))
  chunks
}

#' Add an object to the report list or print it
#'
#' @param obj A volker table or plot
#' @param chunks The current report list
#' @param tab A tabsheet name or NULL
#' @return A list with volker tables or plots
report_add <- function(obj, chunks, tab = NULL) {
  if (knitr::is_html_output()) {
    # Tab
    if (!is.null(tab)) {
      tab <- paste0("\n##### ", tab, "  \n")
      chunks <- report_add(tab, chunks)
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
    print(obj)
  }

  chunks
}

#' Output styles for the pill navigation in markdown reports
#'
#' @export
add_styles <- function() {
  filename <-  paste0(system.file("extdata", package = "volker"),"/styles.css")
  styles <- readLines(filename)
  styles <- paste0(styles, collapse=" ")
  styles <- paste0("<style>", styles, "</style>")

  styles %>%
    knitr::asis_output() %>%
    knitr::knit_print()
}

#' Printing method for volker reports.
#'
#' @param obj The volker report
#' @export
print.vlkr_rprt <- function(obj) {
  if (knitr::is_html_output()) {
    obj %>%
      unlist() %>%
      paste0(collapse = "\n") %>%
      knitr::asis_output() %>%
      knitr::knit_print()
  }
}
