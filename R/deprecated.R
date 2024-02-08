#
# Deprecated or aliased functions
#

#' Alias for tab_counts_one
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_var_counts` has been deprecated. Use  \link{tab_counts_one} instead.
#'
#' @keywords internal
#' @export
tab_var_counts <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "tab_var_counts()", "tab_counts_one()", always = TRUE)
  tab_counts_one(...)
}

#' Alias for tab_counts_one
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_counts_var` has been deprecated. Use \link{tab_counts_one} instead.
#'
#' @keywords internal
#' @export
tab_counts_var <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "tab_counts_var()", "tab_counts_one()", always = TRUE)
  tab_counts_one(...)
}

#' Alias for tab_counts_one_grouped
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_group_counts` has been deprecated. Use \link{tab_counts_one_grouped} instead.
#'
#' @keywords internal
#' @export
tab_group_counts <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "tab_group_counts()", "tab_counts_one_grouped()", always = TRUE)
  tab_counts_one_grouped(...)
}

#' Alias for tab_counts_items
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_item_counts` has been deprecated. Use \link{tab_counts_items} instead.
#'
#' @keywords internal
#' @export
tab_item_counts <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "tab_item_counts()", "tab_counts_items()", always = TRUE)
  tab_counts_items(...)
}

#' Alias for tab_metrics_one
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_var_metrics` has been deprecated. Use \link{tab_metrics_one} instead.
#'
#' @keywords internal
#' @export
tab_var_metrics <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "tab_var_metrics()", "tab_metrics_one()", always = TRUE)
  tab_metrics_one(...)
}

#' Alias for tab_metrics_one_grouped
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_group_metrics` has been deprecated. Use \link{tab_metrics_one_grouped} instead.
#'
#' @keywords internal
#' @export
tab_group_metrics <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "tab_group_metrics()", "tab_metrics_one_grouped()", always = TRUE)
  tab_metrics_one_grouped(...)
}

#' Alias for tab_metrics_items
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_item_metrics` has been deprecated. Use \link{tab_metrics_items} instead.
#'
#' @keywords internal
#' @export
tab_item_metrics <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "tab_items_metrics()", "tab_metrics_items()", always = TRUE)
  tab_metrics_items(...)
}

#' Alias for tab_metrics_items_grouped
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_multi_means` has been deprecated. Use \link{tab_metrics_items_grouped} instead.
#'
#' @keywords internal
#' @export
tab_multi_means <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "tab_multi_means()", "tab_metrics_items_grouped()", always = TRUE)
  tab_metrics_items_grouped(...)
}


#' Alias for tab_metrics_items_cor
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `tab_multi_corr` has been deprecated. Use \link{tab_metrics_items_cor} instead.
#'
#' @keywords internal
#' @export
tab_multi_corr <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "tab_multi_corr()", "tab_metrics_items_cor()", always = TRUE)
  tab_metrics_items_cor(...)
}

#' Alias for plot_counts_one
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_var_counts` has been deprecated. Use  \link{plot_counts_one} instead.
#'
#' @keywords internal
#' @export
plot_var_counts <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "plot_var_counts()", "plot_counts_one()", always = TRUE)
  plot_counts_one(...)
}

#' Alias for plot_counts_one_grouped
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_group_counts` has been deprecated. Use \link{plot_counts_one_grouped} instead.
#'
#' @keywords internal
#' @export
plot_group_counts <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "plot_group_counts()", "plot_counts_one_grouped()", always = TRUE)
  plot_counts_one_grouped(...)
}

#' Alias for plot_counts_items
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_item_counts` has been deprecated. Use \link{plot_counts_items} instead.
#'
#' @keywords internal
#' @export
plot_item_counts <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "plot_item_counts()", "plot_counts_items()", always = TRUE)
  plot_counts_items(...)
}

#' Alias for plot_metrics_one
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_var_metrics` has been deprecated. Use \link{plot_metrics_one} instead.
#'
#' @keywords internal
#' @export
plot_var_metrics <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "plot_var_metrics()", "plot_metrics_one()", always = TRUE)
  plot_metrics_one(...)
}

#' Alias for plot_metrics_one_grouped
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_group_metrics` has been deprecated. Use \link{plot_metrics_one_grouped} instead.
#'
#' @keywords internal
#' @export
plot_group_metrics <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "plot_group_metrics()", "plot_metrics_one_grouped()", always = TRUE)
  plot_metrics_one_grouped(...)
}


#' Alias for plot_metrics_items
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_item_metrics` has been deprecated. Use \link{plot_metrics_items} instead.
#'
#' @keywords internal
#' @export
plot_item_metrics <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "plot_item_metrics()", "plot_metrics_items()", always = TRUE)
  plot_metrics_items(...)
}


#' Alias for plot_metrics_items_grouped
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `plot_multi_means` has been deprecated. Use \link{plot_metrics_items_grouped} instead.
#'
#' @keywords internal
#' @export
plot_multi_means <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "plot_multi_means()", "plot_metrics_items_grouped()", always = TRUE)
  plot_metrics_items_grouped(...)
}


#' Alias for report_styles
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `add_styles` has been deprecated. Use \link{report_styles} instead.
#'
#' @keywords internal
#' @export
add_styles <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "add_styles()", "report_styles()", always = TRUE)
  report_styles(...)
}


#' Alias for codebook
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `get_labels` has been deprecated. Use \link{codebook} instead.
#'
#' @keywords internal
#' @export
get_labels <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "get_labels()", "codebook()", always = TRUE)
  codebook(...)
}

#' Automatically detect the table type and output table and plot
#'
#' For each column prefix:
#' - If it matches a single column, use tab_counts_one and plot_counts_one
#' - If it matches multiple columns, use tab_counts_items and plot_counts_items
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `report` has been deprecated. Use \link{report_counts}) and \link{report_metrics} instead.
#'
#' @keywords internal
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
  lifecycle::deprecate_warn("1.0.0", "report()",I("report_counts() and report_metrics()"), always = TRUE)

  chunks <- list()

  # Quote col_group
  if (!missing(col_group)) {
    # Column without quotes
    if (rlang::quo_is_symbol(rlang::enquo(col_group))) {
      col_group <- rlang::enquo(col_group)
    }
    # Column as character
    else {
      #lifecycle::deprecate_warn("1.0.0", "report(col_group = 'Must be a column without quotation marks')")
      col_group <- rlang::sym(col_group)
    }
  }



  # Get item label from the attributes
  labels <-  codebook(data)

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
    is_scale <- get_direction(data, tidyselect::starts_with(scope), F)


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
      chunks <- .add_to_vlkr_rprt(paste0("\n#### ", scope_title, " {.tabset .tabset-pills}  \n"), chunks)
      plot_title <- F
    } else {
      plot_title <- T
    }


    # A single categorical variable
    if (is_var && is_scale == 0 && missing(col_group)) {
      chunks <- plot_counts(data, !!rlang::sym(scope), numbers = numbers, title = plot_title) %>%
        .add_to_vlkr_rprt(chunks, "Plot")

      chunks <- tab_counts(data, !!rlang::sym(scope)) %>%
        .add_to_vlkr_rprt(chunks, "Table")
    }

    # A single metric variable
    else if (is_var && is_scale != 0 && missing(col_group)) {
      chunks <- plot_metrics(data, !!rlang::sym(scope), title = plot_title) %>%
        .add_to_vlkr_rprt(chunks, "Plot")

      chunks <- tab_metrics(data, !!rlang::sym(scope)) %>%
        .add_to_vlkr_rprt(chunks, "Table")
    }

    # A single categorical variable by group
    else if (is_var && is_scale == 0 && !missing(col_group)) {
      chunks <- plot_counts(data, !!rlang::sym(scope), !!col_group, prop = prop, numbers = numbers, ordered = ordered, missings = missings, title = plot_title) %>%
        .add_to_vlkr_rprt(chunks, "Plot")

      chunks <- tab_counts(data, !!rlang::sym(scope), !!col_group, prop = prop, missings = missings) %>%
        .add_to_vlkr_rprt(chunks, "Table")
    }

    # A single metric variable by group
    else if (is_var && is_scale != 0 && !missing(col_group)) {
      chunks <- plot_metrics(data, !!rlang::sym(scope), !!col_group, title = plot_title) %>%
        .add_to_vlkr_rprt(chunks, "Plot")

      chunks <- tab_metrics(data, !!rlang::sym(scope), !!col_group) %>%
        .add_to_vlkr_rprt(chunks, "Table")
    }

    # Multiple items
    else if (is_items && missing(col_group)) {
      chunks <- plot_counts(data, tidyselect::starts_with(scope), numbers = numbers, ordered = ordered, title = plot_title) %>%
        .add_to_vlkr_rprt(chunks, "Plot")

      chunks <- tab_counts(data, tidyselect::starts_with(scope)) %>%
        .add_to_vlkr_rprt(chunks, "Table")
    }

    # Multiple items by group
    else if (is_items && !missing(col_group)) {
      chunks <- plot_metrics(data, tidyselect::starts_with(scope), !!col_group, title = plot_title) %>%
        .add_to_vlkr_rprt(chunks, "Plot")

      chunks <- tab_metrics(data, tidyselect::starts_with(scope), !!col_group) %>%
        .add_to_vlkr_rprt(chunks, "Table")
    } else {
      warning("Could't find columns to autodetect the table type for the scope ", scope, ". Check your parameters.")
    }


    # Output index
    if (index && is_items && (is_scale != 0)) {
      idx <- add_idx(data, tidyselect::starts_with(scope), newcol = ".idx")
      idx_name <- setdiff(colnames(idx), colnames(data))

      if ((length(idx_name) > 0) && missing(col_group)) {

        chunks <- idx %>%
          plot_metrics(!!rlang::sym(idx_name), title = plot_title) %>%
          .add_to_vlkr_rprt(chunks, "Index: Plot")

        chunks <- idx %>%
          tab_metrics(!!rlang::sym(idx_name)) %>%
          .add_to_vlkr_rprt(chunks, "Index: Table")

      } else if (length(idx_name) > 0) {
        chunks <- idx %>%
          plot_metrics(!!rlang::sym(idx_name), !!col_group, title = plot_title) %>%
          .add_to_vlkr_rprt(chunks, "Index: Plot")

        chunks <- idx %>%
          tab_metrics(!!rlang::sym(idx_name), !!col_group) %>%
          .add_to_vlkr_rprt(chunks, "Index: Table")
      }
    }


    # Close tabs
    if (close) {
      if (knitr::is_html_output()) {
        chunks <- .add_to_vlkr_rprt(paste0("\n##### {-}  \n"), chunks)
      }
      if (is.character(scope_title) && knitr::is_html_output()) {
        chunks <- .add_to_vlkr_rprt(paste0("\n#### {-}  \n"), chunks)
      }
    }

  }

  class(chunks) <- c("vlkr_rprt", class(chunks))
  chunks
}

#' Pill navigation for switching between plots and tables in markdown reports
#'
#' Call report_styles() in one of your first code chunks to let the magic happen.
#' Make sure the code chunk's include option is set to TRUE.
#' Outputs a style tag with css styles that modify the standard bootstrap
#' theme.
#'
#' See the html_report() format for an alternative to include the styles.
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `report_styles` has been deprecated. Use the volker::html_document template instead.
#'
#' @keywords internal
#' @export
report_styles <- function() {
  lifecycle::deprecate_warn("1.0.0", "report_styles()", always = TRUE)

  filename <-  paste0(system.file("extdata", package = "volker"),"/styles.css")
  styles <- readLines(filename)
  styles <- paste0(styles, collapse=" ")
  styles <- paste0("<style>", styles, "</style>")

  styles %>%
    knitr::asis_output() %>%
    knitr::knit_print()
}


#' Set column labels by their comment attribute
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `set_col_labels` has been deprecated. Use \link{labs_apply} instead.
#'
#' @param data A data frame
#' @param cols Tidyselect column names (e. g. a single column name without quotes)
#' @param labels A character vector with the same length as the column selection, containing new labels
#' @examples
#' ds <- volker::chatgpt
#' ds <- set_col_labels(ds, sd_age, "Age")
#'
#' @keywords internal
#' @export
set_col_labels <- function(data, cols, labels) {

  lifecycle::deprecate_warn("1.0.0", "set_col_labels()", "labs_apply()", always = TRUE)

  cols <- tidyselect::eval_select(expr = enquo(cols), data = data)
    for (i in c(1:length(cols))) {
    attr(data[[cols[i]]], "comment") <- labels[i]
  }
  data
}

#' Alias for set_col_labels
#'
#' #' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `set_col_labels` has been deprecated. Use \link{labs_apply} instead.
#' @keywords internal
#' @export
set_col_label <- set_col_labels
