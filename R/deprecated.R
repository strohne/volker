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


#' Alias for get_codebook
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' `get_labels` has been deprecated. Use \link{get_codebook} instead.
#'
#' @keywords internal
#' @export
get_labels <- function(...)
{
  lifecycle::deprecate_warn("1.0.0", "get_labels()", "get_codebook()", always = TRUE)
  get_codebook(...)
}
