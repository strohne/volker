#
# Deprecated or aliased functions
#


#' Alias for report_styles
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#' `add_styles()` has been deprecated. Use `report_styles()` instead.
#'
#' @rdname report_styles
#' @keywords internal
#' @export
add_styles <- function(...) {
  lifecycle::deprecate_warn("1.0.0", "add_styles()", "report_styles()")
  report_styles(...)
}

