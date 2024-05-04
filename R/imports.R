#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

#' Filter function
#'
#' See \code{dplyr::\link[dplyr:filter]{filter}} for details.
#'
#' @name filter
#' @rdname filter
#' @keywords internal
#' @export
#' @importFrom dplyr filter
NULL

#' Select function
#'
#' See \code{dplyr::\link[dplyr:select]{select}} for details.
#'
#' @name select
#' @rdname select
#' @keywords internal
#' @export
#' @importFrom dplyr select
NULL

#' Mutate function
#'
#' See \code{dplyr::\link[dplyr:mutate]{mutate}} for details.
#'
#' @name mutate
#' @rdname mutate
#' @keywords internal
#' @export
#' @importFrom dplyr mutate
NULL

#' Select variables by their prefix
#'
#' See \code{tidyselect::\link[tidyselect:starts_with]{starts_with}} for details.
#'
#' @name starts_with
#' @rdname starts_with
#' @keywords internal
#' @export
#' @importFrom tidyselect starts_with
NULL

#' Select variables by their postfix
#'
#' See \code{tidyselect::\link[tidyselect:ends_with]{ends_with}} for details.
#'
#' @name ends_with
#' @rdname ends_with
#' @keywords internal
#' @export
#' @importFrom tidyselect ends_with
NULL

#' Get, set, and modify the active ggplot theme
#'
#' See \code{ggplot2::\link[ggplot2:theme_set]{theme_set}} for details.
#'
#' @name theme_set
#' @rdname theme_set
#' @keywords internal
#' @export
#' @importFrom ggplot2 theme_set
NULL
