#
# Constants
#

#' Levels to remove from factors
#'
#' Override with \code{options(vlkr.na.levels=c("Not answered"))}.
#'
#' @keywords internal
VLKR_NA_LEVELS <- c("[NA] nicht beantwortet", "[NA] keine Angabe")

#' Numbers to remove from vectors
#'
#' Override with \code{options(vlkr.na.numbers=c(-2,-9))}.
#'
#' @keywords internal
VLKR_NA_NUMERIC <- c(-9)

#' Fill colors
#'
#' @keywords internal
VLKR_FILLDISCRETE <- list(
  c("darkcyan"),
  c("#1B9E77", "#D95F02", "#7570B3", "#E7298A", "#66A61E", "#E6AB02", "#A6761D"),
  c("#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499",
    "#44AA99", "#999933", "#882255", "#661100", "#6699CC")
)

#' Gradient colors
#'
#' @keywords internal
VLKR_FILLGRADIENT <- c("#e5f7ff", "#96dfde", "#008b8b", "#006363", "#001212")

VLKR_COLOR_BOX_BACKGROUND <- "#aaaaaa"
VLKR_COLOR_BOX_FOREGROUND <- "#222222"
VLKR_COLOR_CI <- "#222222"

# Thresholds
VLKR_NORMAL_DIGITS <- 1
VLKR_SMALL_DIGITS <- 2

VLKR_LOWPERCENT <- 5
VLKR_SCATTER_ALPHA <- 0.3

VLKR_PLOT_DPI <- 192
VLKR_PLOT_SCALE <- 96
VLKR_PLOT_WIDTH <- 910
VLKR_PLOT_PXPERLINE <- 15
VLKR_PLOT_OFFSETROWS <- 5
VLKR_PLOT_TITLEROWS <- 2
VLKR_PLOT_LABELWRAP <- 40
