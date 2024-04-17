#
# Constants
#

#' Levels to remove from factors
#'
#' By default the value `[NA] nicht beantwortet` is removed from tables and plots
#'
#' @keywords internal
VLKR_NA_LEVELS <- c("[NA] nicht beantwortet")

#' Numbers to remove from vectors
#'
#' By default the value `-9` is removed  from tables and plots
#'
#' @keywords internal
VLKR_NA_NUMERIC <- c(-9)

# Plot settings
VLKR_POINTCOLOR <- "darkcyan"  #"#611F53FF"
VLKR_FILLCOLOR <- "darkcyan"  #"#611F53FF"
VLKR_FILLDISCRETE <- c(
  "#e41a1c", "#377eb8", "#4daf4a", "#984ea3",
  "#ff7f00","#ffff33","#a65628","#f781bf",
  "#cab2d6", "#a6cee3","#fdbf6f","#b2df8a"
)
VLKR_FILLGRADIENT <- c("#e5f7ff", "#96dfde", "#008b8b", "#006363", "black")
VLKR_LOWPERCENT <- 5

VLKR_PLOT_DPI <- 192
VLKR_PLOT_SCALE <- 96
VLKR_PLOT_WIDTH <- 910
VLKR_PLOT_PXPERLINE <- 15
VLKR_PLOT_OFFSETROWS <- 5
VLKR_PLOT_TITLEROWS <- 2
VLKR_PLOT_LABELWRAP <- 40
