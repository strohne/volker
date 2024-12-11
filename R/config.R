#
# Constants
#

#' Levels to remove from factors
#'
#' Override with \code{options(vlkr.na.levels=c("Not answered"))}.
#'
#' @keywords internal
VLKR_NA_LEVELS <- c("[NA] nicht beantwortet", "[NA] keine Angabe", "[no answer]")

#' Numbers to remove from vectors
#'
#' Override with \code{options(vlkr.na.numbers=c(-2,-9))}.
#'
#' @keywords internal
VLKR_NA_NUMBERS <- c(-9, -2, -1)

#' Get configured na numbers
#'
#' Retrieves values either from the option or from the constant.
#'
#' @keywords internal
#' return A vector with numbers that should be treated as NAs
cfg_get_na_numbers <- function() {
  na.numbers <- getOption("vlkr.na.numbers")
  if (is.null(na.numbers)) {
    na.numbers <- VLKR_NA_NUMBERS
  } else if (all(na.numbers == FALSE)) {
    na.numbers <- c()
  }
  na.numbers
}


#' Fill colors
#'
#' Override with \code{options(vlkr.discrete.fill=list(c("purple")))}.
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
#' Override with \code{options(vlkr.gradient.fill=list(c("white","black")))}.
#'
#' @keywords internal
VLKR_FILLGRADIENT <- c("#e5f7ff", "#96dfde", "#008b8b", "#006363", "#001212")

VLKR_COLOR_BOX_BACKGROUND <- "#aaaaaa"
VLKR_COLOR_BOX_FOREGROUND <- "#222222"
VLKR_COLOR_CI <- "#222222"
VLKR_COLOR_DISABLED <- "#aaaaaa"

#' Polarized colors
#'
#' @keywords internal
VLKR_FILLPOLARIZED <- c("#B34E00", "#D95F02", "#FFFFFF", "#66A61E","#3B660A")
VLKR_COLORPOLARIZED <- c("#FFFFFF")

#' Resolution settings for plots
#'
#' Override with \code{options(vlkr.fig.settings=list(html = list(dpi = 192, scale = 2, width = 910, pxperline = 15)))}.
#' Add a key for each output format when knitting a document.
#' You can override the width by setting vlkr.fig.width in the chunk options.
#'
#' @keywords internal
VLKR_FIG_SETTINGS <- list(
  html = list(dpi = 2 * 96, scale = 1.7, width = 910, pxperline = 15),

  # Results in 16,51 cm width at 300 dpi
  docx = list(dpi = 300, scale = 3, width = 650, pxperline = 15)
)

VLKR_PLOT_OFFSETROWS <- 5
VLKR_PLOT_TITLEROWS <- 2

#' Wrapping threshold
#'
#' Override with \code{options(vlkr.wrap.labels=20)}.
#' Override with \code{options(vlkr.wrap.legend=10)}.
#' Override with \code{options(vlkr.wrap.scale=10)}.
#'
#' @keywords internal
VLKR_PLOT_LABELWRAP <- 40
VLKR_PLOT_LEGENDWRAP <- 20
VLKR_PLOT_SCALEWRAP <- 10

#' Word wrap separators
#'
#' @keywords internal
VLKR_WRAP_SEPARATOR <- " |(?<=[/\\\\])"

#' Output thresholds
#'
#' @keywords internal
VLKR_NORMAL_DIGITS <- 1
VLKR_SMALL_DIGITS <- 2
VLKR_LOWPERCENT <- 5

#' Shapes
#'
#' @keywords internal
VLKR_POINT_MEAN_SHAPE <- 18
VLKR_POINT_COR_SHAPE <- 19

#' Alpha values
#'
#' @keywords internal
VLKR_POINT_ALPHA <- 0.3
VLKR_LINE_ALPHA <- 0.6

#' Sizes
#'
#' @keywords internal
VLKR_POINT_SIZE <-  3
