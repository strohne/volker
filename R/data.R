#' ChatGPT Adoption Dataset CG-GE-APR23
#'
#' A small random subset of data from a survey about ChatGPT
#' adoption. The survey was conducted in April 2023 within
#' the population of German Internet users.
#'
#' Call codebook(volker::chatgpt) to see the items and and answer options.
#'
#' @format ## `chatgpt`
#' A data frame with 101 rows and 19 columns:
#' \describe{
#'   \item{case}{A running case number}
#'   \item{adopter}{Adoption groups inspired by Roger's innovator typology.}
#'   \item{use_}{Columns starting with use contain data about ChatGPT usage in different contexts.}
#'   \item{cg_activities}{Text answers to the question, what the respondents do with ChatGPT.}
#'   \item{cg_adoption_}{A scale consisting of items about advantages, fears, and social aspects.
#'                       The scales match theoretical constructs inspired by Roger's diffusion model and Davis' Technology Acceptance Model}
#'   \item{sd_}{Columns starting with sd contain sociodemographics of the respondents.}
#' }
#' @source Communication Department of the University of Münster (<gehrau@uni-muenster.de>).
"chatgpt"
