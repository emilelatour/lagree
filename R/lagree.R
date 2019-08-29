#' lagree:: Calculate various interrater agreement coefficients
#'
#' Calculate various agreement coefficients for 2 or more raters along with
#' standard errors and confidence intervals. Many packages are available to do
#' this, but this one returns neat tibbles with results.
#'
#' @examples
#' # Example usage:
#' library(lagree)
#'
#' @docType package
#' @name lagree
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
## From Jenny Bryan's googlesheets package
## From infer package
## https://github.com/tidymodels/infer/blob/master/R/infer.R
if (getRversion() >= "2.15.1") {
  utils::globalVariables(
    c(".",
      "raters",
      "ratings",
      "subject"
    )
  )
}
