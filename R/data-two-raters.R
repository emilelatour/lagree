#' @title
#' Generic ratings by two raters
#'
#' @description
#' 125 subjects rated by two raters
#'
#'
#' @format A tibble with 125 observations (1, 2) on 2 variables representing
#'   different raters.
#' \describe{
#'   \item{rater_a}{ratings of rater a (1, 2)}
#'   \item{rater_a}{ratings of rater b (1, 2)}
#' }
#'
#' @references
#' I forget where or why I found this one.
#' @source
#' I forget where or why I found this one.
#'
#' @examples
#' two_raters
#'
#' library(dplyr)
#' library(tidyr)
#' two_raters %>%
#'   with(., table(rater_a, rater_b))
"two_raters"
