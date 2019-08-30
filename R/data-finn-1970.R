#' @title
#' Finn (1970): 5 raters classified 4 subjects into 3 categories
#'
#' @description
#' 5 raters classified 4 subjects into 3 categories labeled as 1, 2, 3.
#'
#'
#' @format A tibble with 4 observations (1, 2, 3) on 5 variables representing
#'   different raters and one variable for subject ID.
#' \describe{
#'   \item{subject}{Subject ID}
#'   \item{rater_I}{a factor including the ratings of rater_I (1, 2, 3)}
#'   \item{rater_II}{a factor including the ratings of rater_II (1, 2, 3)}
#'   \item{rater_III}{a factor including the ratings of rater_III (1, 2, 3)}
#'   \item{rater_IV}{a factor including the ratings of rater_IV (1, 2, 3)}
#'   \item{rater_V}{a factor including the ratings of rater_V (1, 2, 3)}
#' }
#'
#' @references
#' Finn, R.H. (1970). A note on estimating the reliability of categorical data. Educational and Psychological Measurement, 30, 71-76.
#' @source
#' Finn, R.H. (1970). A note on estimating the reliability of categorical data. Educational and Psychological Measurement, 30, 71-76.
#'
#' @examples
#' finn_1970
#'
#' library(dplyr)
#' library(tidyr)
#' finn_1970 %>%
#'   tidyr::gather(.,
#'                 key = "rater",
#'                 value = "rating",
#'                 dplyr::starts_with("rater")) %>%
#'   with(., table(subject, rating))
"finn_1970"
