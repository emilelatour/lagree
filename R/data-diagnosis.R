#' @title
#' Fleiss (1971): Diagnoses on 30 subjects by 6 raters per subject
#'
#' @description
#' Psychiatric diagnoses of 30 patients provided by 6 raters. Data were used by
#' Fleiss (1971) to illustrate the computation of Kappa for multiple raters.
#'
#' @format A tibble with 30 observations (psychiatric diagnoses with levels
#'   Depression, Personality Disorder, Schizophrenia, Neurosis, Other) on 6
#'   variables representing different raters and one variable for subject ID.
#' \describe{
#'   \item{subject}{Subject ID}
#'   \item{rater1}{a factor including the diagnoses of rater 1 (levels see above)}
#'   \item{rater2}{a factor including the diagnoses of rater 2 (levels see above)}
#'   \item{rater3}{a factor including the diagnoses of rater 3 (levels see above)}
#'   \item{rater4}{a factor including the diagnoses of rater 4 (levels see above)}
#'   \item{rater5}{a factor including the diagnoses of rater 5 (levels see above)}
#'   \item{rater6}{a factor including the diagnoses of rater 6 (levels see above)}
#' }
#'
#' @references
#' Fleiss, J.L. (1971). Measuring nominal scale agreement among many raters. Psychological Bulletin, 76, 378-382.
#' @source
#' Fleiss, J.L. (1971). Measuring nominal scale agreement among many raters. Psychological Bulletin, 76, 378-382.
#'
#' @examples
#' diagnosis
#'
#' library(dplyr)
#' library(tidyr)
#' # Table 1, Fleiss (1971): Diagnoses on 30 subjects by 6 raters per subject
#' diagnosis %>%
#'   tidyr::gather(.,
#'                 key = "rater",
#'                 value = "rating",
#'                 dplyr::starts_with("rater")) %>%
#'   with(., table(subject, rating))
"diagnosis"
