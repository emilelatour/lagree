#' @title
#' Landis and Koch (1977): Diagnostic Classification of Multiple Sclerosis Patients by Two Neurologists.
#'
#' @description
#' Two neurologists classified 69 patients into 4 diagnostic categories.
#'
#'
#' @format A tibble with 69 observations (1, 2, 3, 4) on 2 variables representing
#'   different raters.
#' \describe{
#'   \item{new_orleans}{ratings of a neurologist from New Orleans (1, 2, 3, 4)}
#'   \item{winnipeg}{ratings of a neurologist from Winnipeg (1, 2, 3, 4)}
#' }
#'
#' @references
#' Landis, J., & Koch, G. (1977). The Measurement of Observer Agreement for Categorical Data. Biometrics, 33(1), 159-174. doi:10.2307/2529310
#' @source
#' Landis, J., & Koch, G. (1977). The Measurement of Observer Agreement for Categorical Data. Biometrics, 33(1), 159-174. doi:10.2307/2529310
#'
#' @examples
#' neurologists
#'
#' library(dplyr)
#' library(tidyr)
#' neurologists %>%
#'   with(., table(new_orleans, winnipeg))
"neurologists"
