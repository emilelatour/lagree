#' Calculate Agreement Coefficients
#'
#' This function computes various agreement coefficients based on the type of
#' input data provided, including raw data, contingency tables, or
#' distributions. It automatically selects the appropriate calculation method
#' based on whether `data`, `table`, or `distribution` is provided.
#'
#' @param data A data frame containing raw ratings data. Only used if `table`
#'   and `distribution` are NULL.
#' @param ... Columns within the `data` for calculating agreement coefficients.
#'   Used only with `data`.
#' @param table A contingency table of ratings. Only used if `data` and
#'   `distribution` are NULL.
#' @param distribution A distribution matrix of ratings. Only used if `data` and
#'   `table` are NULL.
#' @param weights The weighting method for agreement calculation. Default is
#'   "unweighted".
#' @param categ A vector of category names, optional.
#' @param conf_lev Confidence level for confidence intervals. Default is 0.95.
#' @param N Population size for finite population correction. Default is `Inf`.
#' @param test_value The null hypothesis value for hypothesis testing. Default
#'   is 0.
#' @param alternative The alternative hypothesis to test: "two.sided", "less",
#'   or "greater". Default is "two.sided".
#' @param show_weights Logical, whether to include the weights matrix in the
#'   output. Default is FALSE.
#'
#' @return A list containing the summary of the input, agreement coefficient
#'   table, and hypothesis text. If `show_weights` is TRUE, the weights matrix
#'   is included in the output.
#'
#' @examples
#' library(tidyverse)
#' library(janitor)
#'
#' #### Example 1 --------------------------------
#'
#' # two radiologists who classify 85 xeromammograms into one of four categories
#' # (Altman p. 403)
#' radiologist
#'
#' radiologist |>
#'   janitor::tabyl(radiologist_a, radiologist_b) |>
#'   janitor::adorn_totals(where = c("row", "col")) |>
#'   janitor::adorn_title(placement = "combined")
#'
#' ## With a raw data frame ----------------
#'
#' calc_ac(data = radiologist,
#'         radiologist_a, radiologist_b)
#'
#' ## With a table ----------------
#'
#' (tab <- with(radiologist,
#'              table(radiologist_a, radiologist_b)))
#'
#' calc_ac(table = tab)
#'
#' ## With a distribution ----------------
#'
#' ex_dist <- radiologist |>
#'   mutate(subject = dplyr::row_number()) |>
#'   calc_agree_mat(dplyr::starts_with("radiologist"),
#'                  subject_id = subject)
#'
#' ex_dist
#'
#' calc_ac(distribution = ex_dist)
#'
#'
#' #### Example 5 --------------------------------
#'
#' #  5 raters classify 10 subjects into 1 of 3 rating categories
#' rvary2
#'
#'
#' ## With a raw data frame ----------------
#'
#' calc_ac(data = rvary2,
#'         dplyr::starts_with("rater"))
#'
#' ## With a table ----------------
#'
#' # Only works for 2x2 tables. This example has more than 2 raters.
#'
#' ## With a distribution ----------------
#'
#' ex_dist <- calc_agree_mat(data = rvary2,
#'                           dplyr::starts_with("rater"),
#'                           subject_id = subject)
#'
#' ex_dist
#'
#' calc_ac(distribution = ex_dist)
#'
#' @export
calc_ac <- function(data = NULL,
                    ...,
                    table = NULL,
                    distribution = NULL,
                    weights = "unweighted",
                    categ = NULL,
                    conf_lev = 0.95,
                    N = Inf,
                    test_value = 0,
                    alternative = "two.sided",
                    show_weights = FALSE) {

  # Check if exactly one of data, table, or distribution is not NULL
  if (sum(!is.null(c(data, table, distribution))) != 1) {
    stop("Please provide exactly one of 'data', 'table', or 'distribution'.")
  }

  # Call calc_ac_raw if 'data' is provided
  if (!is.null(data)) {
    return(calc_ac_raw(data = data,
                       ...,
                       weights = weights,
                       categ = categ,
                       conf_lev = conf_lev,
                       N = N,
                       test_value = test_value,
                       alternative = alternative,
                       show_weights = show_weights))
  }

  # Call calc_ac_table if 'table' is provided
  if (!is.null(table)) {
    return(calc_ac_table(table = table,
                         weights = weights,
                         categ = categ,
                         conf_lev = conf_lev,
                         N = N,
                         test_value = test_value,
                         alternative = alternative,
                         show_weights = show_weights))
  }

  # Call calc_ac_dist if 'distribution' is provided
  if (!is.null(distribution)) {
    return(calc_ac_dist(distribution = distribution,
                        weights = weights,
                        categ = categ,
                        conf_lev = conf_lev,
                        N = N,
                        test_value = test_value,
                        alternative = alternative,
                        show_weights = show_weights))
  }
}


# # Another way with a single input argument.
# calc_ac <- function(input,
#                     ...,
#                     weights = "unweighted",
#                     categ = NULL,
#                     conf_lev = 0.95,
#                     N = Inf,
#                     test_value = 0,
#                     alternative = "two.sided",
#                     show_weights = FALSE) {
#
#   # Check if the input is a data frame (for raw data)
#   if (is.data.frame(input)) {
#     return(calc_ac_raw(data = input,
#                        # !!! rlang::enquos(...),
#                        ...,
#                        weights = weights,
#                        categ = categ,
#                        conf_lev = conf_lev,
#                        N = N,
#                        test_value = test_value,
#                        alternative = alternative,
#                        show_weights = show_weights))
#   }
#
#   # Check if the input is a matrix with the same number of rows and columns (for tables)
#   if (is.matrix(input) && dim(input)[1] == dim(input)[2]) {
#     return(calc_ac_table(table = input,
#                          weights = weights,
#                          categ = categ,
#                          conf_lev = conf_lev,
#                          N = N,
#                          test_value = test_value,
#                          alternative = alternative,
#                          show_weights = show_weights))
#   }
#
#   # Check if the input is a matrix but not a square matrix (for distributions)
#   if (is.matrix(input) && dim(input)[1] != dim(input)[2]) {
#     return(calc_ac_dist(distribution = input,
#                         weights = weights,
#                         categ = categ,
#                         conf_lev = conf_lev,
#                         N = N,
#                         test_value = test_value,
#                         alternative = alternative,
#                         show_weights = show_weights))
#   }
#
#   # If input doesn't match any expected type, throw an error
#   stop("Input format not recognized. Please provide either raw data (data.frame), a contingency table (square matrix), or a distribution matrix (non-square matrix).")
# }

