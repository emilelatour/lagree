#' Calculate Agreement Coefficients (AC) from a contengency table
#'
#' This function calculates agreement coefficients, including Gwet's AC, Fleiss'
#' kappa, and others, when input dataset is a contingency table. It supports
#' various weight types and hypothesis testing options.
#'
#' @param table A q×q matrix (or contingency table) showing the distribution
#'   of subjects by rater, where q is the number of categories. This is
#'   the only argument you must specify if you want the unweighted analysis
#' @param weights The type of weighting to apply. One of "unweighted",
#'   "quadratic", "linear", "ordinal", "radical", "ratio", "circular",
#'   "bipolar", or a custom matrix of weights.
#' @param categ Optional. A vector of category labels. If not provided, the
#'   function will extract unique categories from the data.
#' @param conf_lev Confidence level for confidence intervals (default is 0.95).
#' @param N Population size for finite population correction (default is Inf).
#' @param test_value Hypothesis test value (default is 0).
#' @param alternative The type of alternative hypothesis. One of "two.sided",
#'   "less", or "greater".
#' @param show_weights Logical whether to show the weights matric with the
#'   results. Default is FALSE.
#'
#' @return A list containing: \item{summary}{A summary of the subjects, raters,
#'   and categories.} \item{ac_table}{A table with calculated agreement
#'   coefficients.} \item{hypothesis}{A string describing the hypothesis test.}
#'
#' @importFrom dplyr select mutate summarise rowwise ungroup everything
#' @importFrom purrr discard
#' @importFrom tidyr unnest
#' @importFrom tibble tibble
#' @importFrom stringr str_trim
#' @importFrom glue glue
#' @export
#'
#' @examples
#'
#' #### Example 2 --------------------------------
#'
#' (ratings <- matrix(c(118, 5,
#'                      2, 0), ncol = 2, byrow = TRUE))
#'
#' calc_ac_table(table = ratings)
#'
#'
#' calc_ac_table(table = ratings,
#'               test_value = 0.67,
#'               alternative = "greater")
#'
#' calc_ac_table(table = ratings,
#'               test_value = 0.67,
#'               alternative = "less")
#'
#'
#' #### Example 3 --------------------------------
#'
#' (ratings <- matrix(c(45, 15,
#'                      25, 15), ncol = 2, byrow = TRUE))
#'
#' calc_ac_table(table = ratings)
#'
#'
#' (ratings <- matrix(c(25, 35,
#'                      5, 35), ncol = 2, byrow = TRUE))
#'
#' calc_ac_table(table = ratings)
#'
#'
#' #### Example 4 --------------------------------
#'
#' (ratings <- matrix(c(1, 15, 1,
#'                      3, 0, 3,
#'                      2, 3, 2), ncol = 3, byrow = TRUE))
#'
#' calc_ac_table(table = ratings,
#'               weights = "quadratic",
#'               show_weights = TRUE)
#'
#'
#' (ratings <- matrix(c(1, 1, 1,
#'                      3, 17, 3,
#'                      2, 0, 2), ncol = 3, byrow = TRUE))
#'
#' calc_ac_table(table = ratings,
#'               weights = "quadratic",
#'               show_weights = TRUE)
calc_ac_table <- function(table,
                          weights = "unweighted",
                          categ = NULL,
                          conf_lev = 0.95,
                          N = Inf,
                          test_value = 0,
                          alternative = "two.sided",
                          show_weights = FALSE) {

  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop('alternative must be one of "two.sided", "less", "greater".')
  }

  ratings <- as.matrix(table)
  if(dim(ratings)[1] != dim(ratings)[2]){
    stop('The contingency table should have the same number of rows and columns.')
  }


  # Calc number of subjects and categories
  n <- sum(ratings)  # number of subjects
  q <- ncol(ratings) # number of categories
  f <- n / N  # finite population correction

  rating_stats <- tibble::tibble(n_subjects = n,
                                 k_raters = 2,
                                 q_categories = q)


  # Create weights matrix
  wts_res <- get_ac_weights(weights = weights,
                            q = q)

  weights_name <- wts_res$w_name
  weights_mat <- wts_res$weights_mat


  # Combine results from various agreement calculations
  results_table <- dplyr::bind_rows(
    pa_2_table(table = table,
               weights = weights_mat,
               conf_lev = conf_lev,
               N = N,
               test_value = test_value,
               alternative = alternative),
    bp_2_table(table = table,
               weights = weights_mat,
               conf_lev = conf_lev,
               N = N,
               test_value = test_value,
               alternative = alternative),
    kappa_2_table(table = table,
                  weights = weights_mat,
                  conf_lev = conf_lev,
                  N = N,
                  test_value = test_value,
                  alternative = alternative),
    scott_2_table(table = table,
                  weights = weights_mat,
                  conf_lev = conf_lev,
                  N = N,
                  test_value = test_value,
                  alternative = alternative),
    gwet_ac1_table(table = table,
                   weights = weights_mat,
                   conf_lev = conf_lev,
                   N = N,
                   test_value = test_value,
                   alternative = alternative),
    krippen_2_table(table = table,
                    weights = weights_mat,
                    conf_lev = conf_lev,
                    N = N,
                    test_value = test_value,
                    alternative = alternative))


  # Generate hypothesis text

  hyp_txt <- calc_test_txt(u = test_value,
                           alternative = alternative)



  # Return summary, results table, and hypothesis
  if (show_weights) {
    res <- list(rating_stats,
                results_table,
                hyp_txt,
                weights_mat)

    names(res) <- c("summary", "ac_table", "hypothesis", weights_name)

  } else {
    res <- list(rating_stats,
                results_table,
                hyp_txt)

    names(res) <- c("summary", "ac_table", "hypothesis")

  }


  return(res)

}
