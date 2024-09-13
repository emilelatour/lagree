#' Calculate Agreement Coefficients (AC)
#'
#' This function calculates agreement coefficients, including Gwet's AC, Fleiss'
#' kappa, and others, based on the input dataset. It supports various weight
#' types and hypothesis testing options.
#'
#' @param data A data frame containing the raw ratings data.
#' @param ... Columns in the data to be used for calculating agreement
#'   coefficients.
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
#' calc_ac_raw(data = radiologist,
#'             radiologist_a, radiologist_b)
#'
#' # Compare to Stata
#' # . webuse rate2, clear
#' # (Altman p. 403)
#' #
#' # . kappaetc rada radb
#' #
#' # Interrater agreement                             Number of subjects =      85
#' #                                                 Ratings per subject =       2
#' #                                         Number of rating categories =       4
#' # ------------------------------------------------------------------------------
#' #                      |   Coef.  Std. Err.    t    P>|t|   [95% Conf. Interval]
#' # ---------------------+--------------------------------------------------------
#' #    Percent Agreement |  0.6353    0.0525  12.10   0.000     0.5309     0.7397
#' # Brennan and Prediger |  0.5137    0.0700   7.34   0.000     0.3745     0.6530
#' # Cohen/Conger's Kappa |  0.4728    0.0731   6.46   0.000     0.3273     0.6182
#' #     Scott/Fleiss' Pi |  0.4605    0.0781   5.89   0.000     0.3051     0.6159
#' #            Gwet's AC |  0.5292    0.0679   7.80   0.000     0.3942     0.6642
#' # Krippendorff's Alpha |  0.4637    0.0781   5.93   0.000     0.3083     0.6191
#' # ------------------------------------------------------------------------------
#'
#'
#' #### Example 5 --------------------------------
#'
#' #  5 raters classify 10 subjects into 1 of 3 rating categories
#' rvary2
#'
#' calc_ac_raw(data = rvary2,
#'             dplyr::starts_with("rater"))
#'
#' # Compare to Stata
#' # . webuse rvary2, clear
#' #
#' # . kappaetc rater1-rater5
#' #
#' # Interrater agreement                             Number of subjects =      10
#' #                                            Ratings per subject: min =       3
#' #                                                                 avg =     4.7
#' #                                                                 max =       5
#' #                                         Number of rating categories =       3
#' # ------------------------------------------------------------------------------
#' #                      |   Coef.  Std. Err.    t    P>|t|   [95% Conf. Interval]
#' # ---------------------+--------------------------------------------------------
#' #    Percent Agreement |  0.5833    0.0759   7.69   0.000     0.4117     0.7550
#' # Brennan and Prediger |  0.3750    0.1138   3.29   0.009     0.1175     0.6325
#' # Cohen/Conger's Kappa |  0.3854    0.1047   3.68   0.005     0.1485     0.6224
#' #  Scott/Fleiss' Kappa |  0.3586    0.1207   2.97   0.016     0.0856     0.6316
#' #            Gwet's AC |  0.3829    0.1145   3.34   0.009     0.1238     0.6420
#' # Krippendorff's Alpha |  0.3897    0.1226   3.18   0.011     0.1122     0.6671
#' # ------------------------------------------------------------------------------
#'
#'
#' # Some researchers (for example, Hayes and Krippendorff [2007]) have suggested
#' # computing the probability that an AC fails to reach some required minimum
#' # level. This approach seems most popular when Krippendorff’s alpha is
#' # estimated. Typically, a minimum of κα > 0.8 or at least κα > 0.67 is
#' # recommended
#' calc_ac_raw(data = rvary2,
#'             dplyr::starts_with("rater"),
#'             test_value = 0.67,
#'             alternative = "greater")
#'
#' # Compare to Stata
#' # .  kappaetc, testvalue(< 0.67) noheader
#' # ------------------------------------------------------------------------------
#' #                      |   Coef.  Std. Err.    t     P>t    [95% Conf. Interval]
#' # ---------------------+--------------------------------------------------------
#' #    Percent Agreement |  0.5833    0.0759  -1.14   0.859     0.4117     0.7550
#' # Brennan and Prediger |  0.3750    0.1138  -2.59   0.985     0.1175     0.6325
#' # Cohen/Conger's Kappa |  0.3854    0.1047  -2.72   0.988     0.1485     0.6224
#' #  Scott/Fleiss' Kappa |  0.3586    0.1207  -2.58   0.985     0.0856     0.6316
#' #            Gwet's AC |  0.3829    0.1145  -2.51   0.983     0.1238     0.6420
#' # Krippendorff's Alpha |  0.3897    0.1226  -2.29   0.976     0.1122     0.6671
#' # ------------------------------------------------------------------------------
#' #  t test Ho: Coef. <=    0.6700  Ha: Coef. >   0.6700
#'
calc_ac_raw <- function(data,
                        ...,
                        weights = "unweighted",
                        categ = NULL,
                        conf_lev = 0.95,
                        N = Inf,
                        test_value = 0,
                        alternative = "two.sided",
                        show_weights = FALSE) {

  # Silence warning: no visible binding for global variable
  subj_count <- NULL

  # Validate input for the alternative hypothesis
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop('alternative must be one of "two.sided", "less", "greater"')
  }


  # Get the categories from factor levels or unique values in data
  fac_lvls <- unique(unlist(dplyr::select(data, ...)))

  # If factor levels exist, use them, otherwise extract unique non-NA values
  if (is.null(levels(fac_lvls))) {
    fac_lvls <- unique(na.omit(fac_lvls))
  } else {
    fac_lvls <- levels(fac_lvls)
  }

  # Calculate number of raters, subjects, and categories

  # Count the number of subjects
  subj_counts <- data |>
    dplyr::select(...) |>
    dplyr::rowwise() |>
    mutate(subj_count = sum(!is.na(dplyr::c_across(dplyr::everything())))) |>
    ungroup() |>
    summarise(ratings_per_subject_min = min(subj_count),
              ratings_per_subject_avg = mean(subj_count),
              ratings_per_subject_max = max(subj_count))

  if (length(unique(as.vector(as.matrix(dplyr::slice(subj_counts, 1))))) == 1) {

    subj_counts <- subj_counts |>
      dplyr::select(ratings_per_subject = 1)

  }

  rating_stats <- tibble::tibble(n_subjects = nrow(data),
                                 k_raters = ncol(dplyr::select(data, ...)),
                                 res = subj_counts,
                                 q_categories = length(fac_lvls)) |>
    tidyr::unnest(res)



  # Select and process ratings data
  ratings <- data |>
    dplyr::select(...)

  ratings.mat <- as.matrix(ratings)

  if (is.character(ratings.mat)) {
    ratings.mat <- stringr::str_trim(toupper(ratings.mat), side = "both")
    ratings.mat[ratings.mat == ''] <- NA_character_
  }

  # Create or infer categories
  if (is.null(categ)) {
    categ <- sort(unique(na.omit(as.vector(ratings.mat))))
  } else {
    categ <- toupper(categ)
  }
  q <- length(categ)

  # Create weights matrix
  wts_res <- get_ac_weights(weights = weights,
                            q = q)

  weights_name <- wts_res$w_name
  weights_mat <- wts_res$weights_mat


  # Combine results from various agreement calculations
  results_table <- dplyr::bind_rows(
    pa_3_raw(data = data,
             ...,
             weights = weights_mat,
             categ = NULL,
             conf_lev = conf_lev,
             N = N,
             test_value = test_value,
             alternative = alternative),
    bp_3_raw(data = data,
             ...,
             weights = weights_mat,
             categ = NULL,
             conf_lev = conf_lev,
             N = N,
             test_value = test_value,
             alternative = alternative),
    conger_3_raw(data = data,
                 ...,
                 weights = weights_mat,
                 categ = NULL,
                 conf_lev = conf_lev,
                 N = N,
                 test_value = test_value,
                 alternative = alternative),
    fleiss_3_raw(data = data,
                 ...,
                 weights = weights_mat,
                 categ = NULL,
                 conf_lev = conf_lev,
                 N = N,
                 test_value = test_value,
                 alternative = alternative),
    gwet_3_raw(data = data,
               ...,
               weights = weights_mat,
               categ = NULL,
               conf_lev = conf_lev,
               N = N,
               test_value = test_value,
               alternative = alternative),
    krippen_3_raw(data = data,
                  ...,
                  weights = weights_mat,
                  categ = NULL,
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



