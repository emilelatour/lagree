
#' Calculate Agreement Coefficients (AC) from a distribution
#'
#' This function calculates agreement coefficients, including Gwet's AC, Fleiss'
#' kappa, and others, when the input dataset is the distribution of raters by
#' subject and category. It supports various weight types and hypothesis testing
#' options.
#'
#' @param distribution An \emph{nxq} matrix / data frame containing the distribution
#'   of raters by subject and category. Each cell \emph{(i,k)} contains the
#' number of raters who classified subject \emph{i} into category \emph{k}. An n
#' x q agreement matrix representing the distribution of raters by subjects (n)
#' and category (q) (see `calc_agree_mat` to convert raw data to distribution).
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
#' library(irrCAC)
#'
#' rvary2
#'
#' ex_dist <- calc_agree_mat(data = rvary2,
#'                           dplyr::starts_with("rater"),
#'                           subject_id = subject)
#'
#' ex_dist
#'
#' calc_ac_dist(distribution = ex_dist)
#'
#' # Compare to raw
#' calc_ac_raw(data = rvary2,
#'             dplyr::starts_with("rater"))
#'
#' # Compare to irrCAC package
#' dplyr::bind_rows(
#'   irrCAC::pa.coeff.dist(ratings = ex_dist),
#'   irrCAC::bp.coeff.dist(ratings = ex_dist),
#'   irrCAC::fleiss.kappa.dist(ratings = ex_dist),
#'   irrCAC::gwet.ac1.dist(ratings = ex_dist),
#'   irrCAC::krippen.alpha.dist(ratings = ex_dist)
#' )
calc_ac_dist <- function(distribution,
                         weights = "unweighted",
                         categ = NULL,
                         conf_lev = 0.95,
                         N = Inf,
                         test_value = 0,
                         alternative = "two.sided",
                         show_weights = FALSE) {

  # Check for valid alternative hypothesis
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop('alternative must be one of "two.sided", "less", "greater".')
  }


  agree.mat <- as.matrix(distribution)
  n <- nrow(agree.mat)  # number of subjects
  q <- ncol(agree.mat)  # number of categories
  f <- n / N  # finite population correction


  # Calculate number of raters, subjects, and categories

  rtngs_per_subj <- rowSums(agree.mat)

  subj_counts <- tibble::tibble(ratings_per_subject_min = min(rtngs_per_subj),
                                ratings_per_subject_avg = mean(rtngs_per_subj),
                                ratings_per_subject_max = max(rtngs_per_subj))

  if (length(unique(as.vector(as.matrix(dplyr::slice(subj_counts, 1))))) == 1) {

    subj_counts <- subj_counts |>
      dplyr::select(ratings_per_subject = 1)

  }

  rating_stats <- tibble::tibble(n_subjects = n,
                                 # k_raters = ncol(dplyr::select(data, ...)),
                                 res = subj_counts,
                                 q_categories = q) |>
    tidyr::unnest(res)



  # Set or infer categories
  if (is.null(categ)) {
    categ <- 1:q
  } else {
    q2 <- length(categ)
    categ <- if (!is.numeric(categ)) 1:q2 else categ

    # Adjust matrix dimensions if needed
    if (q2 > q) {
      colna1 <- colnames(agree.mat)
      agree.mat <- cbind(agree.mat, matrix(0, n, q2 - q))
      colnames(agree.mat) <- c(colna1, paste0("v", 1:(q2 - q)))
      q <- q2
    }
  }


  # Create weights matrix
  wts_res <- get_ac_weights(weights = weights,
                            q = q)

  weights_name <- wts_res$w_name
  weights_mat <- wts_res$weights_mat


  # Combine results from various agreement calculations
  results_table <- dplyr::bind_rows(
    pa_3_dist(distribution = distribution,
              weights = weights_mat,
              categ = NULL,
              conf_lev = conf_lev,
              N = N,
              test_value = test_value,
              alternative = alternative),
    bp_3_dist(distribution = distribution,
              weights = weights_mat,
              categ = NULL,
              conf_lev = conf_lev,
              N = N,
              test_value = test_value,
              alternative = alternative),
    conger_3_dist(distribution = distribution,
                  weights = weights_mat,
                  categ = NULL,
                  conf_lev = conf_lev,
                  N = N,
                  test_value = test_value,
                  alternative = alternative),
    fleiss_3_dist(distribution = distribution,
                  weights = weights_mat,
                  categ = NULL,
                  conf_lev = conf_lev,
                  N = N,
                  test_value = test_value,
                  alternative = alternative),
    gwet_3_dist(distribution = distribution,
                weights = weights_mat,
                categ = NULL,
                conf_lev = conf_lev,
                N = N,
                test_value = test_value,
                alternative = alternative),
    krippen_3_dist(distribution = distribution,
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
