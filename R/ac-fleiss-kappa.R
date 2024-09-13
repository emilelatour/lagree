#' @name fleiss_3_raw
#'
#' @title
#' Fleiss' generalized kappa coefficient among multiple raters (2, 3, +)
#'
#' @description
#' Fleiss' generalized kappa among multiple raters (2, 3, +) when the input data
#' represent the raw ratings reported for each subject and each rater.
#'
#' @param data A data frame or tibble
#' @param ... Variable (column) names containing the ratings where each column
#'   represents one rater and each row one subject.
#' @param weights is an optional parameter that is either a string variable or a
#'   matrix. The string describes one of the predefined weights and must take
#'   one of the values ("quadratic", "ordinal", "linear", "radical", "ratio",
#'   "circular", "bipolar"). If this parameter is a matrix then it must be a
#'   square matrix qxq where q is the number of possible categories where a
#'   subject can be classified. If some of the q possible categories are not
#'   used, then it is strongly advised to specify the complete list of possible
#'   categories as a vector in parameter `categ`. Otherwise, only the categories
#'   reported will be used.
#' @param categ An optional parameter representing all categories available to
#'   raters during the experiment. This parameter may be useful if some
#'   categories were not used by any rater in spite of being available to the
#' raters, they will still be used when calculating agreement coefficients. The
#' default value is NULL. In this case, only categories reported by the raters
#' are used in the calculations.
#' @param conf_lev The confidence level associated with the agreement
#'   coefficient’s confidence interval. Default is 0.95.
#' @param N An optional parameter representing the total number of subjects in
#'   the target subject population. Its default value is infinity, which for all
#'   practical purposes assumes the target subject population to be very large
#'   and will not require any finite-population correction when computing the
#'   standard error.
#' @param test_value value to test the estimated AC against. Default is 0.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of "two.sided" (default), "greater" or "less".
#'
#' @references
#' 2014. Handbook of Inter-Rater Reliability: The Definitive Guide to Measuring
#' the Extent of Agreement Among Raters. 4th ed. Gaithersburg, MD: Advanced
#' Analytics.
#'
#' Fleiss, J. L. (1981). Statistical Methods for Rates and Proportions. John
#' Wiley & Sons.
#'
#' @return
#' A tbl_df with the coefficient, standard error, lower and upper confidence
#' limits.
#' @export
#'
#' @examples
#' #  5 raters classify 10 subjects into 1 of 3 rating categories
#' rvary2
#'
#' # More than two raters
#' fleiss_3_raw(data = rvary2,
#'              dplyr::starts_with("rater"))
#'
#' # Two raters
#' fleiss_3_raw(data = rvary2,
#'              rater1:rater2)
#'
#' # Another example with two raters
#' # two radiologists who classify 85 xeromammograms into one of four categories
#' # (Altman p. 403)
#' radiologist
#'
#' fleiss_3_raw(data = radiologist,
#'              radiologist_a, radiologist_b)
fleiss_3_raw <- function(data,
                         ...,
                         weights = "unweighted",
                         categ = NULL,
                         conf_lev = 0.95,
                         N = Inf,
                         test_value = 0,
                         alternative = "two.sided") {

  # Check for valid alternative hypothesis
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop('alternative must be one of "two.sided", "less", "greater".')
  }

  # Select and process ratings data
  ratings <- data |>
    dplyr::select(...)

  ratings.mat <- as.matrix(ratings)

  if (is.character(ratings.mat)) {
    ratings.mat <- trimws(toupper(ratings.mat), which = "both")
    ratings.mat[ratings.mat == ''] <- NA_character_
  }


  # Calc number of subjects and categories
  n <- nrow(ratings.mat) # number of subjects
  r <- ncol(ratings.mat) # number of raters
  f <- n / N # finite population correction


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


  # Create agreement matrix
  agree.mat <- matrix(0,
                      nrow = n,
                      ncol = q)

  for (k in 1:q) {
    categ.is.k <- (ratings.mat == categ[k])
    agree.mat[, k] <- (replace(categ.is.k, is.na(categ.is.k), FALSE)) %*% rep(1, r)

  }

  agree.mat.w <- t(weights_mat%*%t(agree.mat))


  # Calculate Percent Agreement and Fleiss' Kappa
  ri.vec <- agree.mat %*% rep(1, q)
  sum.q <- (agree.mat * (agree.mat.w - 1)) %*% rep(1, q)
  n2more <- sum(ri.vec >= 2)
  pa <- sum(sum.q[ri.vec >= 2] / ((ri.vec * (ri.vec - 1))[ri.vec >= 2])) / n2more

  pi.vec <- t(t(rep(1 / n, n)) %*% (agree.mat / (ri.vec %*% t(rep(1, q)))))

  if (q >= 2) {
    pe <- sum(weights_mat * (pi.vec %*% t(pi.vec)))
  } else {
    pe = 1e-15
  }

  fleiss.kappa <- (pa - pe) / (1 - pe)


  # Variance and Standard Error
  den.ivec <- ri.vec * (ri.vec - 1)
  den.ivec <- den.ivec - (den.ivec == 0) # this operation replaces each 0 value with -1 to make the next ratio calculation always possible.
  pa.ivec <- sum.q / den.ivec

  pe.r2 <- pe * (ri.vec >= 2)
  kappa.ivec <- (n / n2more) * (pa.ivec - pe.r2) / (1 - pe)
  pi.vec.wk. <- weights_mat %*% pi.vec
  pi.vec.w.k <- t(weights_mat) %*% pi.vec
  pi.vec.w <- (pi.vec.wk. + pi.vec.w.k) / 2
  pe.ivec <- (agree.mat %*% pi.vec.w) / ri.vec
  kappa.ivec.x <- kappa.ivec - 2 * (1 - fleiss.kappa) * (pe.ivec - pe) / (1 - pe)

  if (n >= 2) {
    var.fleiss <- ((1 - f) / (n * (n - 1))) * sum((kappa.ivec.x - fleiss.kappa) ^ 2)
    stderr <- sqrt(var.fleiss) # standard error

    # Hypothesis testing
    t_stat <- calc_t_stat(x = fleiss.kappa, u = test_value, se = stderr)
    p_value <- calc_p_val(t = t_stat, df = n - 1, alternative = alternative)

    # Confidence intervals
    lcb <- fleiss.kappa - stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
    ucb <- fleiss.kappa + stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
  } else {
    stderr <- t_stat <- p_value <- lcb <- ucb <- NA
  }


  # Return results

  if (ucb > 1) {
    warning("Confidence intervals are clipped at the upper limit.")
  }

  res <- tibble::tibble(
    agreement_coefficient = "Scott's Pi / Fleiss' Kappa",
    pct_chance_agmt = pe,
    coefficient = fleiss.kappa,
    std_err = round(stderr, 5),
    t_stat = t_stat,
    p_value = p_value,
    lower_ci = lcb,
    upper_ci = min(1, ucb)
  )

  return(res)

}



#' @title
#' Scott’s unweighted and weighted Pi coefficients
#'
#' @description
#' Scott's pi coefficient (Scott(1955)) and its standard error for 2
#' raters when input dataset is a contingency table.
#'
#' @param table A q×q matrix (or contingency table) showing the distribution
#'   of subjects by rater, where q is the number of categories. This is
#'   the only argument you must specify if you want the unweighted analysis
#' @param weights One of the following to calculate weight based on defined
#'   methods: "unweighted", "quadratic", "linear", "ordinal", "radical",
#'   "ratio", "circular", "bipolar". The default is "unweighted", a diagonal
#'   matrix where all diagonal numbers equal to 1, and all off-diagonal numbers
#'   equal to 0. This special weight matrix leads to the unweighted analysis.
#'   You may specify your own q × q weight matrix here
#' @param conf_lev The confidence level associated with the agreement
#'   coefficient’s confidence interval. Default is 0.95.
#' @param N An optional parameter representing the total number of subjects in
#'   the target subject population. Its default value is infinity, which for all
#'   practical purposes assumes the target subject population to be very large
#'   and will not require any finite-population correction when computing the
#'   standard error.
#' @param test_value value to test the estimated AC against. Default is 0.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of "two.sided" (default), "greater" or "less".
#'
#' @importFrom tibble tibble
#' @importFrom stats pt
#' @importFrom stats qt
#'
#' @references
#' Scott (1955)
#'
#' 2014. Handbook of Inter-Rater Reliability: The Definitive Guide to Measuring
#' the Extent of Agreement Among Raters. 4th ed. Gaithersburg, MD: Advanced
#' Analytics.
#'
#' @return
#' A tbl_df with the coefficient, standard error, lower and upper confidence
#' limits.
#' @export
#'
#' @rdname fleiss_3_raw
#'
#' @examples
#' ratings <- matrix(c(5, 3, 0, 0,
#'                     3, 11, 4, 0,
#'                     2, 13, 3, 4,
#'                     1, 2, 4, 14), ncol = 4, byrow = TRUE)
#'
#' scott_2_table(table = ratings)
#'
#' scott_2_table(table = ratings,
#'               weights = "quadratic")
#'
#' scott_2_table(table = ratings,
#'               weights = ac_weights(categ = c(1:4),
#'                                    weight_type = "quadratic"))
#'
#' my_weights <- matrix(c(1.0000000, 0.8888889, 0.5555556, 0.0000000,
#'                        0.8888889, 1.0000000, 0.8888889, 0.5555556,
#'                        0.5555556, 0.8888889, 1.0000000, 0.8888889,
#'                        0.0000000, 0.5555556, 0.8888889, 1.0000000),
#'                      ncol = 4, byrow = TRUE)
#'
#' scott_2_table(table = ratings,
#'               weights = my_weights)
scott_2_table <- function(table,
                          weights = "unweighted",
                          conf_lev = 0.95,
                          N = Inf,
                          test_value = 0,
                          alternative = "two.sided") {

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


  # Create weights matrix
  wts_res <- get_ac_weights(weights = weights,
                            q = q)

  weights_name <- wts_res$w_name
  weights_mat <- wts_res$weights_mat

  # Calculate Percent Agreement and Scott's Pi
  pa <- sum(weights_mat * ratings / n) # percent agreement

  pk. <- (ratings %*% rep(1, q)) / n
  p.l <- t((t(rep(1, q)) %*% ratings) / n)
  pi.k <- (pk. + p.l) / 2
  pe <- sum(weights_mat * (pi.k %*% t(pi.k)))
  scott <- (pa - pe) / (1 - pe) # Scott's Pi


  # Variance and Standard Error
  # 2 raters special case variance

  pkl <- ratings / n	     #p_{kl}
  pb.k <- weights_mat %*% p.l    #\ov{p}_{+k}
  pbl. <- t(weights_mat) %*% pk. #\ov{p}_{l+}
  pbk  <- (pb.k + pbl.) / 2    #\ov{p}_{k}
  sum1 <- 0
  for (k in 1:q) {
    for (l in 1:q) {
      sum1 <- sum1 + pkl[k, l] * (weights_mat[k, l] - (1 - scott) * (pbk[k] + pbk[l])) ^ 2
    }}

  var.scott <- ((1 - f) / (n * (1 - pe) ^ 2)) * (sum1 - (pa - 2 * (1 - scott) * pe) ^ 2)
  stderr <- sqrt(var.scott)  # Scott's standard error


  # Hypothesis testing
  t_stat <- calc_t_stat(x = scott, u = test_value, se = stderr)
  p_value <- calc_p_val(t = t_stat, df = n - 1, alternative = alternative)

  # Confidence intervals
  lcb <- scott - stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
  ucb <- scott + stderr * qt(1 - (1 - conf_lev) / 2, n - 1)


  # Return results

  if (ucb > 1) {
    warning("Confidence intervals are clipped at the upper limit.")
  }

  res <- tibble::tibble(
    agreement_coefficient = "Scott's Pi / Fleiss' Kappa",
    pct_chance_agmt = pe,
    coefficient = scott,
    std_err = round(stderr, 5),
    t_stat = t_stat,
    p_value = p_value,
    lower_ci = lcb,
    upper_ci = min(1, ucb)
  )

  return(res)

}


#' @title
#' Fleiss' agreement coefficient among multiple raters (2, 3, +) when
#' the input dataset is the distribution of raters by subject and category.
#'
#' @description
#' This function computes Fleiss' generalized kappa coefficient (see
#' Fleiss(1971)) and its standard error for 3 raters or more when input dataset
#' is a nxq matrix representing the distribution of raters by subject and by
#' category.
#'
#' @param distribution An \emph{nxq} matrix / data frame containing the distribution
#'   of raters by subject and category. Each cell \emph{(i,k)} contains the
#' number of raters who classified subject \emph{i} into category \emph{k}. An n
#' x q agreement matrix representing the distribution of raters by subjects (n)
#' and category (q) (see `calc_agree_mat` to convert raw data to distribution).
#' @param weights is an optional parameter that is either a string variable or a
#'   matrix. The string describes one of the predefined weights and must take
#'   one of the values ("quadratic", "ordinal", "linear", "radical", "ratio",
#'   "circular", "bipolar"). If this parameter is a matrix then it must be a
#'   square matrix qxq where q is the number of possible categories where a
#'   subject can be classified. If some of the q possible categories are not
#'   used, then it is strongly advised to specify the complete list of possible
#'   categories as a vector in parameter `categ`. Otherwise, only the categories
#'   reported will be used.
#' @param categ An optional parameter representing all categories available to
#'   raters during the experiment. This parameter may be useful if some
#'   categories were not used by any rater in spite of being available to the
#'   raters.
#' @param conf_lev The confidence level associated with the agreement
#'   coefficient’s confidence interval. Default is 0.95.
#' @param N An optional parameter representing the total number of subjects in
#'   the target subject population. Its default value is infinity, which for all
#'   practical purposes assumes the target subject population to be very large
#'   and will not require any finite-population correction when computing the
#'   standard error.
#'
#' @references
#' 2014. Handbook of Inter-Rater Reliability: The Definitive Guide to Measuring
#' the Extent of Agreement Among Raters. 4th ed. Gaithersburg, MD: Advanced
#' Analytics.
#'
#' Fleiss, J. L. (1981). Statistical Methods for Rates and Proportions. John
#' Wiley & Sons.
#'
#' @return
#' A tbl_df with the coefficient, standard error, lower and upper confidence
#' limits.
#' @export
#'
#' @rdname fleiss_3_raw
#'
#' @examples
#' library(tidyverse)
#'
#' rvary2 <- tibble::tribble(
#'             ~subject, ~rater1, ~rater2, ~rater3, ~rater4, ~rater5,
#'                   1L,      1L,      2L,      2L,      NA,      2L,
#'                   2L,      1L,      1L,      3L,      3L,      3L,
#'                   3L,      3L,      3L,      3L,      3L,      3L,
#'                   4L,      1L,      1L,      1L,      1L,      3L,
#'                   5L,      1L,      1L,      1L,      3L,      3L,
#'                   6L,      1L,      2L,      2L,      2L,      2L,
#'                   7L,      1L,      1L,      1L,      1L,      1L,
#'                   8L,      2L,      2L,      2L,      2L,      3L,
#'                   9L,      1L,      3L,      NA,      NA,      3L,
#'                  10L,      1L,      1L,      1L,      3L,      3L
#'             )
#'
#' ex_dist <- calc_agree_mat(data = rvary2,
#'                           dplyr::starts_with("rater"),
#'                           subject_id = subject)
#'
#' ex_dist
#'
#' fleiss_3_dist(distribution = ex_dist)
fleiss_3_dist <- function(distribution,
                          weights = "unweighted",
                          categ = NULL,
                          conf_lev = 0.95,
                          N = Inf,
                          test_value = 0,
                          alternative = "two.sided") {

  # Check for valid alternative hypothesis
  if (!alternative %in% c("two.sided", "less", "greater")) {
    stop('alternative must be one of "two.sided", "less", "greater".')
  }


  agree.mat <- as.matrix(distribution)
  n <- nrow(agree.mat)  # number of subjects
  q <- ncol(agree.mat)  # number of categories
  f <- n / N  # finite population correction


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

  agree.mat.w <- t(weights_mat %*% t(agree.mat))


  # Calculate Percent Agreement and Fleiss' Kappa
  ri.vec <- agree.mat %*% rep(1, q)
  sum.q <- (agree.mat * (agree.mat.w - 1)) %*% rep(1, q)
  n2more <- sum(ri.vec >= 2)
  pa <- sum(sum.q[ri.vec >= 2] / ((ri.vec * (ri.vec - 1))[ri.vec >= 2])) / n2more

  pi.vec <- t(t(rep(1 / n, n)) %*% (agree.mat / (ri.vec %*% t(rep(1, q)))))
  pe <- sum(weights_mat * (pi.vec %*% t(pi.vec)))
  fleiss.kappa <- (pa - pe) / (1 - pe)


  # Variance and standard error
  den.ivec <- ri.vec * (ri.vec - 1)
  den.ivec <- den.ivec - (den.ivec == 0) # this operation replaces each 0 value with -1 to make the next ratio calculation always possible.
  pa.ivec <- sum.q / den.ivec

  pe.r2 <- pe * (ri.vec >= 2)
  kappa.ivec <- (n / n2more) * (pa.ivec - pe.r2) / (1 - pe)
  pi.vec.wk. <- weights_mat %*% pi.vec
  pi.vec.w.k <- t(weights_mat) %*% pi.vec
  pi.vec.w <- (pi.vec.wk. + pi.vec.w.k) / 2

  pe.ivec <- (agree.mat %*% pi.vec.w) / ri.vec
  kappa.ivec.x <- kappa.ivec - 2 * (1 - fleiss.kappa) * (pe.ivec - pe) / (1 - pe)

  var.fleiss <- ((1 - f) / (n * (n - 1))) * sum((kappa.ivec.x - fleiss.kappa) ^ 2)
  stderr <- sqrt(var.fleiss)


  # Hypothesis testing
  t_stat <- calc_t_stat(x = fleiss.kappa, u = test_value, se = stderr)
  p_value <- calc_p_val(t = t_stat, df = n - 1, alternative = alternative)

  # Confidence intervals
  lcb <- fleiss.kappa - stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
  ucb <- fleiss.kappa + stderr * qt(1 - (1 - conf_lev) / 2, n - 1)


  # Return results

  if (ucb > 1) {
    warning("Confidence intervals are clipped at the upper limit.")
  }

  res <- tibble::tibble(
    agreement_coefficient = "Fleiss' Kappa",
    pct_chance_agmt = pe,
    coefficient = fleiss.kappa,
    std_err = round(stderr, 5),
    t_stat = t_stat,
    p_value = p_value,
    lower_ci = lcb,
    upper_ci = min(1, ucb)
  )

  return(res)

}
