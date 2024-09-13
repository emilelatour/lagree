#' @name krippen_3_raw
#'
#' @title
#' Krippendorff's alpha coefficient among multiple raters (2, 3, +)
#'
#' @description
#' Krippendorff's alpha coefficient for an arbitrary number of raters (2, 3, +)
#' when the input data represent the raw ratings reported for each subject and
#' each rater.
#'
#' The algorithm used to compute krippendorff's alpha is very different from
#' anything that was published on this topic. Instead, it follows the equations
#' presented by K. Gwet (2012).
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
#'   coefficient's confidence interval. Default is 0.95.
#' @param N An optional parameter representing the total number of subjects in
#'   the target subject population. Its default value is infinity, which for all
#'   practical purposes assumes the target subject population to be very large
#'   and will not require any finite-population correction when computing the
#'   standard error.
#' @param test_value value to test the estimated AC against. Default is 0.
#' @param alternative a character string specifying the alternative hypothesis,
#'   must be one of "two.sided" (default), "greater" or "less".
#'
#'
#' @references
#' Gwet, K. (2014). Handbook of Inter-Rater Reliability: The Definitive Guide to
#' Measuring the Extent of Agreement Among Multiple Raters, 4th Edition.
#' Advanced Analytics, LLC;
#'
#' Krippendorff (1970). "Bivariate agreement coefficients for reliability of
#' data." Sociological Methodology,2,139-150
#'
#' Krippendorff (1980). Content analysis: An introduction to its methodology
#' (2nd ed.), New-bury Park, CA: Sage.
#'
#' @return
#' A tbl_df with the coefficient, standard error, lower and upper confidence
#' limits.
#' @export
#'
#' @rdname krippen_3_raw
#'
#' @examples
#' #  5 raters classify 10 subjects into 1 of 3 rating categories
#' rvary2
#'
#' # More than two raters
#' krippen_3_raw(data = rvary2,
#'               dplyr::starts_with("rater"))
#'
#' # Two raters
#' krippen_3_raw(data = rvary2,
#'               rater1:rater2)
#'
#' # Another example with two raters
#' # two radiologists who classify 85 xeromammograms into one of four categories
#' # (Altman p. 403)
#' radiologist
#'
#' krippen_3_raw(data = radiologist,
#'               radiologist_a, radiologist_b)
krippen_3_raw <- function(data,
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
  n0 <- nrow(ratings.mat) # number of subjects
  r <- ncol(ratings.mat) # number of raters
  f <- n0 / N # finite population correction


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
                      nrow = n0,
                      ncol = q)

  for (k in 1:q) {
    categ.is.k <- (ratings.mat == categ[k])
    agree.mat[, k] <- (replace(categ.is.k, is.na(categ.is.k), FALSE)) %*% rep(1, r)

  }

  agree.mat.w <- t(weights_mat%*%t(agree.mat))


  # Calculate agreement coefficient
  ri.vec <- agree.mat %*% rep(1, q)
  agree.mat <- agree.mat[(ri.vec >= 2), ]
  agree.mat.w <- agree.mat.w[(ri.vec >= 2), ]
  ri.vec <- ri.vec[(ri.vec >= 2)]
  ri.mean <- mean(ri.vec)
  n <- nrow(as.matrix(agree.mat))
  epsi <- 1 / sum(ri.vec)
  sum.q <- (agree.mat * (agree.mat.w - 1)) %*% as.matrix(rep(1, q))
  paprime <- sum(sum.q / (ri.mean * (ri.vec - 1))) / n
  pa <- (1 - epsi) * paprime + epsi
  pi.vec <- t(t(rep(1 / n, n)) %*% (agree.mat / ri.mean))

  if (q >= 2) {
    pe <- sum(weights_mat * (pi.vec %*% t(pi.vec)))
  } else {
    pe = 1e-15
  }

  krippen.alpha <- (pa - pe) / (1 - pe)
  krippen.alpha.prime <- (paprime - pe) / (1 - pe)


  # Variance and Standard Error
  if (q >= 2) {
    pa.ivec <- sum.q / (ri.mean * (ri.vec - 1)) - paprime * (ri.vec - ri.mean) / ri.mean
    krippen.ivec <- (pa.ivec - pe) / (1 - pe)
    pi.vec.wk. <- weights_mat %*% pi.vec
    pi.vec.w.k <- t(weights_mat) %*% pi.vec
    pi.vec.w <- (pi.vec.wk. + pi.vec.w.k) / 2
    pe.ivec <- (agree.mat %*% pi.vec.w) / ri.mean - pe * (ri.vec - ri.mean) / ri.mean
    krippen.ivec.x <- krippen.ivec - 2 * (1 - krippen.alpha.prime) * (pe.ivec - pe) / (1 - pe)

    var.krippen <- stderr <- t_stat <- p.value <- lcb <- ucb <- NA

    if (n >= 2) {
      var.krippen <- ((1 - f) / (n * (n - 1))) * sum((krippen.ivec.x - krippen.alpha.prime) ^ 2)
      stderr <- sqrt(var.krippen)

      # Hypothesis testing
      t_stat <- calc_t_stat(x = krippen.alpha, u = test_value, se = stderr)
      p_value <- calc_p_val(t = t_stat, df = n - 1, alternative = alternative)

      # Confidence intervals
      lcb <- krippen.alpha - stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
      ucb <- krippen.alpha + stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
    }
  }


  # Return results

  if (ucb > 1) {
    warning("Confidence intervals are clipped at the upper limit.")
  }

  res <- tibble::tibble(
    agreement_coefficient = "Krippendorff's Alpha",
    pct_chance_agmt = pe,
    coefficient = krippen.alpha,
    std_err = round(stderr, 5),
    t_stat = t_stat,
    p_value = p_value,
    lower_ci = lcb,
    upper_ci = min(1, ucb)
  )

  return(res)

}



#' @title
#' Krippendorff's Alpha
#'
#' @description
#' Krippendorff's Alpha and its standard error for 2 raters when input dataset
#' is a contingency table
#'
#' @param table A qxq matrix (or contingency table) showing the distribution
#'   of subjects by rater, where q is the number of categories. This is
#'   the only argument you must specify if you want the unweighted analysis
#' @param weights One of the following to calculate weight based on defined
#'   methods: "unweighted", "quadratic", "linear", "ordinal", "radical",
#'   "ratio", "circular", "bipolar". The default is "unweighted", a diagonal
#'   matrix where all diagonal numbers equal to 1, and all off-diagonal numbers
#'   equal to 0. This special weight matrix leads to the unweighted analysis.
#'   You may specify your own q x q weight matrix here
#' @param conf_lev The confidence level associated with the agreement
#'   coefficient's confidence interval. Default is 0.95.
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
#' Gwet, K. (2014). Handbook of Inter-Rater Reliability: The Definitive Guide to
#' Measuring the Extent of Agreement Among Multiple Raters, 4th Edition.
#' Advanced Analytics, LLC;
#'
#' Krippendorff (1970). "Bivariate agreement coefficients for reliability of
#' data." Sociological Methodology,2,139-150
#'
#' Krippendorff (1980). Content analysis: An introduction to its methodology
#' (2nd ed.), New-bury Park, CA: Sage.
#'
#' @return
#' A tbl_df with the coefficient, standard error, lower and upper confidence
#' limits.
#' @export
#'
#' @rdname krippen_2_raw
#'
#' @examples
#' ratings <- matrix(c(5, 3, 0, 0,
#'                     3, 11, 4, 0,
#'                     2, 13, 3, 4,
#'                     1, 2, 4, 14), ncol = 4, byrow = TRUE)
#'
#' krippen_2_table(table = ratings)
#'
#' krippen_2_table(table = ratings,
#'                weights = "quadratic")
#'
#' krippen_2_table(table = ratings,
#'                weights = ac_weights(categ = c(1:4),
#'                                     weight_type = "quadratic"))
#'
#' my_weights <- matrix(c(1.0000000, 0.8888889, 0.5555556, 0.0000000,
#'                        0.8888889, 1.0000000, 0.8888889, 0.5555556,
#'                        0.5555556, 0.8888889, 1.0000000, 0.8888889,
#'                        0.0000000, 0.5555556, 0.8888889, 1.0000000),
#'                      ncol = 4, byrow = TRUE)
#'
#' krippen_2_table(table = ratings,
#'                weights = my_weights)
krippen_2_table <- function(table,
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

  # Calculate agreement coefficient
  epsi = 1 / (2 * n)
  pa0 <- sum(weights_mat * ratings / n)
  pa <- (1 - epsi) * pa0 + epsi # percent agreement

  pk. <- (ratings %*% rep(1, q)) / n
  p.l <- t((t(rep(1, q)) %*% ratings) / n)
  pi.k <- (pk. + p.l) / 2
  pe <- sum(weights_mat * (pi.k %*% t(pi.k)))
  kripp.coeff <- (pa - pe) / (1 - pe) # weighted Krippen's alpha coefficient


  # Variance and Standard Error
  pkl <- ratings / n	     #p_{kl}
  pb.k <- weights_mat %*% p.l    #\ov{p}_{+k}
  pbl. <- t(weights_mat) %*% pk. #\ov{p}_{l+}
  pbk  <- (pb.k + pbl.) / 2    #\ov{p}_{k}
  kcoeff <- (pa0 - pe) / (1 - pe)
  sum1 <- 0

  for (k in 1:q) {

    for (l in 1:q) {
      sum1 <-
        sum1 + pkl[k, l] * (weights_mat[k, l] - (1 - kcoeff) * (pbk[k] + pbk[l])) ^ 2
    }
  }

  var.kripp <- ((1 - f) / (n * (1 - pe) ^ 2)) * (sum1 - (pa0 - 2 * (1 - kcoeff) * pe) ^ 2)
  stderr <- sqrt(var.kripp)  # Kripp. alpha's standard error

  # Hypothesis testing
  t_stat <- calc_t_stat(x = kripp.coeff, u = test_value, se = stderr)
  p_value <- calc_p_val(t = t_stat, df = n - 1, alternative = alternative)

  # Confidence intervals
  lcb <- kripp.coeff - stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
  ucb <- kripp.coeff + stderr * qt(1 - (1 - conf_lev) / 2, n - 1)


  # Return results

  if (ucb > 1) {
    warning("Confidence intervals are clipped at the upper limit.")
  }

  res <- tibble::tibble(
    agreement_coefficient = "Krippendorff's Alpha",
    pct_chance_agmt = pe,
    coefficient = kripp.coeff,
    std_err = round(stderr, 5),
    t_stat = t_stat,
    p_value = p_value,
    lower_ci = lcb,
    upper_ci = min(1, ucb)
  )

  return(res)

}


#' @title
#' Krippendorff's agreement coefficient among multiple raters (2, 3, +) when
#' the input dataset is the distribution of raters by subject and category.
#'
#' @description
#' This function computes Krippendorff's alpha coefficient (see
#' Krippendorff(1970, 1980)) and its standard error for 3 raters or more when
#' input dataset is a nxq matrix representing the distribution of raters by
#' subject and by category. The input data "ratings" is an nxq matrix showing
#' the number of raters by subject and category. A typical entry associated with
#' a subject and a category, represents the number of raters who classified the
#' subject into the specified category. Exclude all subjects that are not rated
#' by any rater. The algorithm used to compute krippendorff's alpha is very
#' different from anything that was published on this topic. Instead, it follows
#' the equations presented by K. Gwet (2010)
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
#'   coefficient's confidence interval. Default is 0.95.
#' @param N An optional parameter representing the total number of subjects in
#'   the target subject population. Its default value is infinity, which for all
#'   practical purposes assumes the target subject population to be very large
#'   and will not require any finite-population correction when computing the
#'   standard error.
#'
#' @references
#' Gwet, K. (2012). Handbook of Inter-Rater Reliability: The Definitive Guide to
#' Measuring the Extent of Agreement Among Multiple Raters, 3rd Edition.
#' Advanced Analytics, LLC; 3rd edition (March 2, 2012)
#'
#' Krippendorff (1970). "Bivariate agreement coefficients for reliability of data." Sociological
#' Methodology,2,139-150
#'
#' Krippendorff (1980). Content analysis: An introduction to its methodology (2nd ed.), New-bury Park, CA: Sage.
#'
#' @return
#' A tbl_df with the coefficient, standard error, lower and upper confidence
#' limits.
#' @export
#'
#' @rdname krippen_3_raw
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
#' krippen_3_dist(distribution = ex_dist)
krippen_3_dist <- function(distribution,
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


  # Calculate the agreement coefficient
  ri.vec <- agree.mat %*% rep(1, q)
  agree.mat <- agree.mat[(ri.vec >= 2), ]
  agree.mat.w <- agree.mat.w[(ri.vec >= 2), ]
  ri.vec <- ri.vec[(ri.vec >= 2)]
  ri.mean <- mean(ri.vec)
  n <- nrow(agree.mat)
  epsi <- 1 / sum(ri.vec)
  sum.q <- (agree.mat * (agree.mat.w - 1)) %*% rep(1, q)
  pa <- (1 - epsi) * sum(sum.q / (ri.mean * (ri.vec - 1))) / n + epsi

  pi.vec <- t(t(rep(1 / n, n)) %*% (agree.mat / ri.mean))
  pe <- sum(weights_mat * (pi.vec %*% t(pi.vec)))
  krippen.alpha <- (pa - pe) / (1 - pe)


  # Variance and standard error
  den.ivec <- ri.mean * (ri.vec - 1)
  pa.ivec <- sum.q / den.ivec
  pa.v <- mean(pa.ivec)
  pa.ivec <- (1 - epsi) * (pa.ivec - pa.v * (ri.vec - ri.mean) / ri.mean) + epsi

  krippen.ivec <- (pa.ivec - pe) / (1 - pe)
  pi.vec.wk. <- weights_mat %*% pi.vec
  pi.vec.w.k <- t(weights_mat) %*% pi.vec

  pi.vec.w <- (pi.vec.wk. + pi.vec.w.k) / 2

  pe.ivec <- (agree.mat %*% pi.vec.w) / ri.mean - sum(pi.vec) * (ri.vec - ri.mean) / ri.mean
  krippen.ivec.x <- krippen.ivec - (1 - krippen.alpha) * (pe.ivec - pe) / (1 - pe)

  var.krippen <- ((1 - f) / (n * (n - 1))) * sum((krippen.ivec.x - krippen.alpha) ^ 2)
  stderr <- sqrt(var.krippen) # alpha's standard error


  # Hypothesis testing
  t_stat <- calc_t_stat(x = krippen.alpha, u = test_value, se = stderr)
  p_value <- calc_p_val(t = t_stat, df = n - 1, alternative = alternative)

  # Confidence intervals
  lcb <- krippen.alpha - stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
  ucb <- krippen.alpha + stderr * qt(1 - (1 - conf_lev) / 2, n - 1)



  # Return results

  if (ucb > 1) {
    warning("Confidence intervals are clipped at the upper limit.")
  }

  res <- tibble::tibble(
    agreement_coefficient = "Krippendorff's Alpha",
    pct_chance_agmt = pe,
    coefficient = krippen.alpha,
    std_err = round(stderr, 5),
    t_stat = t_stat,
    p_value = p_value,
    lower_ci = lcb,
    upper_ci = min(1, ucb)
  )

  return(res)

}
