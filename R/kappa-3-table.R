
#' @title 
#' Fleiss' generalized kappa coefficient
#' 
#' @description 
#' Fleiss' generalized kappa coefficient (see Fleiss(1971)) and its standard
#' error for 3 raters or more when input dataset is a n×q matrix representing
#' the distribution of raters by subject and by category.
#' 
#' Ratings are represented as a table where each row represents one subject,
#' each column represents one category, and each table cell represents the
#' number of raters who classified the specified subject into the specified
#' category.
#' 
#' A table cell may have a 0 value if none of the raters classified the subject
#' into the category associated with that cell. The number of raters may vary by
#' subject leading to a table with different row totals. That will be the case
#' when the experiment generated missing ratings, with subjects being rated by a
#' different number of raters.
#' 
#' Exclude all subjects that are not rated by any rater.
#' 
#' @param ratings This is an n×q matrix (or contingency table), where n is
#'   the number of subjects, and q the number of categories. This is the only
#'   argument that must be specified if you want an unweighted analysis.#' 
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
#'
#' @references 
#' Fleiss, J. L. (1981). Statistical Methods for Rates and Proportions. John
#' Wiley & Sons.
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
#' @examples
#' #### Example 1 -------------------------------- 
#' # Distribution of 6 Raters by Subject and Category (Extract of Table 1 of
#' # Fleiss, 1971)
#' ratings <- matrix(c(0, 0, 0, 6, 0,
#'                     0, 1, 4, 0, 1,
#'                     2, 0, 4, 0, 0,
#'                     0, 3, 3, 0, 0),
#'                   ncol = 5,
#'                   byrow = T)
#' 
#' kappa_3_table(ratings = ratings, 
#'               weights = "unweighted")
#' 
#' kappa_3_table(ratings = ratings, 
#'               weights = "quadratic")
#' 
#' #### Example 2 -------------------------------- 
#' # Rating of Four Subjects by Five Raters (Table 2 of Finn, 1970)
#' finn_1970_table <- tibble::tribble(
#'   ~subject, ~rater_I, ~rater_II, ~rater_III, ~rater_IV, ~rater_V,
#'   "A",        2,         2,          3,         2,        2,
#'   "B",        2,         2,          2,         2,        2,
#'   "C",        2,         2,          2,         2,        1,
#'   "D",        1,         2,          2,         2,        2
#' ) %>% 
#'   mutate_at(.vars = vars(dplyr::starts_with("rater")), 
#'             .funs = list(~ factor(., 
#'                                   levels = c(1, 2, 3))))
#' 
#' calc_ac_ratings(data = finn_1970_table, 
#'                 dplyr::starts_with("rater"),
#'                 subject_id = "subject") %>% 
#'   kappa_3_table(ratings = .)
#' 
#' calc_ac_ratings(data = finn_1970_table, 
#'                 dplyr::starts_with("rater"),
#'                 subject_id = "subject") %>% 
#'   kappa_3_table(ratings = ., 
#'                 weights = "quadratic")




kappa_3_table <- function(ratings,
                          weights = "unweighted",
                          conf_lev = 0.95,
                          N = Inf) {
  ## Calc number of subjects and categories ----------------
  
  agree_mat <- as.matrix(ratings)
  
  n <- nrow(agree_mat) # number of subjects
  q <- ncol(agree_mat) # number of categories
  
  f <- n / N # final population correction
  
  
  ## Create the weights matrix ----------------
  
  if (any(weights %in% c("unweighted",
                         "quadratic",
                         "linear",
                         "ordinal",
                         "radical",
                         "ratio",
                         "circular",
                         "bipolar"))) {
    weights_mat <- ac_weights(categ = c(1:q),
                              weight_type = weights)
  } else {
    weights_mat <- weights
  }
  
  if (dim(weights_mat)[1] != dim(weights_mat)[2]) {
    stop('The weights provided should have the same number of rows and columns!')
  }
  
  if (dim(weights_mat)[1] != q) {
    stop('The weights table is not the same dimension as the number of categories')
  }
  
  
  ## Calc stats ----------------
  
  agree_mat_w <- t(weights_mat %*% t(agree_mat))
  
  # calculating fleiss's generalized kappa coefficient
  ri.vec <- agree_mat %*% rep(1, q)
  sum.q <- (agree_mat * (agree_mat_w - 1)) %*% rep(1, q)
  n2more <- sum(ri.vec >= 2)
  pa <-
    sum(sum.q[ri.vec >= 2] / ((ri.vec * (ri.vec - 1))[ri.vec >= 2])) / n2more
  
  pi.vec <- t(t(rep(1 / n, n)) %*% (agree_mat / (ri.vec %*% t(rep(1, q)))))
  pe <- sum(weights_mat * (pi.vec %*% t(pi.vec)))
  fleiss.kappa <- (pa - pe) / (1 - pe)
  
  ## Variance and standard error ----------------
  
  den.ivec <- ri.vec * (ri.vec - 1)
  # this operation replaces each 0 value with -1 to make the next ratio
  # calculation always possible.
  den.ivec <- den.ivec - (den.ivec == 0) 
  pa.ivec <- sum.q / den.ivec
  
  pe.r2 <- pe * (ri.vec >= 2)
  kappa.ivec <- (n / n2more) * (pa.ivec - pe.r2) / (1 - pe)
  pi.vec.wk. <- weights_mat %*% pi.vec
  pi.vec.w.k <- t(weights_mat) %*% pi.vec
  pi.vec.w <- (pi.vec.wk. + pi.vec.w.k) / 2
  
  pe.ivec <- (agree_mat %*% pi.vec.w) / ri.vec
  kappa.ivec.x <-
    kappa.ivec - 2 * (1 - fleiss.kappa) * (pe.ivec - pe) / (1 - pe)
  
  var.fleiss <- 
    ((1 - f) / (n * (n - 1))) * sum((kappa.ivec.x - fleiss.kappa) ^ 2)
  stderr <- sqrt(var.fleiss)
  
  # p-value
  p.value <- 2 * (1 - pt(fleiss.kappa / stderr, n - 1))
  
  # Lower and upper confidence bounds
  lcb <- fleiss.kappa - stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
  ucb <- min(1, fleiss.kappa + stderr * qt(1 - (1 - conf_lev) / 2, n - 1))
  
  
  ## Return results as tbl_df ----------------
  
  tibble::tibble(
    agreement_coefficient = "Fleiss' Kappa",
    pct_agmt = pa,
    pct_chance_agmt = pe,
    coefficient = fleiss.kappa,
    std_err = stderr,
    t_stat = qt(1 - (1 - conf_lev) / 2, n - 1),
    p_value = p.value,
    lower_ci = lcb,
    upper_ci = ucb)
  
}
