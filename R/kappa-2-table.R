#' @title 
#' Cohen’s unweighted and weighted kappa coefficients
#' 
#' @description 
#' Cohen's kappa (Cohen(1960)) coefficient and its standard error for 2 raters
#' when input dataset is a contingency table
#' 
#' @param ratings A q×q matrix (or contingency table) showing the distribution
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
#'
#' @references 
#' Cohen (1960)
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
# ratings <- matrix(c(5, 3, 0, 0,
#                     3, 11, 4, 0,
#                     2, 13, 3, 4,
#                     1, 2, 4, 14), ncol = 4, byrow = TRUE)
# 
# kappa_2_table(ratings = ratings)
# 
# kappa_2_table(ratings = ratings,
#               weights = "quadratic")
# 
# kappa_2_table(ratings = ratings,
#               weights = ac_weights(categ = c(1:4),
#                                    weight_type = "quadratic"))
# 
# my_weights <- matrix(c(1.0000000, 0.8888889, 0.5555556, 0.0000000,
#                        0.8888889, 1.0000000, 0.8888889, 0.5555556,
#                        0.5555556, 0.8888889, 1.0000000, 0.8888889,
#                        0.0000000, 0.5555556, 0.8888889, 1.0000000),
#                      ncol = 4, byrow = TRUE)
# 
# kappa_2_table(ratings = ratings,
#               weights = my_weights)

kappa_2_table <- function(ratings, 
                          weights = "unweighted", 
                          conf_lev = 0.95, 
                          N = Inf) {
  
  if(dim(ratings)[1] != dim(ratings)[2]){
    stop('The contingency table should have the same number of rows and columns!') 
  }
  
  ## Calc number of subjects and categgories ---------------- 
  
  n <- sum(ratings) # number of subjects
  q <- ncol(ratings) # number of categories
  
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
  
  pa <- sum(weights_mat * ratings / n) # percent agreement
  
  pk. <- (ratings %*% rep(1, q)) / n
  p.l <- t((t(rep(1, q)) %*% ratings) / n)
  pe <- sum(weights_mat * (pk. %*% t(p.l)))
  kappa <- (pa - pe) / (1 - pe) # weighted kappa
  
  ## 2 raters special case variance ---------------- 
  
  pkl <- ratings / n
  pb.k <- weights_mat %*% p.l
  pbl. <- t(weights_mat) %*% pk.
  sum1 <- 0
  for (k in 1:q) {
    for (l in 1:q) {
      sum1 <-
        sum1 + pkl[k, l] * (weights_mat[k, l] - (1 - kappa) * (pb.k[k] + pbl.[l])) ^
        2
    }
  }
  
  
  ## Variance and standard error ---------------- 
  
  var.kappa <-
    ((1 - f) / (n * (1 - pe) ^ 2)) * (sum1 - (pa - 2 * (1 - kappa) * pe) ^ 2)
  stderr <- sqrt(var.kappa)
  
  # p-value
  p.value <- 2 * (1 - pt(abs(kappa / stderr), n - 1))
  
  # Lower and upper confidence bounds
  lcb <- kappa - stderr * qt(1 - (1 - conf_lev) / 2, n - 1)
  ucb <- min(1, kappa + stderr * qt(1 - (1 - conf_lev) / 2, n - 1))
  
  
  ## Return results as tbl_df ---------------- 
  
  tibble::tibble(
    agreement_coefficient = "Cohen's kappa", 
    pct_agmt = pa, 
    pct_chance_agmt = pe, 
    coefficient = kappa, 
    std_err = stderr, 
    t_stat = qt(1 - (1 - conf_lev) / 2, n - 1), 
    p_value = p.value, 
    lower_ci = lcb, 
    upper_ci = ucb
    
  )
  
  
  
}


