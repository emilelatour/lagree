
#' Generate Hypothesis Test Text
#'
#' This function generates a descriptive string for a hypothesis test based on the provided test value and the specified alternative hypothesis type.
#'
#' @param u The hypothesized value for the coefficient in the null hypothesis.
#' @param alternative The alternative hypothesis type. Must be one of "two.sided", "less", or "greater". Default is "two.sided".
#'
#' @return A string describing the hypothesis test.
#'
#' @importFrom glue glue
calc_test_txt <- function(u, alternative = "two.sided") {

  if (alternative == "two.sided") {

    glue::glue("t test Ho: Coef. == {u}  Ha: Coef. != {u}")

  } else if (alternative == "less") {

    glue::glue("t test Ho: Coef. >= {u}  Ha: Coef. < {u}")

  } else if (alternative == "greater") {

    glue::glue("t test Ho: Coef. <= {u}  Ha: Coef. > {u}")

  }

}

#' Calculate T-statistic
#'
#' Computes the T-statistic for a given estimate, population value, and standard error.
#'
#' @param x Numeric. The observed value (estimate).
#' @param u Numeric. The hypothesized population value (null value).
#' @param se Numeric. The standard error of the observed value.
#'
#' @return Numeric. The T-statistic computed as \eqn{(x - u) / se}.
#'
calc_t_stat <- function(x, u, se) {

  (x - u) / se
}

#' Calculate P-value from T-statistic
#'
#' Computes the p-value based on a given T-statistic, degrees of freedom, and hypothesis direction.
#'
#' @param t Numeric. The T-statistic value.
#' @param df Numeric. Degrees of freedom for the t-distribution.
#' @param alternative Character. The alternative hypothesis. One of `"two.sided"`, `"less"`, or `"greater"`. Defaults to `"two.sided"`.
#'
#' @return Numeric. The p-value for the specified hypothesis test.
#'
calc_p_val <- function(t, df, alternative = "two.sided") {

  if (alternative == "two.sided") {

    2 * (1 - pt(t, df))

  } else if (alternative == "less") {

    pt(t, df)

  } else if (alternative == "greater") {

    1 - pt(t, df)

  }

}


