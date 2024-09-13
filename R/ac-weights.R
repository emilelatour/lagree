#' @title
#' Get Agreement Coefficient Weights
#'
#' @description
#' This function retrieves or creates a weights matrix for calculating agreement
#' coefficients based on a specified weighting scheme. Basically, a wrapper
#' around `ac_weights`.
#'
#' @param weights A character string specifying the type of weight to use. One
#'   of "unweighted", "quadratic", "linear", "ordinal", "radical", "ratio",
#'   "circular", "bipolar", or a custom matrix. Default is "unweighted".
#' @param q The number of categories in the agreement matrix.
#'
#' @return A list containing: \item{w_name}{The name of the weight type or
#'   "Custom Weights".} \item{weights_mat}{The corresponding weights matrix.}
#'
#' @export
#'
#' @examples
#' get_ac_weights(weights = "quadratic", q = 4)
#' get_ac_weights(weights = matrix(c(1, 0.5, 0, 0,
#'                                   0.5, 1, 0.5, 0,
#'                                   0, 0.5, 1, 0.5,
#'                                   0, 0, 0.5, 1), ncol = 4), q = 4)

get_ac_weights <- function(weights = "unweighted",
                           q) {


  # q <- ncol(agree.mat) # number of categories

  if (any(weights %in% c("unweighted",
                         "quadratic",
                         "linear",
                         "ordinal",
                         "radical",
                         "ratio",
                         "circular",
                         "bipolar"))) {

    w_name <- weights
    weights_mat <- ac_weights(categ = c(1:q),
                              weight_type = weights)

  } else {

    w_name <- "Custom Weights"
    weights_mat = as.matrix(weights)

  }

  if (dim(weights_mat)[1] != dim(weights_mat)[2]) {
    stop('The weights provided should have the same number of rows and columns.')
  }

  if (dim(weights_mat)[1] != q) {
    stop('The weights table is not the same dimension as the number of categories.')
  }


  res_wts <- list(w_name = w_name,
                  weights_mat = weights_mat)


  return(res_wts)


}




#' @name ac_weights
#'
#' @title
#' Weight-generating functions
#'
#' @description
#' To do the weighted analysis, you may create your own weight matrix, or use
#' one of the many existing weight-generating functions in the weights.ge.r
#' script. Each weight function takes single mandatory parameter, which is a
#' vector containing all categories used in the study. The weight functions
#' always sort all numeric-type category vectors in ascending order.
#' Consequently, the weighted coefficients are computed properly only if columns
#' and rows in the input dataset are ordered the same way. For alphanumeric-type
#' category vectors, they are assumed to already be ranked following an order
#' that is meaningful to the researcher.
#'
#' @param categ A vector containing all categories used. Be careful that the
#' order matches the the columns and rows of the input data set.
#' @param weight_type A character vector only available for the `ac_weights`
#'   function. Argument for the generic function to select type of weighting.
#'
#' @return
#' A q × q matrix of weights, where q is the number of categories. The
#' default argument is "unweighted". With this option, the function will create
#' a diagonal weight matrix with all diagonal numbers equal to 1, and all
#' off-diagonal numbers equal to 0. This special weight matrix leads to the
#' unweighted analysis.
#'
#' @references
#' 2014. Handbook of Inter-Rater Reliability: The Definitive Guide to Measuring
#' the Extent of Agreement Among Raters. 4th ed. Gaithersburg, MD: Advanced
#' Analytics.
#'
#' @rdname ac_weights
#' @examples
#' ac_weights(c(1:5), "unweighted")
#' ac_weights(c(1:5), "quadratic")
#' ac_weights(c("A", "B", "C", "D", "E"), "ordinal")
#' ac_weights(c("A", "B", "C", "D", "E"), "radical")
#' ac_weights(c("A", "B", "C", "D", "E"), "ratio")
#' ac_weights(letters[c(1:5)], "circular")
#' ac_weights(letters[c(1:5)], "bipolar")
#' @export
ac_weights <- function(categ, weight_type = "unweighted") {

  if (!weight_type %in% c("unweighted", "quadratic", "linear", "ordinal", "radical", "ratio", "circular", "bipolar")) {
    stop('weight_type must be one of the following: "unweighted", "quadratic", "linear", "ordinal", "radical", "ratio", "circular", "bipolar"')
  }

  if (weight_type == "unweighted") {
    identity_weights(categ)
  } else if (weight_type == "quadratic") {
    quadratic_weights(categ)
  } else if (weight_type == "linear") {
    linear_weights(categ)
  } else if (weight_type == "ordinal") {
    ordinal_weights(categ)
  } else if (weight_type == "radical") {
    radical_weights(categ)
  } else if (weight_type == "ratio") {
    ratio_weights(categ)
  } else if (weight_type == "circular") {
    circular_weights(categ)
  } else if (weight_type == "bipolar") {
    bipolar_weights(categ)
  }

}

#' @rdname ac_weights
#' @examples
#' identity_weights(1:5)
#' @export
identity_weights <- function(categ) {

	weights <- diag(length(categ))
	return(weights)

}


#' @rdname ac_weights
#' @examples
#' quadratic_weights(1:5)
#' @export
quadratic_weights <- function(categ) {

  q <- length(categ)
  weights <- diag(q)

  if (is.numeric(categ)) {
    categ_vec <- sort(categ)
  } else {
    categ_vec <- 1:length(categ)
  }

  xmin <- min(categ_vec)
  xmax <- max(categ_vec)

  for (k in 1:q) {
    for (l in 1:q) {
      weights[k, l] <- 1 - (categ_vec[k] - categ_vec[l]) ^ 2 / (xmax - xmin) ^
        2
    }
  }

  return(weights)

}


#' @rdname ac_weights
#' @examples
#' linear_weights(1:5)
#' @export
linear_weights <- function(categ) {

  q <- length(categ)
  weights <- diag(q)

  if (is.numeric(categ)) {
    categ_vec <- sort(categ)
  } else {
    categ_vec <- 1:length(categ)
  }

  xmin <- min(categ_vec)
  xmax <- max(categ_vec)

  for (k in 1:q) {
    for (l in 1:q) {
      weights[k, l] <- 1 - abs(categ_vec[k] - categ_vec[l]) / abs(xmax - xmin)
    }
  }

  return (weights)

}


#' @rdname ac_weights
#' @examples
#' ordinal_weights(1:5)
#' @export
ordinal_weights <- function(categ) {

  q <- length(categ)
  weights <- diag(q)
  categ_vec <- 1:length(categ)

  for (k in 1:q) {
    for (l in 1:q) {
      nkl <- max(k, l) - min(k, l) + 1
      weights[k, l] <- nkl * (nkl - 1) / 2
    }
  }

  weights <- 1 - weights / max(weights)

  return (weights)

}


#' @rdname ac_weights
#' @examples
#' radical_weights(1:5)
#' @export
radical_weights <- function(categ) {

  q <- length(categ)
  weights <- diag(q)

  if (is.numeric(categ)) {
    categ_vec <- sort(categ)
  }
  else {
    categ_vec <- 1:length(categ)
  }

  xmin <- min(categ_vec)
  xmax <- max(categ_vec)

  for (k in 1:q) {
    for (l in 1:q) {
      weights[k, l] <-
        1 - sqrt(abs(categ_vec[k] - categ_vec[l])) / sqrt(abs(xmax - xmin))
    }
  }

  return(weights)

}


#' @rdname ac_weights
#' @examples
#' ratio_weights(1:5)
#' @export
ratio_weights <- function(categ) {

  q <- length(categ)
  weights <- diag(q)

  if (is.numeric(categ)) {
    categ_vec <- sort(categ)
  }
  else {
    categ_vec <- 1:length(categ)
  }

  xmin <- min(categ_vec)
  xmax <- max(categ_vec)

  for (k in 1:q) {
    for (l in 1:q) {
      weights[k, l] <-
        1 - ((categ_vec[k] - categ_vec[l]) / (categ_vec[k] + categ_vec[l])) ^ 2 / ((xmax -
                                                                                      xmin) / (xmax + xmin)) ^ 2
    }
  }

  return(weights)

}

#' @rdname ac_weights
#' @examples
#' circular_weights(1:5)
#' @export
circular_weights <- function(categ) {

  q <- length(categ)
  weights <- diag(q)

  if (is.numeric(categ)) {
    categ_vec <- sort(categ)
  }
  else {
    categ_vec <- 1:length(categ)
  }

  xmin <- min(categ_vec)
  xmax <- max(categ_vec)

  U = xmax - xmin + 1

  for (k in 1:q) {
    for (l in 1:q) {
      weights[k, l] <- (sin(pi * (categ_vec[k] - categ_vec[l]) / U)) ^ 2
    }
  }

  weights <- 1 - weights / max(weights)

  return(weights)

}

#' @rdname ac_weights
#' @examples
#' bipolar_weights(1:5)
#' @export
bipolar_weights <- function(categ) {

  q <- length(categ)
  weights <- diag(q)

  if (is.numeric(categ)) {
    categ_vec <- sort(categ)
  }
  else {
    categ_vec <- 1:length(categ)
  }

  xmin <- min(categ_vec)
  xmax <- max(categ_vec)

  for (k in 1:q) {
    for (l in 1:q) {
      if (k != l)
        weights[k, l] <-
          (categ_vec[k] - categ_vec[l]) ^ 2 / (((categ_vec[k] + categ_vec[l]) - 2 *
                                                  xmin) * (2 * xmax - (categ_vec[k] + categ_vec[l])))
      else
        weights[k, l] <- 0
    }
  }

  weights <- 1 - weights / max(weights)

  return(weights)

}

