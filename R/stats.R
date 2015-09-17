
#' estBetaParams
#' Solve for the shape parameters of the beta distribution given its mean and
#' variance
#'
#' @param mu A numeric vector of length 1, [0,1]
#' @param var A numeric vector of length 1, [0,1]
#'
#' @return A list with 2 elements, alpha and beta
#' @export
#'
#' @examples
#' estBetaParams(mu = .5, var = .25)
estBetaParams <- function(mu, var) {
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta <- alpha * (1 / mu - 1)
  return(params = list(alpha = alpha, beta = beta))
}

#' meanBeta
#' Calculate the mean of a beta distribution given its two shape parameters
#'
#' @param alpha Beta distribution shape parameter #1 [0, Inf]
#' @param beta  Beta distribution shape parameter #2 [0, Inf]
#'
#' @return A numeric scalar
#' @export
#'
#' @examples
#' meanBeta(alpha=.01, beta = 1)
meanBeta <- function(alpha, beta){
  return(alpha/(alpha + beta))
}

#' varBeta
#' Calculate the variance of a beta distribution given its two shape parameters
#'
#' @param alpha Beta distribution shape parameter #1 [0, Inf]
#' @param beta  Beta distribution shape parameter #2 [0, Inf]
#'
#' @return A numeric scalar
#' @export
#'
#' @examples
#' varBeta(alpha=.01, beta = 1)
varBeta <- function(alpha, beta) {
  return(alpha*beta/((alpha+beta)^2)*(alpha + beta + 1))
}
