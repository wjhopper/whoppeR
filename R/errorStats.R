
#' binomial g2
#'
#' Likelihood ratio statistic for binomial random variable
#' @param obs Observed Proportions
#' @param pred Predicted Proportions
#' @param N Cell Frequencies
#'
#' @return Scalar numeric vector
#' @export
#'
binomialg2 <- function(obs,pred,N) {
  Lc <- obs*(log(pred)) + ((1-obs)*log(1-pred))
  Lu <- obs*(log(obs)) + ((1-obs)*log(1-obs))
  err <- -sum(2*N*(Lc-Lu))
  return(err)
}


#' Multinomial g2
#'
#' Likelihood ratio statistic for multinomial random variable
#' @inheritParams binomialg2
#'
#' @return Scalar numeric vector
#' @export
multinomialg2 <- function(obs,pred,N) {
  Lc <- obs*(log(pred))
  Lu <- obs*(log(obs))
  err <- -sum(2*N*(Lc-Lu))
  return(err)
}


#' Binomial Log Likelihood
#'
#' @inheritParams binomialg2
#'
#' @return Scalar numeric vector
#' @export
binomialLL <- function(obs,pred,N) {
  obs <- obs * N
  ll <-  obs[obs!=0]*log(pred[obs!=0]) + ((N-obs)[obs!=0])*log(1-pred[obs!=0])
  err <- -sum(ll)
  return(err)
}

#' Multinomial Log Likelihood
#'
#' @inheritParams binomialg2
#'
#' @return Scalar numeric vector
#' @export
multinomialLL <- function(obs,pred,N){
  obs = obs * N
  ll <- obs[obs!=0]*log(pred[obs!=0])
  err <- -sum(ll)
  return(err)
}
