
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
#' @author Will Hopper
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
  return((alpha*beta)/(((alpha+beta)^2)*(alpha + beta + 1)))
}

#' betaABfromMeanKappa
#' Beta distribution shape parameters recovered from mean and concentration:
#' @param mean numeric scalar in range (0,1)
#' @param kappa numeric scalar in range (0,Inf)
#'
#' @return Named list with 2 elements:
#' a = Beta shape parameter #1,
#' b = Beta shape parameter #2
#' @export
#'
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#'
#' @examples
#' betaABfromMeanKappa(mean = .6, kappa=15)
betaABfromMeanKappa = function( mean , kappa ) {
  if ( mean <=0 | mean >= 1) stop("must have 0 < mean < 1")
  if ( kappa < 0 ) stop("kappa must be > 0")
  a = mean * kappa
  b = ( 1.0 - mean ) * kappa
  return( list( a=a , b=b ) )
}

#' betaABfromModeKappa
#' Beta distribution shape parameters recovered from mode and concentration:
#' @param mode numeric scalar in range (0,1)
#' @param kappa numeric scalar in range (0,Inf)
#'
#' @return Named list with 2 elements:
#' a = Beta shape parameter #1,
#' b = Beta shape parameter #2
#' @export
#'
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#'
#' @examples
#' betaABfromModeKappa(mode = .4, kappa=10)
betaABfromModeKappa = function( mode , kappa ) {
  if ( any(mode <=0) || any(mode >= 1)) stop("must have 0 < mode < 1")
  if ( any(kappa <2) ) stop("kappa must be > 2 for mode parameterization")
  a = mode * ( kappa - 2 ) + 1
  b = ( 1.0 - mode ) * ( kappa - 2 ) + 1
  return( list( a=a , b=b ) )
}

#' betaABfromMeanSD
#' Beta distribution shape parameters recovered from mean and standard deviation
#' @param mean numeric scalar in range (0,1)
#' @param sdnumeric scalar in range (0,Inf)
#'
#' @return Named list with 2 elements:
#' a = Beta shape parameter #1,
#' b = Beta shape parameter #2
#' @export
#'
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#'
#' @examples
#' betaABfromMeanSD(mean =.5, sd = 1)
betaABfromMeanSD = function( mean , sd ) {
  if (any(mean <=0) || any(mean >= 1)) stop("must have 0 < mean < 1")
  if ( any(sd <= 0 )) stop("sd must be > 0")
  kappa = mean*(1-mean)/sd^2 - 1
  if ( any(kappa < 0 )) stop("invalid combination of mean and sd")
  a = mean * kappa
  b = ( 1.0 - mean ) * kappa
  return( list( a=a , b=b ) )
}

#' gammaShRaFromMeanSD
#' Gamma distribution shape and rate parameters recovered fromm mean and sd
#' @param mean
#' @param sd
#'
#' @return A named list with 2 elements:
#' shape = Gamma distribution shape paratmeter
#' rate = Gamma distribution rate parameter
#' @export
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#' @examples
#' gammaShRaFromMeanSD(mode = 2, sd = 3)
gammaShRaFromMeanSD = function( mean , sd ) {
  if ( any(mean <=0) ) stop("mean must be > 0")
  if ( any(sd <=0) ) stop("sd must be > 0")
  shape = mean^2/sd^2
  rate = mean/sd^2
  return( list( shape=shape , rate=rate ) )
}

#' gammaShRaFromModeSD
#'Gamma distribution shape and rate parameters recovered fromm mode and sd
#' @param mode
#' @param sd
#'
#' @return A named list with 2 elements:
#' shape = Gamma distribution shape paratmeter
#' rate = Gamma distribution rate parameter
#' @export
#'
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#' @examples
#' gammaShRaFromModeSD(mode = 2, sd = 3)
gammaShRaFromModeSD = function( mode , sd ) {
  if ( mode <=0 ) stop("mode must be > 0")
  if ( sd <=0 ) stop("sd must be > 0")
  rate = ( mode + sqrt( mode^2 + 4 * sd^2 ) ) / ( 2 * sd^2 )
  shape = 1 + mode * rate
  return( list( shape=shape , rate=rate ) )
}
