#' t test summary
#' performs a t-test comparing the data from two specified groups,
#' calculates Cohen's d as a measure of effect size, and returns the
#' results in format suitable for including in a matrix, data frame or table
#'
#' @param pair A vector of length two specifying which groups should be compared.
#' @param formula A formula identifying the columns holding the dependent and independent
#' variables in the data frame/matrix.
#' @param data A data frame or matrix containing the variables in the formula.
#' @param ... Other arguments passed to the t.test function
#'
#' @return A named numeric vector, with an element for the Student's t static,
#' the degrees of freedom used for the t.test, the upper and lower bounds of the confidence
#' interval, Cohen's, and the p value.
#'
#' @export
#'
#' @author Will Hopper
#'
t_summary <- function(pair, formula, data, ...) {

  if (missing(formula) || (length(formula) != 3L) || (length(attr(terms(formula[-2L]),
                                                                  "term.labels")) != 1L)) {
    stop("'formula' missing or incorrect")
  }

  if (missing(pair) || (length(pair) != 2L)) {
    stop("'pair' missing or incorrect")
  }

  grouping_var <- as.character(formula)[3L]
  pair_subset <- data[data[[grouping_var]] %in% pair,]
  t <- t.test(formula = formula, data =  pair_subset, ...)
  d <- cohen.d(formula = formula, data = droplevels(pair_subset))
  results <- c(t$statistic,t$parameter,t$conf.int,d$estimate,t$p.value)
  names(results) <- c(paste(t$alternative, "t"),"df", "CI Lower", "CI Upper",
                      "Cohen's d", "p")
  attributes(results) <- c(attributes(results),list(pair = paste(pair, collapse = " - ")))
  return(results)
}

#' Beta Distribution Moments
#' Calculate the mean and variance of a beta distribution, given its two shape parameters
#'
#' @param alpha Beta distribution shape parameter #1 [0, Inf]
#' @param beta  Beta distribution shape parameter #2 [0, Inf]
#'
#' @return Named list with 2 elements:
#' mean = mean of given beta disitribution
#' var = variance of given beta disitribution
#' @export
#'
#' @author Will Hopper
#' @examples
#' betaMoments(alpha=.01, beta = 1)
betaMoments <- function(alpha, beta){
  m <- alpha/(alpha + beta)
  v <- (alpha*beta)/(((alpha+beta)^2)*(alpha + beta + 1))
  return(list(mean=m, var=v))
}


#' Beta a & b
#' Determine the shape parameters of a beta distribution, given its central tendency and spread.
#' See 'Details' section for constraints on which arguments may be used together.
#'
#' @param mean A numeric scalar in range (0,1)
#' @param mode A numeric scalar in range (0,1)
#' @param kappa A numeric scalar in range (0,Inf).
#' @param sd A numeric scalar in range (0,Inf)
#'
#' @return Named list with 2 elements:
#' a = Beta shape parameter #1,
#' b = Beta shape parameter #2
#'
#' @details Only two arguments may be specified together. Specifically, one parameter
#' relating to the distributions central tendency (i.e. mean or mode) must be specified with
#' one parameter relating to its spread (i.e. kappa or sd). Additionally, not all combinations
#' of (mean,mode) and (kappa, sd) are valid; Solutions exist using mean and kappa, mean and sd,
#' and mode and kappa. No solution for the shape parameters using the mode and kappa is used.
#'
#' The kappa parameter refers to the concentration of the beta distribution. As such, it is
#' negatively related to the standard deviation.
#'
#' @examples
#' betaParams(mean = .6, kappa=15)
#' betaParams(mode = .25, kappa=15)
#' betaParams(mean = .6, kappa=15)
#'
#' # Throws an error, because you need one measure of central tendency and one of spread, while
#' # sd and kappa are both measures of spread.
#' \dontrun{betaParams(sd = .2, kappa=15)}
#'
#' @author Will Hopper (2016)
#' Inspired by functions writen by John Krushke to accompany his "Doing Bayesian Data Analysis" book.
#' Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#'
#' @export
betaParams = function( mean, mode, kappa, sd ) {

  input <- as.list(match.call())[-1]
  if (!identical(length(input), 2L)) {
    stop("Exactly two arguments must be specified")
  }

  if (setequal(names(input),c("mean","kappa"))) {
    return(ABfromMeanKappa(mean, kappa))
  } else if (setequal(names(input),c("mode","kappa"))) {
    return(ABfromModeKappa(mode,kappa))
  } else if (setequal(names(input),c("mean","sd"))) {
    return(ABfromMeanSD(mean, sd))
  } else {
    stop("Invalid combination of arguments: Must specify mean & kappa, mean & sd, or mode & kappa")
  }
}

#' ABfromModeKappa
#' @description Beta distribution shape parameters recovered from mode and concentration:
#' @param mode numeric scalar in range (0,1)
#' @param kappa numeric scalar in range (0,Inf)
#'
#' @return Named list with 2 elements:
#' a = Beta shape parameter #1,
#' b = Beta shape parameter #2
#'
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#'
#' @examples
#' \dontrun{ABfromModeKappa(mode = .4, kappa=10)}
ABfromModeKappa = function( mode , kappa ) {
  if ( any(mode <=0) || any(mode >= 1)) stop("must have 0 < mode < 1")
  if ( any(kappa <2) ) stop("kappa must be > 2 for mode parameterization")
  a = mode * ( kappa - 2 ) + 1
  b = ( 1.0 - mode ) * ( kappa - 2 ) + 1
  return( list( a=a , b=b ) )
}

#' ABfromMeanSD
#' @description Beta distribution shape parameters recovered from mean and standard deviation
#' @param mean numeric scalar in range (0,1)
#' @param sd numeric scalar in range (0,Inf)
#'
#' @return Named list with 2 elements:
#' a = Beta shape parameter #1,
#' b = Beta shape parameter #2
#'
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#'
#' @examples
#' \dontrun{ABfromMeanSD(mean =.5, sd = 1)}
ABfromMeanSD = function( mean , sd ) {
  if (any(mean <=0) || any(mean >= 1)) stop("must have 0 < mean < 1")
  if ( any(sd <= 0 )) stop("sd must be > 0")
  mean <- round(mean,10)
  sd <- round(sd,10)
  kappa <- mean*(1-mean)/sd^2 - 1
  if ( any(kappa < 0 )) stop("invalid combination of mean and sd")
  a = mean * kappa
  b = ( 1.0 - mean ) * kappa
  return( list( a=a , b=b ) )
}

#' ABfromMeanKappa
#' @description Beta distribution shape parameters recovered from mean and concentration:
#' @param mode numeric scalar in range (0,1)
#' @param kappa numeric scalar in range (0,Inf)
#'
#' @return Named list with 2 elements:
#' a = Beta shape parameter #1,
#' b = Beta shape parameter #2
#'
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#'
#' @examples
#' \dontrun{ABfromMeanKappa(mean = .6, kappa = 10)}
ABfromMeanKappa = function( mean , kappa ) {
  if ( mean <=0 | mean >= 1) stop("must have 0 < mean < 1")
  if ( kappa < 0 ) stop("kappa must be > 0")
  a = mean * kappa
  b = ( 1.0 - mean ) * kappa
  return( list( a=a , b=b ) )
}

#' Gamma Shape and Rate
#' Determine the shape and rate parameter of a gamma distribution, given its central
#' tendency and spread.See the 'Details' section for constraints on which arguments
#' may be used together.
#'
#' @param mean A numeric vector
#' @param mode A numeric vector
#' @param sd A numeric vector
#'
#' @return Named list with 2 elements:
#' S = Gamma shape parameter (alpha)
#' R = Gamma rate parameter (beta)
#'
#' @details Only two parameters may be specified together. Specifically, one parameter
#' relating to the distributions central tendency (i.e. mean or mode) must be specified with
#' the standard deviation.
#'
#' @examples
#' gammaParams(mean = 1.5, sd = 3)
#' gammaParams(mode = 2, sd = 3)
#'
#' # Throws an error, because mean and mode cannot be used to find the shape and rate.
#' \dontrun{gammaParams(mean = 2, mode = 3)}
#'
#' @author Will Hopper (2016)
#' Inspired by functions writen by John Krushke to accompany his "Doing Bayesian Data Analysis" book.
#' Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#' @export
gammaParams = function(mean, mode, sd) {

  input <- as.list(match.call())[-1]
  if (!identical(length(input), 2L)) {
    stop("Exactly two arguments must be specified")
  }

  if (setequal(names(input), c("mode","sd"))) {
    return(SRfromModeSD(mode,sd))
  } else if (setequal(names(input), c("mean","sd"))) {
    return(SRfromMeanSD(mean, sd))
  } else {
    stop("Invalid combination of arguments: Must specify mean & sd, or mode & sd")
  }
}

#' SRfromMeanSD
#' Gamma distribution shape and rate parameters recovered fromm mean and sd
#' @param mean A numeric vector
#' @param sd A numeric vector
#' @return A named list with 2 elements:
#' shape = Gamma distribution shape paratmeter
#' rate = Gamma distribution rate parameter
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#' @examples
#' \dontrun{SRfromMeanSD(mean = 1.5, sd = 3)}
#'
SRfromMeanSD = function( mean , sd ) {
  if ( any(mean <=0) ) stop("mean must be > 0")
  if ( any(sd <=0) ) stop("sd must be > 0")
  shape = mean^2/sd^2
  rate = mean/sd^2
  return( list( shape=shape , rate=rate ) )
}

#' SRfromModeSD
#' Gamma distribution shape and rate parameters recovered from mode and sd
#' @param mode a numeric vector
#' @param sd a numeric vector
#' @return A named list with 2 elements:
#' shape = Gamma distribution shape paratmeter
#' rate = Gamma distribution rate parameter
#' @author John Krushke, in Kruschke, J. K. (2015). Doing Bayesian Data Analysis, Second Edition:
#' A Tutorial with R, JAGS, and Stan. Academic Press / Elsevier.
#' @examples
#' \dontrun{SRfromModeSD(mode = 2, sd = 3)}
#'
SRfromModeSD = function( mode , sd ) {
  if ( mode <=0 ) stop("mode must be > 0")
  if ( sd <=0 ) stop("sd must be > 0")
  rate = ( mode + sqrt( mode^2 + 4 * sd^2 ) ) / ( 2 * sd^2 )
  shape = 1 + mode * rate
  return( list( shape=shape , rate=rate ) )
}
