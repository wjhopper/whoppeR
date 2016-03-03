
#' d'
#' Calculate the d-prime statistic using the hit and false alarm rate
#' to stimuli from the target and lure distribution, respectively.
#'
#' @param HR A logical vector of yes (TRUE)/no (FALSE) responses to stimili from
#' the target (S1) distribution. Used to calculate the Hit Rate.
#' @param FA  logical vector of yes (TRUE)/no (FALSE) responses to stimili from
#' the lure (S0) distribution. Used to calculate the False Alarm Rate.
#' @param correct If true, the 1/2n correction is applied for all yes or all no responses so that
#' the dprime statistic is not Inf or NaN after the z-transform is applied to the Hit and False Alarm Rates
#'
#' @return
#' A scalar numeric vector
#' @export
#'
#' @examples
#' dprime(c(0,0,1,1), c(1,0,0,0))
#' dprime(c(0,0,0,0), c(0,0,0,0), correct = TRUE)
#' dprime(c(0,0,0,0), c(0,0,0,0)) # NaN


dprime <- function(targets, lures,  correct = FALSE) {
  HR <- mean(targets)
  FA <- mean(lures)

  if (correct) {
    if (HR == 0 || HR == 1 ) {
      HR <- max(1/(2*length(targets)), HR - 1/(2*length(targets)))
    }

    if (FA == 0  || FA == 1) {
      FA <- max(1/(2*length(lures)), FA - 1/(2*length(lures)))
    }
  }

  dprime <- qnorm(HR) - qnorm(FA)
  return(dprime)
}




