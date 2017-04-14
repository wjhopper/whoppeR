#' Count concordances and discordances
#' @description Count concordances and discordances between the ordering of two numeric vectors
#'
#' @param x A numeric vector
#' @param y A numeric vector
#'
#' @details \code{x} and \code{y} must have equal lengths.
#'
#' @return A numeric vector with elements names "concordances" and "discordances".
#'
#' @export
#'
#' @examples
#' rank_agreements(x = c(1, 2, 3), c(3, 1, 2))
#'
#' @seealso The \code{\link{rococo}} function in the \code{rococo} package.
#'
rank_agreements <- function(x, y) {

  if (!(is.numeric(x) && is.numeric(y)) ||
      !(is.atomic(x) && is.vector(x)) ||
      !(is.atomic(y) && is.vector(y))) {
    stop("x and y must be numeric vectors")
  }

  if (length(x) != length(y)) {
    stop("x and y vectors must be the same length")
  }

  Rx <- outer(x,x,function(x1,x2) sign(x1-x2))
  Ry <- outer(y,y,function(y1,y2) sign(y1-y2))
  agreement <- Rx*Ry

  conc <- sum(agreement == 1)
  disc <- sum(agreement == -1)

  x <- c("concordances" = conc,
         "discordances" = disc)

  return(x)

}
