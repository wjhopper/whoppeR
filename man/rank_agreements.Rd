% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/correlations.R
\name{rank_agreements}
\alias{rank_agreements}
\title{Count concordances and discordances}
\usage{
rank_agreements(x, y)
}
\arguments{
\item{x}{A numeric vector}

\item{y}{A numeric vector}
}
\value{
A numeric vector with elements names "concordances" and "discordances".
}
\description{
Count concordances and discordances between the ordering of two numeric vectors
}
\details{
\code{x} and \code{y} must have equal lengths.
}
\examples{
rank_agreements(x = c(1, 2, 3), c(3, 1, 2))

}
\seealso{
The \code{\link{rococo}} function in the \code{rococo} package.
}
