% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stats.R
\name{betaMoments}
\alias{betaMoments}
\title{Beta Distribution Moments
Calculate the mean and variance of a beta distribution, given its two shape parameters}
\usage{
betaMoments(alpha, beta)
}
\arguments{
\item{alpha}{Beta distribution shape parameter #1 [0, Inf]}

\item{beta}{Beta distribution shape parameter #2 [0, Inf]}
}
\value{
Named list with 2 elements:
mean = mean of given beta disitribution
var = variance of given beta disitribution
}
\description{
Beta Distribution Moments
Calculate the mean and variance of a beta distribution, given its two shape parameters
}
\examples{
betaMoments(alpha=.01, beta = 1)
}
\author{
Will Hopper
}
