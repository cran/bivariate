\name{poisson}
\alias{pbvpmf}
\alias{pbvcdf}
\title{Poisson Distributions}
\description{Poisson distributions. Refer to the vignette for better examples and references.}
\usage{
pbvpmf (mean.X, mean.Y, covariance)
pbvcdf (mean.X, mean.Y, covariance)
}
\value{These functions return functions.}
\seealso{\link{pbvpmf.f}, \link{pbvcdf.f}}
\arguments{
\item{mean.X}{Numeric scalar, giving the mean (and variance) of X.}
\item{mean.Y}{Numeric scalar, giving the mean (and variance) of Y.}
\item{covariance}{Numeric scalar, giving the covariance of X and Y.}
}
\examples{
#construct some Poisson distributions
pbvpmf.f = pbvpmf (10, 10, 0)
pbvcdf.f = pbvcdf (10, 10, 0)
}
