\name{normal}
\alias{nbvpdf}
\alias{nbvcdf}
\title{Normal Distributions}
\description{Normal distributions. Refer to the vignette for better examples.}
\usage{
nbvpdf (mean.X, mean.Y, variance.X, variance.Y, covariance)
nbvcdf (mean.X, mean.Y, variance.X, variance.Y, covariance)
}
\value{These functions return functions.}
\seealso{\link{nbvpdf.f}, \link{nbvcdf.f}}
\arguments{
\item{mean.X}{Scalar, giving the mean of X.}
\item{mean.Y}{Scalar, giving the mean of Y.}
\item{variance.X}{Scalar, giving the variance of X.}
\item{variance.Y}{Scalar, giving the variance of Y.}
\item{covariance}{Scalar, giving the covariance of X and Y.}
}
\examples{
#construct some normal distributions
nbvpdf.f = nbvpdf (0, 0, 1, 1, 0)
nbvcdf.f = nbvcdf (0, 0, 1, 1, 0)
}
