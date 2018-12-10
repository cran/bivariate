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
\item{mean.X}{Numeric scalar, giving the mean of X.}
\item{mean.Y}{Numeric scalar, giving the mean of Y.}
\item{variance.X}{Numeric scalar, giving the variance of X.}
\item{variance.Y}{Numeric scalar, giving the variance of Y.}
\item{covariance}{Numeric scalar, giving the covariance of X and Y.}
}
\examples{
#construct some normal distributions
nbvpdf.f = nbvpdf (0, 0, 1, 1, 0)
nbvcdf.f = nbvcdf (0, 0, 1, 1, 0)
}
