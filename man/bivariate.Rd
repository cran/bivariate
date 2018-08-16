\name{bivariate}
\alias{nbpdf}
\alias{nbcdf}
\title{Bivariate Probability Distributions}
\description{Bivariate probability distributions. See the vignette for better examples.}
\usage{
nbpdf (mean.x, mean.y, variance.x, variance.y, covariance)
nbcdf (mean.x, mean.y, variance.x, variance.y, covariance)
}
\value{The functions nbpdf() and nbcdf() return functions.}
\seealso{\link{nbpdf.f}, \link{nbcdf.f}}
\arguments{
\item{mean.x}{Scalar, giving the mean of x.}
\item{mean.y}{Scalar, giving the mean of y.}
\item{variance.x}{Scalar, giving the variance of x.}
\item{variance.y}{Scalar, giving the variance of y.}
\item{covariance}{Scalar, giving the covariance of x and y.}
}
\examples{
#construct some bivariate probability distributions
nbpdf.f = nbpdf (0, 0, 1, 1, 0)
nbcdf.f = nbcdf (0, 0, 1, 1, 0)
}
