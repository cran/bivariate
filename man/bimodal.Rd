\name{bimodal}
\alias{bmbvpdf}
\alias{bmbvcdf}
\title{Bimodal Distributions}
\description{Bimodal distributions. Refer to the vignette for better examples.}
\usage{
bmbvpdf (mean.X1, mean.Y1, variance.X1, variance.Y1,
	mean.X2, mean.Y2, variance.X2, variance.Y2)
bmbvcdf (mean.X1, mean.Y1, variance.X1, variance.Y1,
	mean.X2, mean.Y2, variance.X2, variance.Y2)
}
\value{These functions return functions.}
\seealso{\link{bmbvpdf.f}, \link{bmbvcdf.f}}
\arguments{
\item{mean.X1}{Scalar, giving the mean of the first X component.}
\item{mean.Y1}{Scalar, giving the mean of the first Y component.}
\item{variance.X1}{Scalar, giving the variance of the first X component.}
\item{variance.Y1}{Scalar, giving the variance of the first Y component.}
\item{mean.X2}{Scalar, giving the mean of the second X component.}
\item{mean.Y2}{Scalar, giving the mean of the second Y component.}
\item{variance.X2}{Scalar, giving the variance of the second X component.}
\item{variance.Y2}{Scalar, giving the variance of the second Y component.}
}
\examples{
#construct some bimodal distributions
bmbvpdf.f = bmbvpdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)
bmbvcdf.f = bmbvcdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)
}
