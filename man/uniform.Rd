\name{uniform}
\alias{dubvpmf}
\alias{dubvcdf}
\alias{cubvpdf}
\alias{cubvcdf}
\title{Discrete and Continuous Uniform Distributions}
\description{Discrete and continuous uniform distributions. Refer to the vignette for better examples.}
\usage{
dubvpmf (a.X, b.X, a.Y, b.Y)
dubvcdf (a.X, b.X, a.Y, b.Y)
cubvpdf (a.X, b.X, a.Y, b.Y)
cubvcdf (a.X, b.X, a.Y, b.Y)
}
\value{These functions return functions.}
\seealso{\link{dubvpmf.f}, \link{dubvcdf.f}, \link{cubvpdf.f}, \link{cubvcdf.f}}
\arguments{
\item{a.X}{Numeric scalar, giving the a value of X.}
\item{b.X}{Numeric scalar, giving the b value of X.}
\item{a.Y}{Numeric scalar, giving the a value of Y.}
\item{b.Y}{Numeric scalar, giving the b value of Y.}
}
\examples{
#construct some discrete uniform distributions
dubvpmf.f = dubvpmf (0, 1, 0, 1)
dubvcdf.f = dubvcdf (0, 1, 0, 1)
}
