\name{rd_01_uniform}
\alias{dubvpmf}
\alias{dubvcdf}
\alias{cubvpdf}
\alias{cubvcdf}
\title{Discrete and Continuous Uniform Distributions}
\description{Discrete and continuous uniform distributions.

Refer to the vignette for better examples.}
\usage{
dubvpmf (a.X, b.X, a.Y, b.Y)
dubvcdf (a.X, b.X, a.Y, b.Y)

cubvpdf (a.X, b.X, a.Y, b.Y)
cubvcdf (a.X, b.X, a.Y, b.Y)
}
\value{
Returns functions of the form:

function (x, y) = \{...\}

Where x and y are integers (discrete case) or numeric vectors (continuous case).
}
\arguments{
\item{a.X}{Numeric scalar, giving the a value of X.}
\item{b.X}{Numeric scalar, giving the b value of X.}
\item{a.Y}{Numeric scalar, giving the a value of Y.}
\item{b.Y}{Numeric scalar, giving the b value of Y.}
}
\examples{
f = dubvpmf (1, 4, 1, 4)

f (1, 1)
}
