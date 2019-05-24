\name{rd_03_poisson}
\alias{pbvpmf}
\alias{pbvpmf.2}
\alias{pbvcdf}
\alias{pbvcdf.2}
\title{Poisson Distributions}
\description{
Poisson distributions.

Refer to the vignette for better examples and references.}
\usage{
pbvpmf (lambda.1, lambda.2, lambda.3)
pbvcdf (lambda.1, lambda.2, lambda.3)

pbvpmf.2 (mean.X, mean.Y, cov)
pbvcdf.2 (mean.X, mean.Y, cov)
}
\value{
Returns functions of the form:

function (x, y) = \{...\}

Where x and y are integers.
}
\arguments{
\item{lambda.1}{Integer scalar, giving the first lambda parameter.}
\item{lambda.2}{Integer scalar, giving the second lambda parameter.}
\item{lambda.3}{Integer scalar, giving the third lambda parameter.}
\item{mean.X}{Integer scalar, giving the mean (and variance) of X.}
\item{mean.Y}{Integer scalar, giving the mean (and variance) of Y.}
\item{cov}{Integer scalar, giving the covariance of X and Y.}
}
\examples{
f = pbvpmf (10, 10, 0)

f (4, 4)
}
