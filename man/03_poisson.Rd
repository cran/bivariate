\name{03_poisson}
\alias{pbvpmf}
\alias{pbvpmf.2}
\alias{pbvcdf}
\alias{pbvcdf.2}
\title{Poisson Distributions}
\description{Bivariate Poisson distributions.}
\usage{
pbvpmf (lambda.1, lambda.2, lambda.3)
pbvcdf (lambda.1, lambda.2, lambda.3)

pbvpmf.2 (mean.X, mean.Y, cov)
pbvcdf.2 (mean.X, mean.Y, cov)
}
\value{
Self-referencing functions of the form:

function (x, y) \{\dots\}

Where x and y are integer-valued vectors.
}
\arguments{
\item{lambda.1, lambda.2, lambda.3}{Numeric values, giving the first, second and third lambda parameters.}
\item{mean.X, mean.Y}{Numeric values, giving the mean of X and Y.\cr Note that their means equal their variances.}
\item{cov}{Numeric value, giving the covariance of X and Y.}
}
\examples{
f <- pbvpmf (10, 10, 0)

f (4, 4)
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
