\name{05_normal}
\alias{nbvpdf}
\alias{nbvpdf.2}
\alias{nbvcdf}
\alias{nbvcdf.2}
\title{Normal Distributions}
\description{Bivariate normal distributions.}
\usage{
nbvpdf (mean.X, mean.Y, sd.X, sd.Y, cor)
nbvcdf (mean.X, mean.Y, sd.X, sd.Y, cor)

nbvpdf.2 (mean.X, mean.Y, var.X, var.Y, cov)
nbvcdf.2 (mean.X, mean.Y, var.X, var.Y, cov)
}
\value{
Self-referencing functions of the form:

function (x, y) \{\dots\}

Where x and y are numeric vectors.
}
\arguments{
\item{mean.X, mean.Y}{Numeric values, giving the means of X and Y.}
\item{sd.X, sd.Y}{Numeric values, giving the standard deviations of X and Y.}
\item{var.X, var.Y}{Numeric values, giving the variances of X and Y.}
\item{cor}{Numeric value, giving the correlation of X and Y.}
\item{cov}{Numeric value, giving the covariance of X and Y.}
}
\examples{
f <- nbvpdf (0, 0, 1, 1, 0)

f (0, 0)
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
