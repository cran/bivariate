\name{rd_05_normal}
\alias{nbvpdf}
\alias{nbvpdf.2}
\alias{nbvcdf}
\alias{nbvcdf.2}
\title{Normal Distributions}
\description{
Normal distributions.

Refer to the vignette for better examples.
}
\usage{
nbvpdf (mean.X, mean.Y, sd.X, sd.Y, cor)
nbvcdf (mean.X, mean.Y, sd.X, sd.Y, cor)

nbvpdf.2 (mean.X, mean.Y, var.X, var.Y, cov)
nbvcdf.2 (mean.X, mean.Y, var.X, var.Y, cov)
}
\value{
Returns functions of the form:

function (x, y) = \{...\}

Where x and y are numeric vectors.
}
\arguments{
\item{mean.X}{Numeric scalar, giving the mean of X.}
\item{mean.Y}{Numeric scalar, giving the mean of Y.}
\item{sd.X}{Numeric scalar, giving the sd of X.}
\item{sd.Y}{Numeric scalar, giving the sd of Y.}
\item{var.X}{Numeric scalar, giving the variance of X.}
\item{var.Y}{Numeric scalar, giving the variance of Y.}
\item{cor}{Numeric scalar, giving the correlation of X and Y.}
\item{cov}{Numeric scalar, giving the correlation of X and Y.}
}
\examples{
f = nbvpdf (0, 0, 1, 1, 0)

f (0, 0)
}
