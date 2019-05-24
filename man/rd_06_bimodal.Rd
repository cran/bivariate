\name{rd_06_bimodal}
\alias{bmbvpdf}
\alias{bmbvpdf.2}
\alias{bmbvcdf}
\alias{bmbvcdf.2}
\title{Bimodal Distributions}
\description{
Bimodal distributions.

Refer to the vignette for better examples.}
\usage{
bmbvpdf (mean.X1, mean.Y1, sd.X1, sd.Y1,
    mean.X2, mean.Y2, sd.X2, sd.Y2)
bmbvcdf (mean.X1, mean.Y1, sd.X1, sd.Y1,
    mean.X2, mean.Y2, sd.X2, sd.Y2)

bmbvpdf.2 (mean.X1, mean.Y1, var.X1, var.Y1,
    mean.X2, mean.Y2, var.X2, var.Y2)
bmbvcdf.2 (mean.X1, mean.Y1, var.X1, var.Y1,
    mean.X2, mean.Y2, var.X2, var.Y2)
}
\value{
Returns functions of the form:

function (x, y) = \{...\}

Where x and y are numeric vectors.
}
\arguments{
\item{mean.X1}{Numeric scalar, giving the mean of the first X component.}
\item{mean.Y1}{Numeric scalar, giving the mean of the first Y component.}
\item{sd.X1}{Numeric scalar, giving the sd of the first X component.}
\item{sd.Y1}{Numeric scalar, giving the sd of the first Y component.}
\item{var.X1}{Numeric scalar, giving the variance of the first X component.}
\item{var.Y1}{Numeric scalar, giving the variance of the first Y component.}
\item{mean.X2}{Numeric scalar, giving the mean of the second X component.}
\item{mean.Y2}{Numeric scalar, giving the mean of the second Y component.}
\item{sd.X2}{Numeric scalar, giving the variance of the second X component.}
\item{sd.Y2}{Numeric scalar, giving the variance of the second Y component.}
\item{var.X2}{Numeric scalar, giving the variance of the second X component.}
\item{var.Y2}{Numeric scalar, giving the variance of the second Y component.}
}
\examples{
f = bmbvpdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)

f (0, 0)
}
