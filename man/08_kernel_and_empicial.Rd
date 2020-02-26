\name{08_kernel_and_empirical}
\alias{kbvpdf}
\alias{ebvcdf}
\title{Nonparametric Distributions}
\description{Bivariate kernel density estimates and bivariate empirical cumulative distribution functions.}
\usage{
#kernel density estimates
kbvpdf (x, y, bw.X, bw.Y)

#ecdf
ebvcdf (x, y)
}
\value{
Self-referencing functions of the form:

function (x, y) \{\dots\}

Where x and y are numeric vectors.

Note that you can't evaluate the function representing kernel density estimates.
}
\arguments{
\item{x, y}{Numeric values of x and y values.}
\item{bw.X, bw.Y}{Numeric values, giving the X and Y bandwidths.}
}
\examples{
x <- rnorm (20)
y <- rnorm (20)
fh <- ebvcdf (x, y)

fh (0, 0)
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.

Note that the probhat package provides more tools for kernel smoothing.}
