\name{rd_08_nonparametric}
\alias{kbvpdf}
\alias{ebvcdf}
\title{Nonparametric Distributions}
\description{Kernel density estimation and empirical cumulative distribution functions.

Refer to the vignette for better examples.}
\usage{
kbvpdf (x, y, bw.X, bw.Y)
ebvcdf (x, y)
}
\value{
Returns functions of the form:

function (x, y) = \{...\}

Where x and y are numeric vectors.
}
\details{
The kbvpdf() function calls the bkde2D() function from the KernSmooth package.

Refer the probhat package for kernel smoothing, more generally.
}
\arguments{
\item{x}{A vector of x values.}
\item{y}{A vector of y values.}
\item{bw.X}{Numeric scalar, giving the X bandwidth.}
\item{bw.Y}{Numeric scalar, giving the Y bandwidth.}
}
\examples{
x = rnorm (20)
y = rnorm (20)
f = ebvcdf (x, y)

f (0, 0)
}
