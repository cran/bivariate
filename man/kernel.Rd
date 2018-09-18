\name{kernel}
\alias{kbvpdf}
\alias{kbvcdf}
\title{Kernel Distributions}
\description{Kernel density (and distribution function) estimation. Refer to the vignette for better examples.}
\usage{
kbvpdf (x, y, bandwidth.X, bandwidth.Y)
kbvcdf (x, y, bandwidth.X, bandwidth.Y)
}
\value{These functions return functions. However, currently they can't be evaluated.}
\arguments{
\item{x}{The x values.}
\item{y}{The y values.}
\item{bandwidth.X}{Scalar, giving the X bandwidth.}
\item{bandwidth.Y}{Scalar, giving the Y bandwidth.}
}
\examples{
#construct some kernel distributions
xy = generate.bivariate.sample (200)
kbvpdf.f = kbvpdf (xy [,1], xy [,2], 20, 20)
kbvcdf.f = kbvcdf (xy [,1], xy [,2], 20, 20)
}
