\name{kernel}
\alias{kbvpdf}
\alias{kbvcdf}
\title{Kernel Distributions}
\description{Probability density functions and cumulative distribution functions resulting from kernel smoothing. Refer to the vignette for better examples.}
\usage{
kbvpdf (x, y, bw.X, bw.Y)
kbvcdf (x, y, bw.X, bw.Y)
}
\value{These functions return functions. However, currently they can't be evaluated.}
\arguments{
\item{x}{The x values.}
\item{y}{The y values.}
\item{bw.X}{Numeric scalar, giving the X bandwidth.}
\item{bw.Y}{Numeric scalar, giving the Y bandwidth.}
}
\examples{
#construct some kernel distributions
#(based on example in KernSmooth package)
data ("geyser", package="MASS")
kbvpdf.f = kbvpdf (geyser$duration, geyser$waiting, 0.7, 7)
kbvcdf.f = kbvcdf (geyser$duration, geyser$waiting, 0.7, 7)
}
