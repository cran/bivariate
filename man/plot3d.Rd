\name{plot3d}
\alias{plot3d.empty}
\alias{plot3d.step.regular}
\alias{plot3d.step}
\alias{plot3d.continuous.regular}
\alias{plot3d.continuous}
\title{Alternatives to The persp() function}
\description{Alternatives to the persp() function. The angle of the plot is fixed. It interpolates between two colours. Flatter sections are closer to the first colour and steeper sections are closer to the second colour. See the vignette for examples.}
\usage{
plot3d.empty ()
plot3d.step.regular (z, xlab="x", ylab="y", zlim, \dots, boundary=FALSE)
plot3d.step (x, y, z, xlab="x", ylab="y", zlim, \dots, boundary=FALSE)
plot3d.continuous.regular (z, xlab="x", ylab="y", zlim, \dots,
	col1=rgb (0.3, 0.6, 1), col2="white")
plot3d.continuous (x, y, z, xlab="x", ylab="y", zlim, \dots,
	col1=rgb (0.3, 0.6, 1), col2="white")
}
\arguments{
\item{x}{A vector of sorted x values.}
\item{y}{A vector of sorted y values.}
\item{z}{A matrix of z values. The nrow(z) should equal length(x) and the ncol(z) should equal length(y). Increasing row indices correspond to increasing x values and increasing column indices correspond to increasing y values. Note that currently there must be at least two unique z values.}
\item{xlab}{The x label, defaults to x.}
\item{ylab}{The y label, defaults to y.}
\item{zlim}{The z range of the plot.}
\item{\dots}{Other arguments.}
\item{boundary}{False by default, if true use a different colour for the boundary steps. This feature is experimental.}
\item{col1}{The first colour.}
\item{col2}{The second colour.}
}
\examples{
#plot of a bivariate function
x = y = 1:4
f = function (x, y) 2 * x + y ^ 2
z = outer (x, y, f)
plot3d.continuous.regular (z)
}
