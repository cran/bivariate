\name{plot3d}
\alias{plot3d.empty}
\alias{plot3d.discrete}
\alias{plot3d.continuous}
\title{Alternatives to The persp() function}
\description{Alternatives to the persp() function. The angle of the plot is fixed. It interpolates between two colours. Flatter sections are closer to the first colour and steeper sections are closer to the second colour. See the vignette for examples.}
\usage{
plot3d.empty ()
plot3d.discrete (x, y, z, xlab="x", ylab="y", zlim, \dots, boundary=FALSE)
plot3d.continuous (x, y, z, xlab="x", ylab="y", zlim, \dots,
	col1=rgb (0.3, 0.6, 1), col2="white")
}
\arguments{
\item{x}{A vector of sorted x values. This can be omitted. For plot3d.discrete() length(x) should equal nrow(z)+1. For plot3d.continuous() length(x) should equal nrow(z).}
\item{y}{A vector of sorted y values. This can be omitted. For plot3d.discrete() length(y) should equal ncol(z)+1. For plot3d.continuous() length(y) should equal ncol(z).}
\item{z}{A matrix of z values. Increasing row indices correspond to increasing x values and increasing column indices correspond to increasing y values.}
\item{xlab}{The x label, defaults to x.}
\item{ylab}{The y label, defaults to y.}
\item{zlim}{The z range of the plot.}
\item{\dots}{Other arguments.}
\item{boundary}{False by default, if true use a different colour for the boundary steps.}
\item{col1}{The first colour.}
\item{col2}{The second colour.}
}
\examples{
#plot of a bivariate function
x = y = 1:4
f = function (x, y) 2 * x + y ^ 2
z = outer (x, y, f)
plot3d.continuous (,, z)
}
