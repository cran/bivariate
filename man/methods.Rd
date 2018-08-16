\name{methods}
\alias{print.nbpdf}
\alias{print.nbcdf}
\alias{plot.nbpdf}
\alias{plot.nbcdf}
\title{Methods}
\description{Bivariate probability distribution methods.}
\seealso{\link{nbpdf}, \link{nbcdf}}
\usage{
\method{print}{nbpdf}(x, \dots)
\method{print}{nbcdf}(x, \dots)
\method{plot}{nbpdf}(x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, \dots,
	all=FALSE)
\method{plot}{nbcdf}(x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, \dots)
}
\arguments{
\item{x}{An nbpdf or nbcdf object.}
\item{all}{False by default. If true, plot a two by two grid of both the PDF and CDF using both contour and 3d plots.}
\item{use.plot3d}{False by default. If true use 3d plots.}
\item{xlab}{The x label, x by default.}
\item{ylab}{The y label, y by default.}
\item{xlim}{The x range for the plot.}
\item{ylim}{The y range for the plot.}
\item{zlim}{The z range for the plot.}
\item{\dots}{Other arguments.}
}
\examples{
#print a bivariate probability distribution
nbpdf.f = nbpdf (0, 0, 1, 1, 0)
nbpdf.f
}
