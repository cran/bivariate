\name{methods}
\alias{print.dubvpmf}
\alias{print.dubvcdf}
\alias{print.bnbvpmf}
\alias{print.bnbvcdf}
\alias{print.pbvpmf}
\alias{print.pbvcdf}
\alias{print.cubvpdf}
\alias{print.cubvcdf}
\alias{print.nbvpdf}
\alias{print.nbvcdf}
\alias{print.bmbvpdf}
\alias{print.bmbvcdf}
\alias{print.kbvpdf}
\alias{print.kbvcdf}
\alias{plot.dubvpmf}
\alias{plot.dubvcdf}
\alias{plot.bnbvpmf}
\alias{plot.bnbvcdf}
\alias{plot.pbvpmf}
\alias{plot.pbvcdf}
\alias{plot.cubvpdf}
\alias{plot.cubvcdf}
\alias{plot.nbvpdf}
\alias{plot.nbvcdf}
\alias{plot.bmbvpdf}
\alias{plot.bmbvcdf}
\alias{plot.kbvpdf}
\alias{plot.kbvcdf}
\title{Methods}
\description{Bivariate probability distribution methods.}
\usage{
\method{print}{dubvpmf}(x, \dots)
\method{print}{dubvcdf}(x, \dots)
\method{print}{bnbvpmf}(x, \dots)
\method{print}{bnbvcdf}(x, \dots)
\method{print}{pbvpmf}(x, \dots)
\method{print}{pbvcdf}(x, \dots)
\method{print}{cubvpdf}(x, \dots)
\method{print}{cubvcdf}(x, \dots)
\method{print}{nbvpdf}(x, \dots)
\method{print}{nbvcdf}(x, \dots)
\method{print}{bmbvpdf}(x, \dots)
\method{print}{bmbvcdf}(x, \dots)
\method{print}{kbvpdf}(x, \dots)
\method{print}{kbvcdf}(x, \dots)
\method{plot}{dubvpmf}(x, xlab="x", ylab="y", xlim, ylim, zlim, \dots)
\method{plot}{dubvcdf}(x, xlab="x", ylab="y", xlim, ylim, zlim, \dots)
\method{plot}{bnbvpmf}(x, xlab="x", ylab="y", xlim, ylim, zlim, \dots)
\method{plot}{bnbvcdf}(x, xlab="x", ylab="y", xlim, ylim, zlim, \dots)
\method{plot}{pbvpmf}(x, xlab="x", ylab="y", xmax, ymax, zlim, \dots)
\method{plot}{pbvcdf}(x, xlab="x", ylab="y", xmax, ymax, zlim, \dots)
\method{plot}{cubvpdf}(x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, \dots,
	all=FALSE)
\method{plot}{cubvcdf}(x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, \dots)
\method{plot}{nbvpdf}(x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, \dots,
	all=FALSE)
\method{plot}{nbvcdf}(x, use.plot3d=FALSE, xlab="x", ylab="y", xlim, ylim, zlim, \dots)
\method{plot}{bmbvpdf}(x, use.plot3d=FALSE, np=20, xlab="x", ylab="y",
	xlim, ylim, zlim, \dots, all=FALSE)
\method{plot}{bmbvcdf}(x, use.plot3d=FALSE, np=20, xlab="x", ylab="y",
	xlim, ylim, zlim, \dots)
\method{plot}{kbvpdf}(x, use.plot3d=FALSE, np=20, xlab="x", ylab="y",
	xlim, ylim, zlim, \dots, all=FALSE)
\method{plot}{kbvcdf}(x, use.plot3d=FALSE, np=20, xlab="x", ylab="y",
	xlim, ylim, zlim, \dots)
}
\arguments{
\item{x}{A dubvpmf, dubvcdf, etc object.}
\item{all}{False by default. If true, plot a two by two grid of both the PDF and CDF using both contour and 3d plots.}
\item{use.plot3d}{False by default. If true use 3d plots.}
\item{np}{The number of x and y points. Increase for a smoother surface.}
\item{xlab}{The x label, x by default.}
\item{ylab}{The y label, y by default.}
\item{xlim}{The x range for the plot.}
\item{ylim}{The y range for the plot.}
\item{xmax}{The x range max for the plot.}
\item{ymax}{The y range max for the plot.}
\item{zlim}{The z range for the plot.}
\item{\dots}{Other arguments.}
}
\examples{
#print a bivariate probability distribution
dubvpmf.f = dubvpmf (0, 1, 0, 1)
dubvpmf.f
}
