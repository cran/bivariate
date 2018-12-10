\name{methods}
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
\method{plot}{dubvpmf}(x, xlim, ylim, \dots)
\method{plot}{dubvcdf}(x, xlim, ylim, \dots)
\method{plot}{bnbvpmf}(x, xlim, ylim, \dots)
\method{plot}{bnbvcdf}(x, xlim, ylim, \dots)
\method{plot}{pbvpmf}(x, xmax, ymax, \dots)
\method{plot}{pbvcdf}(x, xmax, ymax, \dots)
\method{plot}{cubvpdf}(x, use.plot3d=FALSE, npoints=20,
    xlim, ylim, \dots, all=FALSE)
\method{plot}{cubvcdf}(x, use.plot3d=FALSE, npoints=20,
    xlim, ylim, \dots)
\method{plot}{nbvpdf}(x, use.plot3d=FALSE, npoints=20,
    xlim, ylim, \dots, all=FALSE)
\method{plot}{nbvcdf}(x, use.plot3d=FALSE, npoints=20,
    xlim, ylim, \dots)
\method{plot}{bmbvpdf}(x, use.plot3d=FALSE, npoints=30,
    xlim, ylim, \dots, all=FALSE)
\method{plot}{bmbvcdf}(x, use.plot3d=FALSE, npoints=30,
    xlim, ylim, \dots)
\method{plot}{kbvpdf}(x, use.plot3d=FALSE, npoints=30,
    xlim, ylim, \dots, all=FALSE)
\method{plot}{kbvcdf}(x, use.plot3d=FALSE, npoints=30,
    xlim, ylim, \dots)
}
\arguments{
\item{x}{A dubvpmf, dubvcdf, etc object.}
\item{use.plot3d}{False by default. If true use 3d plots.}
\item{npoints}{The number of grid points in each direction. Increase for a smoother surface.}
\item{xlim}{The x range for the plot.}
\item{ylim}{The y range for the plot.}
\item{xmax}{The x range max for the plot.}
\item{ymax}{The y range max for the plot.}
\item{all}{False by default. If true, plot a two by two grid of both the PDF and CDF using both contour and 3d plots.}
\item{\dots}{Other arguments.}
}
\examples{
#plot a bivariate probability distribution
dubvpmf.f = dubvpmf (0, 1, 0, 1)
plot (dubvpmf.f)
}
