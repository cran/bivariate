\name{rd_10_plot_methods}
\alias{plot.dubvpmf}
\alias{plot.dubvcdf}
\alias{plot.bnbvpmf}
\alias{plot.bnbvcdf}
\alias{plot.pbvpmf}
\alias{plot.pbvcdf}
\alias{plot.cbvpmf}
\alias{plot.cubvpdf}
\alias{plot.cubvcdf}
\alias{plot.nbvpdf}
\alias{plot.nbvcdf}
\alias{plot.bmbvpdf}
\alias{plot.bmbvcdf}
\alias{plot.dtvpdf}
\alias{plot.kbvpdf}
\alias{plot.ebvcdf}
\title{Plot Methods}
\description{Plot methods for bv (bivariate) objects.}
\usage{
\method{plot}{dubvpmf}(x, use.plot3d=FALSE, xlim, ylim, \dots, all=FALSE)
\method{plot}{dubvcdf}(x, use.plot3d=FALSE, xlim, ylim, \dots)

\method{plot}{bnbvpmf}(x, use.plot3d=FALSE, xlim, ylim, \dots, all=FALSE)
\method{plot}{bnbvcdf}(x, use.plot3d=FALSE, xlim, ylim, \dots)

\method{plot}{pbvpmf}(x, use.plot3d=FALSE, xmax, ymax, \dots, all=FALSE)
\method{plot}{pbvcdf}(x, use.plot3d=FALSE, xmax, ymax, \dots)

\method{plot}{cbvpmf}(x, use.plot3d, \dots)

\method{plot}{cubvpdf}(x, use.plot3d=FALSE, npoints=20, xlim, ylim, \dots, all=FALSE)
\method{plot}{cubvcdf}(x, use.plot3d=FALSE, npoints=20, xlim, ylim, \dots)

\method{plot}{nbvpdf}(x, use.plot3d=FALSE, npoints=20, xlim, ylim, \dots, all=FALSE)
\method{plot}{nbvcdf}(x, use.plot3d=FALSE, npoints=20, xlim, ylim, \dots)

\method{plot}{bmbvpdf}(x, use.plot3d=FALSE, npoints=20, xlim, ylim, \dots, all=FALSE)
\method{plot}{bmbvcdf}(x, use.plot3d=FALSE, npoints=20, xlim, ylim, \dots)

\method{plot}{dtvpdf}(x, use.plot3d=FALSE, npoints=20, log=FALSE, \dots)

\method{plot}{kbvpdf}(x, use.plot3d=FALSE, npoints=30, xlim, ylim, \dots)

\method{plot}{ebvcdf}(x, use.plot3d=FALSE, mpoints=30, \dots)
}
\arguments{
\item{x}{A bv object.}
\item{use.plot3d}{If false, a 2d plot, or if true, a 3d plot.}
\item{npoints}{The number of grid points in each direction. Increase for a smoother surface.}
\item{mpoints}{The max number of points, for plotting as a step function.}
\item{xlim}{The x range for the plot.}
\item{ylim}{The y range for the plot.}
\item{xmax}{The x range max for the plot.}
\item{ymax}{The y range max for the plot.}
\item{log}{If true, plot the log density.}
\item{all}{If true, plot a two by two grid of both the PMF (or PDF) and CDF using both 2d and 3d plots.}
\item{\dots}{Other arguments.}
}
\examples{
f = dubvpmf (1, 4, 1, 4)
plot (f)
}
