\name{10_plot_methods}
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
\alias{plot_ebvcdf_step}
\alias{plot_ebvcdf_smooth}
\title{Plot Methods}
\description{Plot methods for bv (bivariate) objects.}
\details{
These methods use the barsurf package, which contains plotting functions.

Note that the plot method for ebvcdf objects (bivariate ECDFs) calls either the plot_ebvcdf_step or plot_ebvcdf_smooth functions.
}
\usage{
#discrete uniform
\method{plot}{dubvpmf}(x, plot.3d=FALSE, \dots, xlim, ylim, all=FALSE)
\method{plot}{dubvcdf}(x, plot.3d=FALSE, \dots, xlim, ylim)

#binomial
\method{plot}{bnbvpmf}(x, plot.3d=FALSE, \dots, xlim, ylim, all=FALSE)
\method{plot}{bnbvcdf}(x, plot.3d=FALSE, \dots, xlim, ylim)

#poisson
\method{plot}{pbvpmf}(x, plot.3d=FALSE, \dots, xlim, ylim, all=FALSE)
\method{plot}{pbvcdf}(x, plot.3d=FALSE, \dots, xlim, ylim)

#categorical
\method{plot}{cbvpmf}(x, plot.3d, \dots, labels=TRUE)

#continuous uniform
\method{plot}{cubvpdf}(x, plot.3d=FALSE, \dots, xlim, ylim, n=20, all=FALSE)
\method{plot}{cubvcdf}(x, plot.3d=FALSE, \dots, xlim, ylim, n=20)

#normal
\method{plot}{nbvpdf}(x, plot.3d=FALSE, \dots, xlim, ylim, n=30, all=FALSE)
\method{plot}{nbvcdf}(x, plot.3d=FALSE, \dots, xlim, ylim, n=30)

#bimodal
\method{plot}{bmbvpdf}(x, plot.3d=FALSE, \dots, xlim, ylim, n=40, all=FALSE)
\method{plot}{bmbvcdf}(x, plot.3d=FALSE, \dots, xlim, ylim, n=40)

#dirichlet
\method{plot}{dtvpdf}(x, plot.3d=FALSE, \dots, n=30, log=FALSE)

#kernel density estimate
\method{plot}{kbvpdf}(x, plot.3d=FALSE, \dots, xlim, ylim, n=30,
    points=TRUE, point.color="#00000030")

#ecdf
\method{plot}{ebvcdf}(x, plot.3d=FALSE, \dots, as.smooth)

#additional plotting functions
#(called by the ebvcdf method, above)
plot_ebvcdf_step (x, plot.3d=FALSE, \dots,
    steps=TRUE, points=TRUE, point.color="#00000030")
plot_ebvcdf_smooth (x, plot.3d=FALSE, \dots,
    xlim, ylim, n=30)
}
\arguments{
\item{x}{A bv (bivariate) object.}
\item{plot.3d}{Logical value, if false, a 2D plot, if true, a 3D plot.}
\item{n}{Integer, the number of grid points in each x and y direction.\cr Note that in this package, "n", has more than one meaning.}
\item{xlim, ylim}{The x and y ranges for the plot.\cr Note that currently, the plot method for Poisson distributions, uses zero as the lower limits, regardless of the xlim and ylim values.}
\item{labels}{Logical value, if true, include numeric labels.}
\item{log}{Logical value, if true, plot the log density.}
\item{as.smooth}{Logical value, if true, plot the probability distribution like a (continuous) surface.\cr Defaults to false, if there's forty data points or less.}
\item{steps}{Logical value, if true, plot the steps.\cr Ignored if plot.3d is true.}
\item{points}{Logical value, if true, plot the data points.\cr Ignored if plot.3d is true.}
\item{point.color}{String, the color for data points.\cr Ignored if plot.3d is true.}
\item{all}{Logical value, if true, plot a two by two grid of both the PMF/PDF and CDF using both 2D and 3D plots.}
\item{\dots}{Other arguments for plotting functions in the barsurf package.}
}
\examples{
f <- dubvpmf (1, 1, 4, 4)

plot (f)
}
