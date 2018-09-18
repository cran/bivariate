\name{returned.functions}
\alias{dubvpmf.f}
\alias{dubvcdf.f}
\alias{bnbvpmf.f}
\alias{bnbvcdf.f}
\alias{pbvpmf.f}
\alias{pbvcdf.f}
\alias{cubvpdf.f}
\alias{cubvcdf.f}
\alias{nbvpdf.f}
\alias{nbvcdf.f}
\alias{bmbvpdf.f}
\alias{bmbvcdf.f}
\title{Returned Functions}
\description{Returned functions. Note that you can name your functions whatever you like. (In the vignette I've named them f and F). Also note that you don't call these functions, rather call dubvpmf(), dubvcdf(), etc and then call the functions that are returned.}
\usage{
dubvpmf.f (x, y)
dubvcdf.f (x, y)
bnbvpmf.f (x, y)
bnbvcdf.f (x, y)
pbvpmf.f (x, y)
pbvcdf.f (x, y)
cubvpdf.f (x, y)
cubvcdf.f (x, y)
nbvpdf.f (x, y)
nbvcdf.f (x, y)
bmbvpdf.f (x, y)
bmbvcdf.f (x, y)
}
\arguments{
\item{x}{A vector of x values.}
\item{y}{A vector of y values.}
}
\examples{
#construct and evaluate a discrete uniform distribution
dubvpmf.f = dubvpmf (0, 1, 0, 1)
dubvpmf.f (0.5, 0.5)
}
