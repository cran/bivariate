\name{returned.functions}
\alias{nbpdf.f}
\alias{nbcdf.f}
\title{Returned Functions}
\description{Returned functions. Note that you can name your functions whatever you like. (In the vignette I've named them f and F). Also note that you don't call these functions, rather call nbpdf() or nbcdf() and then call the functions that are returned.}
\seealso{\link{nbpdf}, \link{nbcdf}}
\usage{
nbpdf.f (x, y)
nbcdf.f (x, y)
}
\arguments{
\item{x}{A vector of x values.}
\item{y}{A vector of y values.}
}
\examples{
#construct and evaluate a bivariate probability distribution
nbpdf.f = nbpdf (0, 0, 1, 1, 0)
nbpdf.f (0, 0)
}
