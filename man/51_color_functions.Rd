\name{51_color_functions}
\alias{rim.litmus.fit}
\title{Color Functions}
\description{Predefined litmus-fitting functions for the bivariate package.}
\usage{
rim.litmus.fit (x)
}
\arguments{
\item{x}{A numeric vector.} 
}
\details{
The rim.litmus.fit function calls the rainbow.litmus.fit functions, from the barsurf package.\cr
Please refer to that package for details.

The rim litmus is for the (3d) rim of ECDFs.
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\examples{
f <- nbvpdf ()

s <- bvmat (f)
colf <- rim.litmus.fit (s@fv)

plot (colf)
}
