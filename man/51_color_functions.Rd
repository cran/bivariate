\name{51_color_functions}
\alias{gpd.litmus.fit}
\alias{rim.litmus.fit}
\title{Color Functions}
\description{Predefined litmus-fitting functions for the bivariate package.}
\usage{
gpd.litmus.fit (x)
rim.litmus.fit (x)
}
\arguments{
\item{x}{A numeric vector.} 
}
\details{
The gpd.litmus.fit and rim.litmus.fit functions call the litmus.fit and rainbow.litmus.fit functions, from the barsurf package, respectively.\cr
Please refer to that package for details.

The rim litmus is for the (3d) rim of ECDFs.

The gpl litmus is for gold heatmaps, and is only used in the vignette.
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\examples{
f <- nbvpdf ()

s <- bvmat (f)
colf <- rim.litmus.fit (s$fv)

plot (colf)
}
