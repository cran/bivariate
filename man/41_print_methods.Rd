\name{41_print_methods}
\alias{bv.printf.BV}
\alias{bv.printf.TV}
\title{Print and Plot Methods}
\description{Print methods for bv (bivariate) and tv (trivariate) objects.}
\usage{
\method{bv.printf}{BV}(sf, \dots)
\method{bv.printf}{TV}(sf, \dots)
}
\arguments{
\item{sf}{A bv or tv object.}
\item{\dots}{Ignored.}
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\examples{
f <- nbvpdf ()

print (f)
plot (f)
}
