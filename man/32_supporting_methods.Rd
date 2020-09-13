\name{32_supporting_methods}
\alias{print.bv}
\alias{print.tv}
\alias{plot.bv}
\alias{plot.tv}
\title{Print and Plot Methods}
\description{Print and plot methods for bv (bivariate) and tv (trivariate) objects.}
\usage{
\method{print}{bv}(x, \dots)
\method{print}{tv}(x, \dots)

\method{plot}{bv}(x, \dots)
\method{plot}{tv}(x, \dots)
}
\arguments{
\item{x}{A bv object.}
\item{\dots}{Other arguments for intoo::object.summary.}
}
\details{
The print methods call the intoo::object.summary function.

The plot methods call bvplot, which then call one of the bvplot methods.
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\examples{
f <- nbvpdf ()

print (f)
plot (f)
}
