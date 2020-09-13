\name{42_random_numbers}
\alias{Random Numbers}
\alias{bvrng.nbvpdf}
\alias{bvrng.ntvpdf}
\title{Random Numbers}
\description{Generate random numbers from bivariate and trivariate normal distributions.}
\usage{
\method{bvrng}{nbvpdf}(f, n=1, \dots)
\method{bvrng}{ntvpdf}(f, n=1, \dots)
}
\arguments{
\item{f}{
	A \link{nbvpdf} (bivariate normal) or \link{ntvpdf} (trivariate normal) object.}
\item{n}{Integer, the number of random numbers.}
\item{\dots}{Ignored.}
}
\details{
These functions call the rmvnorm function from the mvtnorm package.
}
\value{
A two-column or three-column numeric matrix.
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\seealso{
\link{Normal}\cr
For bivarate and triviate normal distributions.
}
\examples{
f <- nbvpdf ()

bvrng (f, 30)
}
