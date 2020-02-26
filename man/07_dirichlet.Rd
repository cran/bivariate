\name{07_dirichlet}
\alias{dtvpdf}
\title{Trivariate Dirichlet Distributions}
\description{Trivariate Dirichlet distributions.}
\usage{
dtvpdf (alpha.X, alpha.Y, alpha.Z)
}
\value{
Self-referencing functions of the form:

function (x, y, z, log=FALSE) \{\dots\}

Where x, y and z are numeric vectors, in the interval (0, 1), which sum (across) to one.

Note that a small error (<= 0.001) is allowed, given possible floating point errors.
}
\arguments{
\item{alpha.X, alpha.Y, alpha.Z}{Numeric values, giving the alpha parameters, greater than zero.}
}
\examples{
f <- dtvpdf (1, 1, 1)

f (0.3, 0.3, 0.4)
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
