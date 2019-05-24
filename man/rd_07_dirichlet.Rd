\name{rd_07_dirichlet}
\alias{dtvpdf}
\title{Trivariate Dirichlet Distributions}
\description{
Trivariate Dirichlet distributions.

Refer to the vignette for better examples.}
\usage{
dtvpdf (alpha.1, alpha.2, alpha.3, tol=0.001)
}
\value{
Returns a function of the form:

function (x1, x2, x3, log=FALSE) = \{...\}

Where x1, x2 and x3 are numeric vectors.
}
\details{
The alpha parameters should be greater than 0.

The x values should sum to approximately 1 (with an error defined by tol) and be in the interval (0, 1).
}
\arguments{
\item{alpha.1}{Numeric scalar, giving the first alpha parameter.}
\item{alpha.2}{Numeric scalar, giving the second alpha parameter.}
\item{alpha.3}{Numeric scalar, giving the third alpha parameter.}
\item{tol}{.}
}
\examples{
f = dtvpdf (1, 1, 1)

f (0.3, 0.3, 0.4)
}
