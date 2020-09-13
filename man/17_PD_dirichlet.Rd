\name{17_PD_dirichlet}
\alias{Dirichlet}
\alias{dtvpdf}
\title{Trivariate Dirichlet Distributions}
\description{Trivariate Dirichlet distributions.}
\usage{
dtvpdf (alpha.X, alpha.Y, alpha.Z)
}
\arguments{
\item{alpha.X, alpha.Y, alpha.Z}{Positive numeric values, giving the alpha parameters.}
}
\value{
Self-referencing function objects.

Refer to \link{Mockup Function Objects}.
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\seealso{
\link{Uniform}\cr
For uniform distributions.

\link{Binomial}, \link{Poisson} and \link{Categorical}\cr
For other probability distributions of discrete random variables.

\link{Normal}, \link{Bimodal} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.

\link{Main Plotting Functions}

\link{Density Matrices}
}
\examples{
f <- dtvpdf (1.05, 1.05, 1.05)

plot (f)
f (0.3, 0.3, 0.4)
}
