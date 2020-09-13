\name{14_PD_categorical}
\alias{Categorical}
\alias{gbvpmf}
\title{Categorical Distributions}
\description{Bivariate categorical distributions.}
\usage{
gbvpmf (p)
}
\arguments{
\item{p}{Numeric matrix of probabilities (or frequencies), preferably with row and column names.}
}
\value{
Self-referencing function objects.

Refer to \link{Mockup Function Objects}.
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.

Note that the probhat package supports categorical distributions, and provides more tools.}
\seealso{
\link{Uniform}\cr
For uniform distributions.

\link{Binomial} and \link{Poisson}\cr
For other probability distributions of discrete random variables.

\link{Normal}, \link{Bimodal}, \link{Dirichlet} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.

\link{Main Plotting Functions}

\link{Density Matrices}
}
\examples{
library (barsurf)

h <- matrix (sample (1:24), 4, 6)
rownames (h) <- LETTERS [1:4]
colnames (h) <- letters [1:6]

f <- gbvpmf (h)

plot (f)
plot (f, FALSE)

h [2, 1:4] / sum (h)
f (2, 1:4)
f ("B", c ("a", "b", "c", "d") )
}
