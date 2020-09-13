\name{12_PD_binomial}
\alias{Binomial}
\alias{bnbvpmf}
\alias{bnbvcdf}
\title{Binomial Distributions}
\description{Bivariate binomial distributions.}
\usage{
bnbvpmf (p.X, p.Y, n=1)
bnbvcdf (p.X, p.Y, n=1)
}
\arguments{
\item{p.X, p.Y}{Numeric values (between zero and one), giving the probabilities of the first and second events.}
\item{n}{Positive integer, giving the number of trials.}
}
\value{
Self-referencing function objects.

Refer to \link{Mockup Function Objects}.
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\seealso{
\link{Uniform}\cr
For uniform distributions.

\link{Poisson} and \link{Categorical}\cr
For other probability distributions of discrete random variables.

\link{Normal}, \link{Bimodal}, \link{Dirichlet} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.

\link{Main Plotting Functions}

\link{Density Matrices}
}
\examples{
f <- bnbvpmf (0.5, 0.5, 10)

plot (f)
f (5, 5)
}
