\name{13_PD_poisson}
\alias{Poisson}
\alias{pbvpmf}
\alias{pbvpmf.2}
\alias{pbvcdf}
\alias{pbvcdf.2}
\title{Poisson Distributions}
\description{Bivariate Poisson distributions.}
\usage{
pbvpmf (lambda.1, lambda.2, lambda.3)
pbvcdf (lambda.1, lambda.2, lambda.3)

pbvpmf.2 (mean.X, mean.Y, cov)
pbvcdf.2 (mean.X, mean.Y, cov)
}

\arguments{
\item{lambda.1, lambda.2, lambda.3}{Positive numeric values, giving the first, second and third lambda parameters.}
\item{mean.X, mean.Y}{Suitable numeric values, giving the mean of X and Y.\cr Note that their means equal their variances.}
\item{cov}{Suitable numeric value, giving the covariance of X and Y.}
}
\value{
Self-referencing S4-based function objects.

Refer to \link{Function Objects}.
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\seealso{
\link{Uniform}\cr
For uniform distributions.

\link{Binomial} and \link{Categorical}\cr
For other probability distributions of discrete random variables.

\link{Normal}, \link{Bimodal}, \link{Dirichlet} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.

\link{Main Plotting Functions}

\link{Density Matrices}
}
\examples{
f <- pbvpmf (10, 10, 0)

plot (f)
f (5, 5)
}
