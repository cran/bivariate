\name{11_PD_uniform}
\alias{Uniform}
\alias{dubvpmf}
\alias{dubvcdf}
\alias{cubvpdf}
\alias{cubvcdf}
\title{Uniform Distributions}
\description{Bivariate uniform distributions, both discrete and continuous (on closed intervals).}
\usage{
#discrete
dubvpmf (
    a.X, b.X, #first variable
    a.Y, b.Y) #second variable
dubvcdf (
    a.X, b.X,
    a.Y, b.Y)

#continuous
cubvpdf (
    a.X, b.X,
    a.Y, b.Y)
cubvcdf (
    a.X, b.X,
    a.Y, b.Y)
}

\arguments{
\item{a.X, b.X}{Integers (in the discrete case) or numeric values (in the continuous case), giving the min/max possible values of X.}
\item{a.Y, b.Y}{Integers (in the discrete case) or numeric values (in the continuous case), giving the min/max possible values of Y.}
}
\value{
Self-referencing function objects.

Refer to \link{Mockup Function Objects}.
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\seealso{
\link{Binomial}, \link{Poisson} and \link{Categorical}\cr
For other probability distributions of discrete random variables.

\link{Normal}, \link{Bimodal}, \link{Dirichlet} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.

\link{Main Plotting Functions}

\link{Density Matrices}
}
\examples{
f <- dubvpmf (
    1, 4, #first variable
    1, 4) #second varaible

1 / (4 * 4)

plot (f)
f (1, 1)
}
