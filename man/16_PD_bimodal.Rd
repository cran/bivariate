\name{16_PD_bimodal}
\alias{Bimodal}
\alias{bmbvpdf}
\alias{bmbvpdf.2}
\alias{bmbvcdf}
\alias{bmbvcdf.2}
\title{Bimodal Distributions}
\description{Bivariate bimodal distributions.}
\usage{
bmbvpdf (
    mean.X1, mean.Y1, sd.X1, sd.Y1,
    mean.X2, mean.Y2, sd.X2, sd.Y2)
bmbvcdf (
    mean.X1, mean.Y1, sd.X1, sd.Y1,
    mean.X2, mean.Y2, sd.X2, sd.Y2)

bmbvpdf.2 (
    mean.X1, mean.Y1, var.X1, var.Y1,
    mean.X2, mean.Y2, var.X2, var.Y2)
bmbvcdf.2 (
    mean.X1, mean.Y1, var.X1, var.Y1,
    mean.X2, mean.Y2, var.X2, var.Y2)
}
\arguments{
\item{mean.X1, mean.Y1}{Numeric values, giving the means of the first X and Y components.}
\item{sd.X1, sd.Y1}{Positive numeric values, giving the standard deviations of the first X and Y components.}
\item{var.X1, var.Y1}{Positive numeric values, giving the variances of the first X and Y components.}
\item{mean.X2, mean.Y2}{Numeric values, giving the means of the second X and Y components.}
\item{sd.X2, sd.Y2}{Positive numeric values, giving the standard deviations of the second X and Y components.}
\item{var.X2, var.Y2}{Positive numeric values, giving the variances of the second X and Y components.}
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

\link{Normal}, \link{Dirichlet} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.

\link{Main Plotting Functions}

\link{Density Matrices}
}
\examples{
f <- bmbvpdf (3.5, 0, 1, 1, 6.5, 0, 1, 1)

plot (f, axes = c (TRUE, FALSE), ref.arrows=FALSE, xat = c (3.5, 6.5) )
f (3.5, 0)
}
