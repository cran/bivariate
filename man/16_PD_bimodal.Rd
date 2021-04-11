\name{16_PD_bimodal}
\alias{Bimodal}
\alias{bmbvpdf}
\alias{bmbvpdf.2}
\alias{bmbvcdf}
\alias{bmbvcdf.2}
\title{Bimodal Distributions}
\description{
Bivariate bimodal distributions.

NOTE THAT THE ORDER OR THE FUNCTION ARGUMENTS HAS CHANGED.\cr
(In version 0.7.x)
}
\usage{
bmbvpdf (
    mean.X1=0, mean.X2=0,
    mean.Y1=0, mean.Y2=0,
      sd.X1=1,   sd.X2=1,
      sd.Y1=1,   sd.Y2=1)
bmbvcdf (
    mean.X1=0, mean.X2=0,
    mean.Y1=0, mean.Y2=0,
      sd.X1=1,   sd.X2=1,
      sd.Y1=1,   sd.Y2=1)

bmbvpdf.2 (
    mean.X1=0, mean.X2=0,
    mean.Y1=0, mean.Y2=0,
     var.X1=1,  var.X2=1,
     var.Y1=1,  var.Y2=1)
bmbvcdf.2 (
    mean.X1=0, mean.X2=0,
    mean.Y1=0, mean.Y2=0,
     var.X1=1,  var.X2=1,
     var.Y1=1,  var.Y2=1)
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
Self-referencing S4-based function objects.

Refer to \link{Function Objects}.
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
f <- bmbvpdf (3.5, 6.5)

plot (f, axes = c (TRUE, FALSE), ref.arrows=FALSE, xat = c (3.5, 6.5) )
f (3.5, 0)
}
