\name{15_PD_normal}
\alias{Normal}
\alias{nbvpdf}
\alias{nbvpdf.2}
\alias{nbvcdf}
\alias{nbvcdf.2}
\alias{ntvpdf}
\alias{ntvpdf.2}
\title{Normal Distributions}
\description{Bivariate and trivariate normal distributions.}
\usage{
#normal bivariate (PDF and CDF), sd/cor
nbvpdf (mean.X=0, mean.Y=0, sd.X=1, sd.Y=1, cor=0)
nbvcdf (mean.X=0, mean.Y=0, sd.X=1, sd.Y=1, cor=0)

#normal bivariate (PDF and CDF), var/cov
nbvpdf.2 (mean.X=0, mean.Y=0, var.X=1, var.Y=1, cov=0)
nbvcdf.2 (mean.X=0, mean.Y=0, var.X=1, var.Y=1, cov=0)

#normal trivariate (PDF only)
ntvpdf (mean.X=0, mean.Y=0, mean.Z=0,
    sd.X=1, sd.Y=1, sd.Z=1,
    cor.XY=0, cor.XZ=0, cor.YZ=0)
ntvpdf.2 (mean.X=0, mean.Y=0, mean.Z=0,
    var.X=1, var.Y=1, var.Z=1,
    cov.XY=0, cov.XZ=0, cov.YZ=0)
}

\arguments{
\item{mean.X, mean.Y, mean.Z}{Numeric values, giving the means of X, Y and Z.}
\item{sd.X, sd.Y, sd.Z}{Positive numeric values, giving the standard deviations of X, Y and Z.}
\item{var.X, var.Y, var.Z}{Positive numeric values, giving the variances of X, Y and Z.}
\item{cor}{Numeric value, giving the correlation between X and Y.}
\item{cov}{Numeric value, giving the covariance between X and Y.}
\item{cor.XY, cor.XZ, cor.YZ}{Numeric values, giving the pairwise correlations between X, Y and Z.}
\item{cov.XY, cov.XZ, cov.YZ}{Numeric values, giving the pairwise covariances between X, Y and Z.}
}
\details{
Note that the resulting covariance matrix needs to be a positive definite matrix.\cr
Also, in the bivariate case, both sd/variance parameters need to be positive, and the absolute correlation needs to be less than one.

Note that the misc3d package needs to be installed and loaded, in order to plot the trivariate normal distribution.
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

\link{Bimodal}, \link{Dirichlet} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.

\link{Main Plotting Functions}

\link{Random Numbers}\cr
For bivariate and trivariate normal random numbers.

\link{Density Matrices}
}
\examples{
f <- nbvpdf ()

plot (f)
f (0, 0)
}
