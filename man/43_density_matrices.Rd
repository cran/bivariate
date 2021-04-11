\name{43_density_matrices}
\alias{Density Matrices}
\alias{PMatrix-class}
\alias{bvmat.DUBV}
\alias{bvmat.BNBV}
\alias{bvmat.PBV}
\alias{bvmat.CUBV}
\alias{bvmat.NBV}
\alias{bvmat.BMBV}
\alias{bvmat.GBV}
\alias{bvmat.DTV}
\alias{bvmat.KBV}
\alias{bvmat.EBV}
\alias{ebvmat_reg}
\alias{ebvmat_step}
\title{Density Matrices}
\description{Compute density (and probability) matrices.}
\usage{
########################################
#(du) discrete uniform 
#(bn) binomial
#(p)  poisson
########################################
\method{bvmat}{DUBV}(sf, xlim, ylim, \dots)
\method{bvmat}{BNBV}(sf, xlim, ylim, \dots)
\method{bvmat}{PBV}(sf, xlim, ylim, \dots)

########################################
#(cu) continuous uniform
#(n)  normal
#(bm) bimodal
########################################
\method{bvmat}{CUBV}(sf, xlim, ylim, \dots, n=10)
\method{bvmat}{NBV}(sf, xlim, ylim, \dots, n=10)
\method{bvmat}{BMBV}(sf, xlim, ylim, \dots, n=10)

########################################
#(g)  categorical
#(d)  dirichlet
#(k)  kernel
#(e)  empirical
########################################
\method{bvmat}{GBV}(sf, \dots)
\method{bvmat}{DTV}(sf, \dots, log=FALSE, n=10)
\method{bvmat}{KBV}(sf, xlim, ylim, \dots, n=10)
\method{bvmat}{EBV}(sf, \dots, reg=TRUE)

########################################
#additional functions
#(called by the ebv method, above)
########################################
ebvmat_reg (sf, xlim, ylim, \dots, n=10)
ebvmat_step (sf, \dots, extend=FALSE)
}
\arguments{
\item{sf}{A suitable function object.\cr
	Which here, represents one of the probability distributions from this package.\cr
	Refer to the see also section.}
\item{xlim, ylim}{A length-two vector for the evaluation window.\cr
	For discrete distributions, this should be an integer vector, and for continuous distributions, this should be a numeric vector.}
\item{n}{Integer vector of length one or two, number of grid points.\cr
	(Except for Dirichlet distributions where it needs to be scalar).}
\item{log}{Logical, if true, return log-transformed densities.}
\item{reg}{
	Logical, if true, evaluate the ECDF over a regularly-spaced grid.\cr
	In general, reg should be true, for large sample sizes.}
\item{extend}{Logical vector or length one or two, if true, compute a rim (of probabilities) outside the observed values.}
\item{\dots}{Ignored.}
}
\details{
These methods work for both PMFs/PDFs and CDFs, where applicable.

For example:\cr
If bvmat is called with a normal density function it will return densities (from the density function), however, if called with a normal distribution function it will return the probabilities (from the distribution function).

Note that the ebv (empirical) method calls either the ebvmat_reg or ebvmat_step function, depending on the reg argument.
}
\value{
An S4 object with:

An fv slot, which is a matrix, of densities or probabilities.

x and y slots, which are the evaluation points.\cr
(This length of x should equal the number of rows, and the length of y should equal the number of columns).

And exception to the above is ECDFs with reg=FALSE, where x and y are breakpoints.\cr
(The length of x should be equal to the number of rows plus one, and the length of y should be equal to the number of columns plus one).
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\seealso{
\link{Uniform}\cr
For uniform distributions.

\link{Binomial}, \link{Poisson} and \link{Categorical}\cr
For other probability distributions of discrete random variables.

\link{Normal}, \link{Bimodal}, \link{Dirichlet} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.

\link{Main Plotting Functions}
}
\examples{
data (geyser, package="MASS")
attach (geyser)

#adapted from the KernSmooth package
fh <- kbvpdf (duration, waiting, 0.7, 7)

bvmat (fh)

detach (geyser)
}
