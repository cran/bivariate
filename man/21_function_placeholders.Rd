\name{21_function_placeholders}
\alias{Function Objects}
\alias{PMF-class}
\alias{PDF-class}
\alias{CDF-class}
\alias{BV-class}
\alias{TV-class}
\alias{DBV-class}
\alias{CBV-class}
\alias{SBV-class}
\alias{CTV-class}
\alias{DUBV-class}
\alias{BNBV-class}
\alias{PBV-class}
\alias{GBV-class}
\alias{CUBV-class}
\alias{NBV-class}
\alias{NTV-class}
\alias{BMBV-class}
\alias{DTV-class}
\alias{KBV-class}
\alias{EBV-class}
\alias{DUBVPMF-class}
\alias{DUBVCDF-class}
\alias{BNBVPMF-class}
\alias{BNBVCDF-class}
\alias{PBVPMF-class}
\alias{PBVCDF-class}
\alias{GBVPMF-class}
\alias{CUBVPDF-class}
\alias{CUBVCDF-class}
\alias{NBVPDF-class}
\alias{NBVCDF-class}
\alias{NTVPDF-class}
\alias{NTVCDF-class}
\alias{BMBVPDF-class}
\alias{BMBVCDF-class}
\alias{DTVPDF-class}
\alias{KBVPDF-class}
\alias{EBVCDF-class}
\alias{fobj.f}
\alias{fobj.F}
\alias{fobj.gf}
\alias{fobj.f3}
\alias{fobj.wf}
\alias{fobj.fh}
\alias{fobj.Fh}
\title{Function Objects}
\description{
Hard-coded functions, representing (runtime) function objects.

DO NOT CALL THESE FUNCTIONS.\cr
CALL A CONSTRUCTOR, WHICH SHOULD RETURN A FUNCTION OBJECT.}
\usage{
#PMF/PDF and CDF
#(all distributions, except the exceptions below)
fobj.f (x, y)
fobj.F (x, y)

#Categorical PMF
fobj.gf (a, b)

#Trivariate Normal PDF
fobj.f3 (x, y, z)

#Dirichlet PDF
fobj.wf (x, y, z = 1 - x - y, \dots, log=FALSE)

#ECDF 
fobj.Fh (x, y)
}
\arguments{
\item{x, y, z}{
	Vectors giving the evaluation bins/points.\cr
	In discrete distributions, they should be integers.\cr
	In continuous distributions they should be numerics.\cr
	Currently, missing values, non-finite values and zero-length vectors are not allowed.}
\item{a, b}{
	Integer or character vectors, giving the evaluation bins.\cr
	(Same as x and y above, except that strings are allowed).}
\item{log}{Logical, if true, return log-transformed densities.}
\item{\dots}{Ignored.}
}
\details{
DO NO CALL THESE FUNCTIONS.\cr
CALL A CONSTRUCTOR, WHICH SHOULD RETURN A FUNCTION OBJECT.

Uniform and binomial distributions allow evaluation values outside the supported region.\cr
(PMFs/PDFs return zero mass/density, and CDFs return the corresponding values).

Poisson evaluation values need to be non-negative.

Categorical string evaluation values need to match group names, taken from the parameter matrix.

Categorical integer evaluation values, need to have:\cr
x in [1, ngx], where ngx is the number of x groups (and rows of the parameter matrix).\cr
y in [1, ngy], where ngy is the number of y groups (and columns of the parameter matrix).
 
In Dirichlet distributions, z is optional.\cr
And the three variables should be in the interval (0, 1) and sum to one.

Note that kbvpdf objects (often labelled fh), can't be evaluated.\cr
(However, the bvmat function can be used to compute density matrices).

Also note that the help pages, linked to in the see also section, have better examples.
}
\value{
PMFs return a numeric vector, giving mass.\cr
P (X = x, Y = y)

PDFs return a numeric vector, giving density.

CDFs return a numeric vector, giving cumulative probability.\cr
P (X <= x, Y <= y)
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
\seealso{
\link{Uniform}\cr
For uniform distributions.

\link{Binomial}, \link{Poisson} and \link{Categorical}\cr
For other probability distributions of discrete random variables.

\link{Normal}, \link{Bimodal}, \link{Dirichlet} and \link{Nonparametric}\cr
For other probability distributions of continuous random variables.
}
\examples{
f <- nbvpdf ()

x <- -3:3
y <- 0
density <- f (x, y)

cbind (x, y, density)
}
