\name{18_PD_kernel_and_empirical}
\alias{Nonparametric}
\alias{kbvpdf}
\alias{ebvcdf}
\title{Nonparametric Distributions}
\description{Bivariate kernel density estimates and bivariate empirical cumulative distribution functions.}
\usage{
#kernel density estimates
kbvpdf (x, y, xbw, ybw, ..., xsmoothness=1, ysmoothness=1, data)

#ecdf
ebvcdf (x, y, ..., data)
}
\arguments{
\item{x, y}{
	Equal length numeric vectors, of x and y values.\cr
	Ignored, if data is provided.}
\item{xbw, ybw}{Optional numeric values, giving the x and y bandwidths.}
\item{xsmoothness, ysmoothness}{
	Numeric scalars, giving the relative smoothness.\cr
	(Initial default bandwidth parameters are multiplied by the smoothness parameters).\cr
	Ignored, if the corresponding bandwidth parameters are provided.}
\item{data}{A two-column data.frame object.}
\item{\dots}{Ignored.}
}
\details{
Default xlab and ylab labels are taken for deparsed x and y names.\cr
(Unless data is provided, in which case, column names are used).

Bandwidth parameters are optional, however, the default values are likely to be sub-optimal.

Kernel density estimates use KernSmooth::bkde2D.
}
\value{
Self-referencing S4-based function objects.

Refer to \link{Function Objects}.

Note that you can't evaluate the function representing kernel density estimates.\cr
(However, the bvmat function can be used to compute density matrices).
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.

Note that the probhat package provides more tools for kernel smoothing.}
\seealso{
\link{Uniform}\cr
For uniform distributions.

\link{Binomial}, \link{Poisson} and \link{Categorical}\cr
For other probability distributions of discrete random variables.

\link{Normal}, \link{Bimodal} and \link{Dirichlet}\cr
For other probability distributions of continuous random variables.

\link{Main Plotting Functions}

\link{Density Matrices}
}
\examples{
########################################
#kernel density estimates
########################################
data ("geyser", package="MASS")
attach (geyser)

#adapted from the KernSmooth package
fh <- kbvpdf (duration, waiting, 0.7, 7)

plot (fh,, TRUE)
plot (fh, TRUE, z.axis=TRUE, ref.arrows=FALSE)

detach (geyser)

########################################
#ECDF
########################################
attach (trees)

Fh <- ebvcdf (Height, Volume)

plot (Fh)
plot (Fh, FALSE)

Fh (median (Height), mean (Volume) )

detach (trees)
}
