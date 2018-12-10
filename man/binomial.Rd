\name{binomial}
\alias{bnbvpmf}
\alias{bnbvcdf}
\title{Binomial Distributions}
\description{Binomial distributions. Refer to the vignette for better examples.}
\usage{
bnbvpmf (n, p.X, p.Y)
bnbvcdf (n, p.X, p.Y)
}
\value{These functions return functions.}
\seealso{\link{bnbvpmf.f}, \link{bnbvcdf.f}}
\arguments{
\item{n}{Integer scalar, giving the number of trials.}
\item{p.X}{Numeric scalar, giving the probability of the first event.}
\item{p.Y}{Numeric scalar, giving the probability of the second event.}
}
\examples{
#construct some binomial distributions
bnbvpmf.f = bnbvpmf (10, 0.5, 0.5)
bnbvcdf.f = bnbvcdf (10, 0.5, 0.5)
}
