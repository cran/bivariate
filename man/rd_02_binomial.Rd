\name{rd_02_binomial}
\alias{bnbvpmf}
\alias{bnbvcdf}
\title{Binomial Distributions}
\description{Binomial distributions.

Refer to the vignette for better examples.}
\usage{
bnbvpmf (p.X, p.Y, n=1)
bnbvcdf (p.X, p.Y, n=1)
}
\value{
Returns functions of the form:

function (x, y) = \{...\}

Where x and y are integers.
}
\arguments{
\item{p.X}{Numeric scalar, giving the probability of the first event.}
\item{p.Y}{Numeric scalar, giving the probability of the second event.}
\item{n}{Integer scalar, giving the number of trials.}
}
\examples{
f = bnbvpmf (0.5, 0.5, 10)

f (4, 4)
}
