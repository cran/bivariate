\name{02_binomial}
\alias{bnbvpmf}
\alias{bnbvcdf}
\title{Binomial Distributions}
\description{Bivariate binomial distributions.}
\usage{
bnbvpmf (p.X, p.Y, n=1)
bnbvcdf (p.X, p.Y, n=1)
}
\value{
Self-referencing functions of the form:

function (x, y) \{\dots\}

Where x and y are integer-valued vectors.
}
\arguments{
\item{p.X, p.Y}{Numeric values (between zero and one), giving the probabilities of the first and second events.}
\item{n}{Integer, giving the number of trials.}
}
\examples{
f <- bnbvpmf (0.5, 0.5, 10)

f (4, 4)
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
