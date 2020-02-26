\name{01_uniform}
\alias{dubvpmf}
\alias{dubvcdf}
\alias{cubvpdf}
\alias{cubvcdf}
\title{Uniform Distributions}
\description{Bivariate uniform distributions, both discrete and continuous.}
\usage{
#discrete
dubvpmf (a.X, a.Y, b.X, b.Y)
dubvcdf (a.X, a.Y, b.X, b.Y)

#continuous
cubvpdf (a.X, a.Y, b.X, b.Y)
cubvcdf (a.X, a.Y, b.X, b.Y)
}
\value{
Self-referencing functions of the form:

function (x, y) \{\dots\}

Where x and y are integer-valued vectors (in the discrete case) or numeric vectors (in the continuous case).
}
\arguments{
\item{a.X, a.Y}{Integers (in the discrete case) or numeric values (in the continuous case), giving the lower bounds of X and Y.}
\item{b.X, b.Y}{Integers (in the discrete case) or numeric values (in the continuous case), giving the upper bounds of X and Y.}

}
\examples{
f <- dubvpmf (1, 1, 4, 4)

f (1, 1)
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}

