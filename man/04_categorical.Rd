\name{04_categorical}
\alias{cbvpmf}
\title{Categorical Distributions}
\description{Bivariate categorical distributions.}
\details{
Note that sometimes, text labels may be missing.

This is likely to be fixed in the near future.
}
\usage{
cbvpmf (p)
}
\value{
Self-referencing functions of the form:

function (x, y) \{\dots\}

Where x and y are integer-valued or character vectors.
}
\arguments{
\item{p}{Numeric matrix of probabilities (or frequencies), preferably with row and column names.\cr Increasing rows correspond to increasing x values and increasing columns correspond to increasing y values.}
}
\examples{
h <- sample (1:24)
h <- matrix (h, 6, 4)
rownames (h) <- LETTERS [1:6]
colnames (h) <- letters [1:4]

f <- cbvpmf (h)

f (1, 2)
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
