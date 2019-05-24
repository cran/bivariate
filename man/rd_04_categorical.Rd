\name{rd_04_categorical}
\alias{cbvpmf}
\title{Categorical Distributions}
\description{
Categorical distributions.

Refer to the vignette for better examples.
}
\usage{
cbvpmf (z)
}
\value{
Returns a function of the form:

function (x, y) = \{...\}

Where x and y are integers or strings.
}
\arguments{
\item{z}{Numeric matrix of probabilities or frequencies, preferably with row and column names.}
}
\examples{
z = matrix (sample (1:16), 4, 4)
f = cbvpmf (z)

f (1, 1)
}
