\name{09_print_method}
\alias{print.bv}
\title{Print Method}
\description{Print method for bv (bivariate) objects.}
\usage{
\method{print}{bv}(x, \dots)
}
\details{This method calls the intoo::object.summary function.}
\arguments{
\item{x}{A bv object.}
\item{\dots}{Other arguments for intoo::object.summary.}
}
\examples{
f <- dubvpmf (1, 1, 4, 4)

print (f)
}
