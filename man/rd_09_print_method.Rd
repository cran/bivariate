\name{rd_09_print_method}
\alias{print.bv}
\title{Print Method}
\description{Print method for bv (bivariate) objects.}
\usage{
\method{print}{bv}(x, \dots)
}
\details{
This method calls the object.summary() function from the intoo package.
}
\arguments{
\item{x}{A bv object.}
\item{\dots}{Other arguments.}
}
\examples{
f = dubvpmf (1, 4, 1, 4)
f
}
