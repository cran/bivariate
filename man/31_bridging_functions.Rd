\name{31_bridging_functions}
\alias{bv.printf}
\alias{bv.plotf}
\alias{print.BV}
\alias{print.TV}
\alias{plot.BV}
\alias{plot.TV}
\title{New Generic Functions}
\description{New generic functions for printing/plotting function objects.}
\usage{
bv.printf (\dots)
bv.plotf (\dots)

\method{print}{BV}(x, \dots)
\method{print}{TV}(x, \dots)
\method{plot}{BV}(x, \dots)
\method{plot}{TV}(x, \dots)
}
\arguments{
\item{x}{The first argument.} 
\item{\dots}{Arguments for specific methods.}
}
\details{
In general, do not call bv.printf and bv.plotf.\cr
Call print and plot instead.\cr
(These call bv.printf and bv.plotf).
}
\references{Refer to the vignette for an overview, references, theoretical background and better examples.}
