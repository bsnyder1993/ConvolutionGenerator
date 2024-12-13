% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/unif.R
\name{unif}
\alias{unif}
\title{Uniform}
\usage{
unif(x, a, b)
}
\arguments{
\item{x}{// Domain vector}

\item{a}{// Lower bound scalar}

\item{b}{// Upper bound scalar}
}
\value{
returns a vector y
}
\description{
This function takes in a domain vector x, a lower bound a, and an upper bound b, and returns a
vector y equal to uniform(a,b) defined over x.
}
\examples{
# Creating a uniform(-.5,.5) vector over the domain (-2, 2)

x <- seq(-2, 2, by = .002)
y <- unif(x, -.5, .5)
}
