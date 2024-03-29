% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/chp4.R
\name{binom2pois}
\alias{binom2pois}
\title{Binomial Approaching Poisson}
\usage{
binom2pois(x, lambda, size = c(10L, 100L))
}
\arguments{
\item{x}{\link[base]{integer} scalar, observed number of responses}

\item{lambda}{positive \link[base]{numeric} scalar, parameter \eqn{\lambda} of Poisson distribution}

\item{size}{\link[base]{integer} vector, parameter \eqn{n} of binomial distribution}
}
\value{
\code{\link[=binom2pois]{binom2pois()}} returns a \code{'binom2pois'} object, for which
a \link[base]{print} method, an \link[ggplot2]{autolayer} and an \link[ggplot2]{autoplot} method are defined.
}
\description{
Binomial Approaching Poisson
}
\details{
\code{\link[=binom2pois]{binom2pois()}} shows how binomial density approaches Poisson density when
\eqn{n\rightarrow\infty} and \eqn{p\rightarrow 0}, while holding a constant product \eqn{np=\lambda}.
}
\examples{
binom2pois(x = 4L, lambda = 6, size = seq.int(10L, 50L, by = 10L))

}
\seealso{
\link[stats]{dbinom} \link[stats]{dpois}
}
