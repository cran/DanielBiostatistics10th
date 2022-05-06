
#' @title Chapter 7
#' 
#' @description 
#' 
#' Functions for Chapter 7, \emph{Hypothesis Testing} of Wayne W. Daniel's 
#' \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' 
#' @param mean1 \link[base]{numeric} vector, parameter(s) \eqn{\mu_1} in the alternative hypothesis
#' 
#' @param mean0 \link[base]{numeric} scalar, parameter \eqn{\mu_0} in the null hypothesis
#' 
#' @param sd \link[base]{numeric} scalar, population standard deviation
#' 
#' @param n \link[base]{integer} scalar, the sample size
#' 
#' @param alternative \link[base]{character} scalar
#' 
#' @param sig.level significance level (Type-I-error probability), nomenclature follows \code{\link[stats]{power.t.test}}, default \eqn{.05}.
#' 
#' @return 
#' Function \code{power_z} returns the powers at each of the alternative parameters \eqn{\mu_1}, for hypothesis testing 
#' \itemize{
#' \item{\eqn{H_0: \mu = \mu_0} vs. \eqn{H_A: \mu \neq \mu_0}, if \code{alternative = 'two.sided'}}
#' \item{\eqn{H_0: \mu \leq \mu_0} vs. \eqn{H_A: \mu > \mu_0}, if \code{alternative = 'greater'}}
#' \item{\eqn{H_0: \mu \geq \mu_0} vs. \eqn{H_A: \mu < \mu_0}, if \code{alternative = 'less'}}
#' }
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @example inst/example/chp7.power.R 
#' 
#' @name Chp7power
#' @export
power_z <- function(mean1, mean0, sd, n, alternative = c('two.sided', 'less', 'greater'), sig.level = .05) {
  alternative <- match.arg(alternative) # make sure `alternative` is one of c('two.sided', 'less', 'greater')
  stderr <- sd / sqrt(n)
  switch(alternative, 'two.sided' = {
    rr <- qnorm(p = c(sig.level/2, 1-sig.level/2), mean = mean0, sd = stderr) # 2-sided rejection region
    message(sprintf(fmt = 'Two-sided Rejection Region: (-\u221E, %.3f) \u222A (%.3f, \u221E)', rr[1L], rr[2L]))
    return(pnorm(q = rr[1L], mean = mean1, sd = stderr, lower.tail = TRUE) + # P(White), in left-RR
             pnorm(q = rr[2L], mean = mean1, sd = stderr, lower.tail = FALSE)) # P(White), in Right-RR  
  }, 'less' = {
    # upper-bound of 1-sided rejection region: RR has cumulative prob (from left) of `sig.level`
    upr <- qnorm(p = sig.level, mean = mean0, sd = stderr, lower.tail = TRUE) # to find the cut-off value, so that P(X_bar < upr) = sig.level
    message(sprintf('One-sided Rejection Region: (-\u221E, %.3f)', upr))
    return(pnorm(q = upr, mean = mean1, sd = stderr, lower.tail = TRUE)) # one-sided P(White)
  }, 'greater' = {
    # lower-bound of 1-sided rejection region: RR has cumulative prob (from right) of `sig.level`
    lwr <- qnorm(p = sig.level, mean = mean0, sd = stderr, lower.tail = FALSE) # to find the cut-off value, so that P(X_bar > lwr) = sig.level
    message(sprintf('One-sided Rejection Region: (%.3f, \u221E)', lwr))
    return(pnorm(q = lwr, mean = mean1, sd = stderr, lower.tail = FALSE)) # one-sided P(White)
  })
}




