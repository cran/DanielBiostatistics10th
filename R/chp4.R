




#' @title Chapter 4
#' 
#' @description 
#' 
#' Functions for Chapter 4, \emph{Probability Distributions} of Wayne W. Daniel's 
#' \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' 
#' @param size,prob see \code{\link[stats]{dbinom}}
#' 
#' @param lambda see \code{\link[stats]{dpois}}
#' 
#' @param xlim length-2 \link[base]{numeric} vector, horizontal limit of the figure.
#' 
#' @return 
#' 
#' \code{\link{binomBar}} generates a bar plot of a Binomial distribution.
#' 
#' \code{\link{poisBar}} generates a bar plot of a Poisson distribution.
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @example inst/example/chp4.R  

#' @name Chp4
#' @export
binomBar <- function(size, prob, xlim = size) {
  if (!is.integer(size) || length(size) != 1L) stop('size must be len-1 integer')
  if (!is.numeric(prob) || length(prob) != 1L || is.na(prob) || prob < 0 || prob > 1) stop('prob must be between 0,1')
  if (!is.integer(xlim)) stop('xlim must be integer')
  nl <- length(xlim)
  xl <- if (nl == 1L) 0:xlim else if (nl == 2L) xlim[1L]:xlim[2L] else stop('illegal xlim')
  pr <- dbinom(setNames(nm = xl), size = size, prob = prob)
  barplot(pr, main = sprintf(fmt = 'Binomial Distribution: n = %d; p = %.1f', size, prob), ylab = 'Density')
  return(invisible())
}

#' @rdname Chp4
#' @export
poisBar <- function(lambda, xlim) {
  if (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda) || lambda < 0) stop('lambda must be len-1 positive numeric')
  if (!is.integer(xlim)) stop('xlim must be integer')
  nl <- length(xlim)
  xl <- if (nl == 1L) 0:xlim else if (nl == 2L) xlim[1L]:xlim[2L] else stop('illegal xlim')
  pr <- dpois(setNames(nm = xl), lambda = lambda)
  barplot(pr, main = sprintf(fmt = 'Poisson Distribution: lambda = %.1f', lambda), ylab = 'Density')
  return(invisible())
}
