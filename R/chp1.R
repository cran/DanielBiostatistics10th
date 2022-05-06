

#' @title Chapter 1
#' 
#' @description 
#' 
#' Functions for Chapter 1, \emph{Introduction to Biostatistics} of Wayne W. Daniel's 
#' \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' 
#' @param x a \link[base]{data.frame}
#' 
#' @param size positive \link[base]{integer} scalar, number of items to choose, see \code{\link[base]{sample.int}}
#' 
#' @param replace \link[base]{logical} scalar, whether sampling should be with replacement (default \code{FALSE}), see \code{\link[base]{sample.int}}
#' 
#' @param prob \link[base]{numeric} vector of probability weights for each row of input \code{x} being sampled, see \code{\link[base]{sample.int}}
#' 
#' @return 
#' 
#' \code{\link{sampleRow}} returns a \link[base]{data.frame}, a simple random sample of the input.
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @example inst/example/chp1.R
#' 
#' @name Chp1
#' @export
sampleRow <- function(x, size, replace = FALSE, prob = NULL) {
  if (!is.data.frame(x)) stop('input must be data.frame')
  if (!length(nr <- nrow(x))) stop('input data.frame cannot be of 0-row')
  message('A random sample of ', size, ' rows')
  x[sample.int(n = nr, size = size, replace = replace, prob = prob), ]
}

