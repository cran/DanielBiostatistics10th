

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
#' @examples
#' # Page 8, Example 1.4.1
#' d141 = read.csv(system.file('extdata', 'EXA_C01_S04_01.csv', package = 'DanielBiostatistics10th'))
#' ?read.csv # invoke the help files of an R 'function'
#' class(d141) # 'data.frame'; most used R object to store a 'data'
#' dim(d141)
#' head(d141, n = 8L) # first `n` rows of a 'data.frame'
#' names(d141) # column names of a 'data.frame'
#' d141$AGE
#' sampleRow(d141, size = 10L, replace = FALSE)
#' 
#' # Page 11, Example 1.4.2 (systematic sample)
#' d141[seq.int(from = 4L, to = 166L, by = 18L), ]
#' 
#' @name Chp1
#' @export
sampleRow <- function(x, size, replace = FALSE, prob = NULL) {
  if (!is.data.frame(x)) stop('input must be data.frame')
  if (!length(nr <- nrow(x))) stop('input data.frame cannot be of 0-row')
  message('A random sample of ', size, ' rows')
  x[sample.int(n = nr, size = size, replace = replace, prob = prob), ]
}

