

#' @title Chapter 2
#' 
#' @description 
#' 
#' Functions for Chapter 2, \emph{Descriptive Statistics} of Wayne W. Daniel's 
#' \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' 
#' @param x \link[base]{numeric} vector, the observations
#' 
#' @param na.rm \link[base]{logical} scalar, whether to remove the missing observations (default \code{TRUE})
#' 
#' @return 
#' 
#' \code{\link{print_stats}} prints the simple statistics, such as 
#' mean, median, standard deviation, inter-quartile range (IQR), range, skewness and kurtosis
#' of the input observations \code{x}.  A histogram is also printed. No value is returned.
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' \url{https://en.wikipedia.org/wiki/Coefficient_of_variation}
#' 
#' @example inst/example/chp2.R
#' 
#' @name Chp2
#' @export
print_stats <- function(x, na.rm = TRUE) {
  nm <- deparse(substitute(x))
  cat('\nSummary Statistics of', sQuote(nm), '\n\n')
  cat(sprintf('Number of observations = %d\n', length(if (na.rm) x[!is.na(x)] else x)))
  cat(sprintf('mean = %.2f\n', mean.default(x, na.rm = na.rm)))
  cat(sprintf('median = %.2f\n', median(x, na.rm = na.rm)))
  cat(sprintf('(smallest) mode = %.2f\n', Mode(x)[1L]))
  cat(sprintf('variance = %.2f\n', var(x, na.rm = na.rm)))
  cat(sprintf('standard deviation = %.2f\n', sd(x, na.rm = na.rm)))
  if (all(x >= 0, na.rm = TRUE)) {
    cat(sprintf('coefficient of variation = %.1f%%\n', 1e2 * sd(x, na.rm = na.rm) / mean.default(x, na.rm = na.rm)))
  } else cat('(coefficient of variation only applicable to all-non-negative observations)\n')
  Q <- quantile(x, probs = c(.25, .5, .75), na.rm = na.rm)
  cat(sprintf('Quartiles: Q1 = %.1f, Q2 = %.1f, Q3 = %.1f\n', Q[1L], Q[2L], Q[3L]))
  cat(sprintf('IQR = %.1f\n', Q[3L] - Q[1L]))
  cat(sprintf('range = %.1f (%.1f ~ %.1f)\n', diff(range(x, na.rm = na.rm)), min(x, na.rm = na.rm), max(x, na.rm = na.rm)))
  cat(sprintf('skewness = %.3f\n', skewness(x, na.rm = na.rm)))
  cat(sprintf('kurtosis = %.3f\n', kurtosis(x, na.rm = na.rm)))
  cat('\n')
  plot(hist(x), main = paste('Histogram of', nm))
  return(invisible())
}

