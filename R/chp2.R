

#' @title Chapter 2
#' 
#' @description 
#' 
#' Functions and examples for Chapter 2, \emph{Descriptive Statistics}.
#' 
#' @param x \link[base]{numeric} vector, the observations. 
#' In \link{print_freqs} function, this argument can also be a \link[base]{factor}
#' 
#' @param breaks,include.lowest,right see \link[base]{cut.default}
#' 
#' @param na.rm \link[base]{logical} scalar, whether to remove the missing observations (default \code{TRUE})
#' 
#' @details 
#' 
#' \link{print_freqs} prints the (relative) frequencies and cumulative (relative) frequencies, from 
#' a numeric input vector, specified interval breaks as well as open/close status of the ends of the intervals.
#' 
#' \link{print_stats} prints the simple statistics of the input observations, such as sample size,
#' mean, median, (smallest) mode, variance, standard deviation, 
#' coefficient of variation (if all observations are non-negative),
#' quartiles, inter-quartile range (IQR), range, skewness and kurtosis.  A histogram is also printed. 
#' 
#' @return 
#' 
#' \link{print_freqs} returns a \link[base]{noquote} \link[base]{matrix}.
#' 
#' \link{print_stats} does not have a returned value.
#' 
#' @seealso \link[base]{cut.default} \link[base]{table} \link[base]{cumsum}
#' \link[base]{mean.default} \link[stats]{median.default} \link[pracma]{Mode} 
#' \link[stats]{var} \link[stats]{sd} \link[stats]{quantile}
#' \link[e1071]{skewness} \link[e1071]{kurtosis}
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @example inst/example/Chapter2.R
#' 
#' @name Chapter02
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


#' @rdname Chapter02
#' @export
print_freqs <- function(x, breaks, include.lowest = FALSE, right = TRUE) {
  object <- if (is.factor(x)) x else {
    cut.default(x, breaks = breaks, include.lowest = include.lowest, right = right, ordered_result = TRUE)
  }
  tab <- table(object)
  ctab <- cumsum(tab)
  n <- sum(tab)
  ret <- cbind(
    'Frequency (%)' = sprintf(fmt = '%d (%.2f%%)', tab, 100 * tab/n), 
    'Cummulative Freq (%)' = sprintf(fmt = '%d (%.2f%%)', ctab, 100 * ctab/n)
  )
  rownames(ret) <- names(tab)
  print.noquote(noquote(ret, right = TRUE))
  return(invisible(list(
    freq = tab, n = n
  )))
}

