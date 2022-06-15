



#' @title Chapter 2
#' 
#' @description 
#' 
#' Functions and examples for Chapter 2, \emph{Descriptive Statistics}.
#' 
#' @param x \link[base]{numeric} vector, the observations. 
#' In \link{print_freqs} function, this argument can also be a \link[base]{factor}
#' 
#' @param breaks,include.lowest,right see \link[base]{cut.default}. Note that we use default
#' \code{include.lowest = TRUE} in \link{print_freqs}
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
#' \link{print_freqs} returns a \linkS4class{freqs} object, for which
#' a \link[methods]{show} method, an \link[ggplot2]{autolayer} and an \link[ggplot2]{autoplot} method are defined.
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
  p <- ggplot() + geom_histogram(mapping = aes(x = x), bins = 30L, colour = 'white') + 
    labs(x = nm, y = NULL) +
    theme_bw()
  print(p)
  return(invisible())
}


#' @rdname Chapter02
#' @export
print_freqs <- function(x, breaks, include.lowest = TRUE, right = TRUE) {
  data.name <- deparse1(substitute(x))
  object <- if (is.factor(x)) x else {
    if (!is.numeric(x)) stop(sQuote(data.name), ' must be numeric')
    # ?base::cut.default will stop on !is.numeric(x), but the error message is not informative enough
    cut.default(x, breaks = breaks, include.lowest = include.lowest, right = right, ordered_result = TRUE)
  }
  new(Class = 'freqs', c(table(object)), data.name = data.name)
}

#' @title Show \linkS4class{freqs} Object
#' 
#' @description Show \linkS4class{freqs} object
#' 
#' @param object an \linkS4class{freqs} object
#' 
#' @return 
#' The \link[methods]{show} method for \linkS4class{freqs} object 
#' does not have a returned value.
#' 
#' @export
setMethod(f = show, signature = signature(object = 'freqs'), definition = function(object) {
  freq <- unclass(object)
  cfreq <- cumsum(freq)
  rev_cfreq <- rev.default(cumsum(rev.default(freq)))
  n <- sum(freq)
  ret <- cbind(
    'Frequency' = sprintf(fmt = '%d (%.2f%%)', freq, 100 * freq/n), 
    'Cummulative Freq' = sprintf(fmt = '%d (%.2f%%)', cfreq, 100 * cfreq/n),
    'Reversed Cumm Freq' = sprintf(fmt = '%d (%.2f%%)', rev_cfreq, 100 * rev_cfreq/n)
  )
  rownames(ret) <- names(freq)
  names(dimnames(ret)) <- c(object@data.name, 'Counts (%)')
  print.noquote(noquote(ret, right = TRUE))
  print(autoplot.freqs(object))
})


#' @export
autoplot.freqs <- function(object, ...) {
  ggplot() + autolayer.freqs(object, ...) + scale_y_continuous(labels = percent) + theme_bw()
}

#' @export
autolayer.freqs <- function(object, type = c('density', 'distribution'), title = NULL, ...) {
  freq <- unclass(object)
  cfreq <- cumsum(freq)
  n <- sum(freq)
  switch(match.arg(type), density = list(
    geom_bar(mapping = aes(x = names(freq), y = c(freq/n)), stat = 'identity'),
    labs(x = 'Categories', y = 'Relative Frequency', title = title)
  ), distribution = list(
    geom_bar(mapping = aes(x = names(freq), y = c(cfreq/n)), stat = 'identity'),
    #geom_step(mapping = aes(x = c(0, seq_along(c(freq))), y = c(0, cfreq/n))), # actually not pretty
    labs(x = 'Categories', y = 'Cumulative Relative Frequency', title = title)
  ))
}
