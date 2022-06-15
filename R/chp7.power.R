
#' @title Chapter 7 (Power Curve)
#' 
#' @description 
#' 
#' Functions for Chapter 7, \emph{Hypothesis Testing}.
#' 
#' @param x \link[base]{numeric} vector, mean parameter(s) \eqn{\mu_1} in the alternative hypothesis
#' 
#' @param null.value \link[base]{numeric} scalar, mean parameter \eqn{\mu_0} in the null hypothesis
#' 
#' @param sd \link[base]{numeric} scalar, population standard deviation \eqn{\sigma}
#' 
#' @param n \link[base]{integer} scalar, sample size \eqn{n}
#' 
#' @param alternative \link[base]{character} scalar, alternative hypothesis, 
#' either \code{'two.sided'} (default), \code{'greater'} or \code{'less'}
#' 
#' @param sig.level \link[base]{numeric} scalar, significance level (i.e., Type-I-error rate), default \eqn{.05}
#' 
#' @details 
#' \link{power_z} calculates the powers at each element of the alternative parameters \eqn{\mu_1}, for one-sample \eqn{z}-test
#' \itemize{
#' \item{\eqn{H_0: \mu = \mu_0} vs. \eqn{H_A: \mu \neq \mu_0}, if \code{alternative = 'two.sided'}}
#' \item{\eqn{H_0: \mu \leq \mu_0} vs. \eqn{H_A: \mu > \mu_0}, if \code{alternative = 'greater'}}
#' \item{\eqn{H_0: \mu \geq \mu_0} vs. \eqn{H_A: \mu < \mu_0}, if \code{alternative = 'less'}}
#' }
#' 
#' @return 
#' \link{power_z} returns a \link{power} object, which is a \link[base]{numeric} vector with attributes
#' \describe{
#' \item{\code{x}}{a \link[base]{numeric} vector, alternative parameters \eqn{\mu_1}}
#' \item{\code{hypothesis}}{a \code{'z_hypothesis'} object, containing information of the null hypothesis,
#' significance level and rejection region, etc.}
#' }
#' A \link[base]{print} method, an \link[ggplot2]{autolayer} and an \link[ggplot2]{autoplot} method are defined for \link{power_z} object.
#' 
#' @seealso \link[stats]{power.t.test}
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @example inst/example/Chapter7.power.R 
#' 
#' @name Chapter07_power
#' @export
power_z <- function(x, null.value, sd, n, alternative = c('two.sided', 'less', 'greater'), sig.level = .05) {
  H <- z_hypothesis(null.value = null.value, sd = sd, n = n, alternative = alternative, sig.level = sig.level)
  pwr <- switch(H[['alternative']], two.sided = {
    pnorm(q = H[['rr']][1L], mean = x, sd = H[['stderr']], lower.tail = TRUE) + # P(White), in left-RR
      pnorm(q = H[['rr']][2L], mean = x, sd = H[['stderr']], lower.tail = FALSE) # P(White), in Right-RR  
  }, 'less' = {
    pnorm(q = H[['rr']], mean = x, sd = H[['stderr']], lower.tail = TRUE) # one-sided P(White)
  }, 'greater' = {
    pnorm(q = H[['rr']], mean = x, sd = H[['stderr']], lower.tail = FALSE) # one-sided P(White)
  })
  attr(pwr, which = 'x') <- x
  attr(pwr, which = 'hypothesis') <- H
  class(pwr) <- 'power'
  return(pwr)
}



z_hypothesis <- function(null.value, sd, n, alternative = c('two.sided', 'less', 'greater'), sig.level = .05) {
  alternative <- match.arg(alternative)
  stderr <- sd / sqrt(n)
  rr <- switch(alternative, two.sided = {
    qnorm(p = c(sig.level/2, 1-sig.level/2), mean = null.value, sd = stderr) # 2-sided rejection region
    #message(sprintf(fmt = 'Two-sided Rejection Region: (-\u221E, %.3f) \u222A (%.3f, \u221E)', rr[1L], rr[2L]))
  }, less = {
    qnorm(p = sig.level, mean = null.value, sd = stderr, lower.tail = TRUE) # to find the cut-off value, so that P(X_bar < rr) = sig.level
    #message(sprintf(fmt = 'One-sided Rejection Region: (-\u221E, %.3f)', rr))
  }, greater = {
    qnorm(p = sig.level, mean = null.value, sd = stderr, lower.tail = FALSE) # to find the cut-off value, so that P(X_bar > rr) = sig.level
    #message(sprintf(fmt = 'One-sided Rejection Region: (%.3f, \u221E)', rr))
  })
  ret <- list(
    rr = rr,
    null.value = null.value,
    sd = sd, n = n, stderr = stderr,
    alternative = alternative, 
    sig.level = sig.level
  )
  class(ret) <- 'z_hypothesis'
  return(ret)
}


#' @export
autolayer.z_hypothesis <- function(object, xlab = 'Alternative values of \u03bc', ylab = NULL, all.alternative = FALSE, ...) {
  rr <- object[['rr']]
  null.value <- object[['null.value']]
  n <- object[['n']]
  sd <- object[['sd']]
  stderr <- object[['stderr']]
  alternative <- object[['alternative']]
  sig.level <- object[['sig.level']]
  
  if (all.alternative) {
    xlim <- null.value + c(-1, 1)*5*stderr
    # do not need `xmin` and `xmax` (for plotting rejection region, otherwise too crowded)
  } else switch(alternative, two.sided = {
    xlim <- null.value + c(-1, 1)*5*stderr
    xmin <- c(xlim[1L], rr[2L])
    xmax <- c(rr[1L], xlim[2L])
  }, less = {
    xlim <- null.value + c(-5, 1)*stderr
    xmin <- xlim[1L]
    xmax <- rr
  }, greater = {
    xlim <- null.value + c(-1, 5)*stderr
    xmin <- rr
    xmax <- xlim[2L]
  })
  
  ag <- list(null.value = null.value, sd = sd, n = n, sig.level = sig.level)
  
  if (all.alternative) {
    if (is.null(ylab)) ylab <- 'Power'
    lyr <- list(
      stat_function(mapping = aes(colour = 'two.sided'), fun = power_z, args = c(ag, list(alternative = 'two.sided')), xlim = xlim, linetype = switch(alternative, two.sided = 1L, 2L)),
      stat_function(mapping = aes(colour = 'less'), fun = power_z, args = c(ag, list(alternative = 'less')), xlim = xlim, linetype = switch(alternative, less = 1L, 2L)),
      stat_function(mapping = aes(colour = 'greater'), fun = power_z, args = c(ag, list(alternative = 'greater')), xlim = xlim, linetype = switch(alternative, greater = 1L, 2L)),
      scale_color_discrete(name = 'Alternative\nHypothesis', breaks = c('two.sided', 'less', 'greater'), labels = paste('\u03bc', c('\u2260', '<', '>'), null.value))
    )
  } else {
    if (is.null(ylab)) ylab <- paste('Power of Ha: \u03bc', switch(alternative, two.sided = '\u2260', less = '<', greater = '>'), null.value)
    lyr <- list(
      stat_function(fun = power_z, args = c(ag, list(alternative = alternative)), xlim = xlim),
      annotate(geom = 'rect', xmin = xmin, xmax = xmax, ymin = 0, ymax = 1, alpha = .2)
    )
  }
  
  return(c(lyr, list(
    geom_point(mapping = aes(x = null.value, y = sig.level), size = 2L),
    geom_label_repel(mapping = aes(x = null.value, y = sig.level, label = sprintf(fmt = '\u03bc0 = %.1f', null.value)), size = 3.5),
    labs(x = xlab, y = ylab)
  )))
}




#' @export
print.power <- function(x, ...) {
  #ret <- cbind('Alternative \u03bc' = sprintf(fmt = '%.1f', attr(x, which = 'x', exact = TRUE)),
  #             '\u03b2' = sprintf(fmt = '%.2f%%', 1e2 * (1-x)),
  #             '1-\u03b2' = sprintf(fmt = '%.2f%%', 1e2 * x))
  # using unicode will cause ?devtools::check_rhub to give warning
  # unable to translate 'Alternative <U+03BC>' to native encoding
  ret <- data.frame(
    'Alternative mu_1' = sprintf(fmt = '%.1f', attr(x, which = 'x', exact = TRUE)),
    'beta' = sprintf(fmt = '%.2f%%', 1e2 * (1-x)),
    '1-beta' = sprintf(fmt = '%.2f%%', 1e2 * x), 
    check.names = FALSE)
  print.data.frame(ret, row.names = FALSE)
}


#' @export
autolayer.power <- function(object, all.alternative = FALSE, ...) {
  H <- attr(object, which = 'hypothesis', exact = TRUE)
  x <- attr(object, which = 'x', exact = TRUE)
  pwr <- unclass(object)
  if (any(id <- (abs(x - H[['null.value']]) < 1e-4))) {
    x <- x[!id]
    pwr <- pwr[!id]
  }
  list(
    autolayer(H, all.alternative = all.alternative, ...),
    if (length(x)) {
      if (all.alternative) {
        geom_point(mapping = aes(x = x, y = pwr, colour = H[['alternative']]), size = 2L)
      } else geom_point(mapping = aes(x = x, y = pwr), size = 2L)
    },
    if (length(x)) {
      if (all.alternative) {
        geom_label_repel(mapping = aes(x = x, y = pwr, colour = H[['alternative']], label = sprintf(fmt = '%.1f%%', 1e2*pwr)), size = 3.5)
      } else geom_label_repel(mapping = aes(x = x, y = pwr, label = sprintf(fmt = '%.1f%%', 1e2*pwr)), size = 3.5)
    }
  )
}


#' @export
autoplot.z_hypothesis <- function(object, title = NULL, ...) {
  ggplot() + autolayer.z_hypothesis(object, ...) +
    scale_y_continuous(labels = percent) +
    labs(title = title) +
    theme_bw()
}

#' @export
autoplot.power <- function(object, title = NULL, ...) {
  ggplot() + autolayer.power(object, ...) +
    scale_y_continuous(labels = percent) +
    labs(title = title) +
    theme_bw()
}