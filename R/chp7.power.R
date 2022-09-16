

#' @title S4 class \linkS4class{RejectionRegion}
#' 
#' @slot .Data \link[base]{numeric} scalar for one-sided test, 
#' or length-two \link[base]{numeric} vector for two-sided test
#' 
#' @slot null.value \link[base]{numeric} scalar, null value
#' 
#' @slot std.err \link[base]{numeric} scalar, standard error of sampling distribution
#' 
#' @slot parameter \link[base]{numeric} vector, additional parameters
#' 
#' @slot alternative \link[base]{character} scalar, alternative hypothesis
#' 
#' @slot sig.level \link[base]{numeric} scalar, significance level (Type I error probability)
#' 
#' @slot test \link[base]{character} scalar, type of test. Currently only \code{'z'} is supported
#' 
#' @export
setClass(Class = 'RejectionRegion', contains = 'numeric', slots = c(
  null.value = 'numeric',
  std.err = 'numeric',
  parameter = 'numeric',
  alternative = 'character', 
  sig.level = 'numeric',
  test = 'character'
), prototype = prototype(
  parameter = numeric()
))



#' @title S4 class \linkS4class{power}
#' 
#' @slot .Data \link[base]{numeric} scalar or vector, power(s) calculated at
#' alternative parameter(s) \eqn{\mu_1}
#' 
#' @slot x \link[base]{numeric} scalar or vector, alternative parameter(s) \eqn{\mu_1}
#' 
#' @slot rr \linkS4class{RejectionRegion} object
#' 
#' @export
setClass(Class = 'power', contains = 'numeric', slots = c(
  x = 'numeric',
  rr = 'RejectionRegion'
))






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
#' @param std.err \link[base]{numeric} scalar, standardized error.  
#' For one-sample \eqn{z}-test, this is \eqn{\sigma/\sqrt{n}}.  
#' Be aware of the name clash with \link[base]{stderr}
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
#' \link{power_z} returns a \linkS4class{power} object, for which a \link[methods]{show} method, an \link[ggplot2]{autolayer} and an \link[ggplot2]{autoplot} method are defined for \link{power_z} object.
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
power_z <- function(x, null.value, sd, n, std.err = sd / sqrt(n), alternative = c('two.sided', 'less', 'greater'), sig.level = .05) {
  rr <- RR_z(null.value = null.value, std.err = std.err, alternative = alternative, sig.level = sig.level)
  std.err <- rr@std.err
  pwr <- switch(rr@alternative, two.sided = {
    pnorm(q = unclass(rr)[1L], mean = x, sd = rr@std.err, lower.tail = TRUE) + # P(White), in left-RR
      pnorm(q = unclass(rr)[2L], mean = x, sd = rr@std.err, lower.tail = FALSE) # P(White), in Right-RR  
  }, 'less' = {
    pnorm(q = unclass(rr), mean = x, sd = rr@std.err, lower.tail = TRUE) # one-sided P(White)
  }, 'greater' = {
    pnorm(q = unclass(rr), mean = x, sd = rr@std.err, lower.tail = FALSE) # one-sided P(White)
  })
  new(Class = 'power', pwr, x = x, rr = rr)
}




RR_z <- function(null.value, std.err, alternative = c('two.sided', 'less', 'greater'), sig.level = .05) {
  alternative <- match.arg(alternative)
  rr <- switch(alternative, two.sided = {
    qnorm(p = c(sig.level/2, 1-sig.level/2), mean = null.value, sd = std.err) # 2-sided rejection region
    #message(sprintf(fmt = 'Two-sided Rejection Region: (-\u221E, %.3f) \u222A (%.3f, \u221E)', rr[1L], rr[2L]))
  }, less = {
    qnorm(p = sig.level, mean = null.value, sd = std.err, lower.tail = TRUE) # to find the cut-off value, so that P(X_bar < rr) = sig.level
    #message(sprintf(fmt = 'One-sided Rejection Region: (-\u221E, %.3f)', rr))
  }, greater = {
    qnorm(p = sig.level, mean = null.value, sd = std.err, lower.tail = FALSE) # to find the cut-off value, so that P(X_bar > rr) = sig.level
    #message(sprintf(fmt = 'One-sided Rejection Region: (%.3f, \u221E)', rr))
  })
  new(Class = 'RejectionRegion', rr, 
      null.value = null.value,
      std.err = std.err,
      alternative = alternative, 
      sig.level = sig.level,
      test = 'z')
}


#' @export
autolayer.RejectionRegion <- function(
    object,
    xlab = 'Alternative values of \u03bc', 
    ylab = NULL, 
    all.alternative = FALSE, 
    extra_x = numeric(),
    ...
) {
  if (object@test != 'z') stop('only z-test supported for now')
  rr <- unclass(object)
  null.value <- object@null.value
  std.err <- object@std.err
  alternative <- object@alternative
  sig.level <- object@sig.level
  
  if (length(extra_x)) {
    if (!is.vector(extra_x, mode = 'numeric') || anyNA(extra_x)) stop('illegal `extra_x`')
    #xlim <- range(c(xlim, extra_x))
  }
  
  if (all.alternative) {
    xlim <- range(null.value + c(-1, 1)*5*std.err, extra_x)
    # do not need `xmin` and `xmax` (for plotting rejection region, otherwise too crowded)
  } else switch(alternative, two.sided = {
    xlim <- range(null.value + c(-1, 1)*5*std.err, extra_x)
    xmin <- c(xlim[1L], rr[2L])
    xmax <- c(rr[1L], xlim[2L])
  }, less = {
    xlim <- range(null.value + c(-5, 1)*std.err, extra_x)
    xmin <- xlim[1L]
    xmax <- rr
  }, greater = {
    xlim <- range(null.value + c(-1, 5)*std.err, extra_x)
    xmin <- rr
    xmax <- xlim[2L]
  })
  
  ag <- list(null.value = null.value, std.err = std.err, sig.level = sig.level)
  
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



#' @title Show \linkS4class{power} Object
#' 
#' @description Show \linkS4class{power} object
#' 
#' @param object a \linkS4class{power} object
#' 
#' @return 
#' The \link[methods]{show} method for \linkS4class{power} object 
#' does not have a returned value.
#' 
#' @export
setMethod(show, signature(object = 'power'), definition = function(object) {
  x0 <- unclass(object)
  ret <- data.frame(
    'Alternative mu_1' = sprintf(fmt = '%.1f', object@x), # 'Alternative \u03bc'
    'beta' = sprintf(fmt = '%.2f%%', 1e2 * (1-x0)), # '\u03b2'
    '1-beta' = sprintf(fmt = '%.2f%%', 1e2 * x0), # '1-\u03b2'
    check.names = FALSE)
  # using unicode will cause ?devtools::check_rhub to give warning
  # unable to translate 'Alternative <U+03BC>' to native encoding
  print.data.frame(ret, row.names = FALSE)
})


#' @export
autolayer.power <- function(object, all.alternative = FALSE, ...) {
  rr <- object@rr
  x <- object@x
  pwr <- unclass(object)
  if (any(id <- (abs(x - rr@null.value) < 1e-4))) {
    x <- x[!id]
    pwr <- pwr[!id]
  }
  ret <- autolayer.RejectionRegion(rr, all.alternative = all.alternative, extra_x = x, ...)
  if (!length(x)) return(ret)
  
  ret_add <- if (all.alternative) list(
    geom_point(mapping = aes(x = x, y = pwr, colour = rr@alternative), size = 2L),
    geom_label_repel(mapping = aes(x = x, y = pwr, colour = rr@alternative, label = sprintf(fmt = '%.1f%%', 1e2*pwr)), size = 3.5)
  ) else list(
    geom_point(mapping = aes(x = x, y = pwr), size = 2L),
    geom_label_repel(mapping = aes(x = x, y = pwr, label = sprintf(fmt = '%.1f%%', 1e2*pwr)), size = 3.5)
  )
  return(c(list(ret), ret_add))
}


#' @export
autoplot.RejectionRegion <- function(object, title = NULL, ...) {
  ggplot() + autolayer.RejectionRegion(object, ...) +
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