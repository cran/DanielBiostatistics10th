

#' @title Chapter 7 (Power Curve)
#' 
#' @description 
#' 
#' Functions for Chapter 7, \emph{Hypothesis Testing}.
#' 
#' @param x \link[base]{numeric} \link[base]{vector}, mean parameter(s) \eqn{\mu_1} in the alternative hypothesis
#' 
#' @param null.value \link[base]{numeric} scalar, mean parameter \eqn{\mu_0} in the null hypothesis
#' 
#' @param sd \link[base]{numeric} scalar, population standard deviation \eqn{\sigma}
#' 
#' @param n \link[base]{integer} scalar, sample size \eqn{n}
#' 
#' @param alternative \link[base]{character} scalar, alternative hypothesis, 
#' either `'two.sided'` (default), `'greater'` or `'less'`
#' 
#' @param sig.level \link[base]{numeric} scalar, significance level (i.e., Type-I-error rate), default \eqn{.05}
#' 
#' @details 
#' Function [power_z()] calculates the powers at each element of the alternative parameters \eqn{\mu_1}, for one-sample \eqn{z}-test
#' \itemize{
#' \item{\eqn{H_0: \mu = \mu_0} vs. \eqn{H_A: \mu \neq \mu_0}, if `alternative = 'two.sided'`}
#' \item{\eqn{H_0: \mu \leq \mu_0} vs. \eqn{H_A: \mu > \mu_0}, if `alternative = 'greater'`}
#' \item{\eqn{H_0: \mu \geq \mu_0} vs. \eqn{H_A: \mu < \mu_0}, if `alternative = 'less'`}
#' }
#' 
#' @return 
#' Function [power_z()] returns a `'power_z'` object, 
#' which inherits from `'power.htest'` class. 
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
power_z <- function(
    x, 
    null.value, 
    sd, n, 
    alternative = c('two.sided', 'less', 'greater'), 
    sig.level = .05
) {
  
  alternative <- match.arg(alternative)
  
  std.err <- sd / sqrt(n)
  
  powerFun <- function(x, alternative) {
    
    switch(alternative, two.sided = {
      rr <- qnorm(p = c(sig.level/2, 1-sig.level/2), mean = null.value, sd = std.err) # 2-sided rejection region
      ret <- pnorm(q = rr[1L], mean = x, sd = std.err, lower.tail = TRUE) + # P(White), in left-RR
        pnorm(q = rr[2L], mean = x, sd = std.err, lower.tail = FALSE) # P(White), in Right-RR  
      attr(ret, which = 'reject') <- sprintf(fmt = '<%.3f or >%.3f', rr[1L], rr[2L])
      
    }, 'less' = {
      rr <- qnorm(p = sig.level, mean = null.value, sd = std.err, lower.tail = TRUE) # P(X_bar < rr) = sig.level
      ret <- pnorm(q = rr, mean = x, sd = std.err, lower.tail = TRUE) # one-sided P(White)
      attr(ret, which = 'reject') <- sprintf(fmt = '<%.3f', rr)
      
    }, 'greater' = {
      rr <- qnorm(p = sig.level, mean = null.value, sd = std.err, lower.tail = FALSE) # P(X_bar > rr) = sig.level
      ret <- pnorm(q = unclass(rr), mean = x, sd = std.err, lower.tail = FALSE) # one-sided P(White)
      attr(ret, which = 'reject') <- sprintf(fmt = '>%.3f', rr)
    })
    
    return(ret)
    
  }
  
  power <- powerFun(x, alternative = alternative)
  
  ret <- list(
    mu = x, 
    null.value = null.value, 
    sd = sd, n = n,
    power = power,
    sig.level = sig.level,
    reject = attr(power, which = 'reject', exact = TRUE),
    alternative = alternative, 
    method = 'One-Sample z-Test'
  )
  attr(ret, which = 'power.function') <- powerFun
  class(ret) <- c('power_z', 'power.htest')
  return(ret)

}


# for which an \link[ggplot2]{autolayer} and an \link[ggplot2]{autoplot} method are defined for `'power_z'` object.

#' @importFrom latex2exp TeX
#' @export
autolayer.power_z <- function(
    object, 
    all.alternative = FALSE, 
    xlim = range(object$mu),
    ...
) {
  
  fun <- attr(object, which = 'power.function', exact = TRUE)
  
  if (all.alternative) {
    ylab <- TeX(sprintf('Power ($alpha$ = %.0f%%)', 1e2*object$sig.level))
    ret_add <- list(
      stat_function(mapping = aes(colour = 'two.sided'), fun = fun, args = list(alternative = 'two.sided'), xlim = xlim, linetype = switch(object$alternative, two.sided = 1L, 2L)),
      stat_function(mapping = aes(colour = 'less'), fun = fun, args = list(alternative = 'less'), xlim = xlim, linetype = switch(object$alternative, less = 1L, 2L)),
      stat_function(mapping = aes(colour = 'greater'), fun = fun, args = list(alternative = 'greater'), xlim = xlim, linetype = switch(object$alternative, greater = 1L, 2L)),
      scale_color_discrete(name = 'Alternative\nHypothesis', breaks = c('two.sided', 'less', 'greater'), labels = lapply(sprintf(fmt = '$mu %s %f$', c('\\neq', '<', '>'), object$null.value), FUN = TeX)),
      geom_point(mapping = aes(x = object$mu, y = object$power, colour = object$alternative), size = 2L),
      geom_label_repel(mapping = aes(x = object$mu, y = object$power, colour = object$alternative, label = sprintf(fmt = '%.1f%%', 1e2*object$power)), size = 3.5)
    )
    
  } else {
    ylab <- TeX(sprintf(fmt = 'Power of Ha: $mu %s %f$, $alpha=%.0f$%%', switch(object$alternative, two.sided = '\\neq', less = '<', greater = '>'), object$null.value, 1e2*object$sig.level))
    ret_add <- list(
      stat_function(fun = fun, args = list(alternative = object$alternative), xlim = xlim),
      geom_point(mapping = aes(x = object$mu, y = object$power), size = 2L),
      geom_label_repel(mapping = aes(x = object$mu, y = object$power, label = sprintf(fmt = '%.1f%%', 1e2*object$power)), size = 3.5)
    )
  }
  
  ret_null = list(
    #geom_label_repel(mapping = aes(x = object$null.value, y = 0), label = sprintf(fmt = '\u03bc0 = %.1f', null.value), size = 3.5)
    #geom_label_repel(mapping = aes(x = object$null.value, y = 0), label = TeX(sprintf(fmt = '$mu_0$ = %.1f', null.value)), size = 3.5)
    # ggrepel::geom_label_repel doesn't work well with latex2exp::TeX
    geom_point(mapping = aes(x = object$null.value, y = 0), size = 3, colour = 'blue')
  )
  
  return(c(ret_add, ret_null, list(labs(
    x = TeX('Alternative values of $mu$'),
    y = ylab
  ))))
}



#' @export
autoplot.power_z <- function(object, ...) {
  ggplot() + autolayer.power_z(object, ...) +
    scale_y_continuous(labels = percent) +
    # scale_x_continuous(breaks = object$null.value, labels = 'a') +
    # https://stackoverflow.com/questions/29824773/annotate-ggplot-with-an-extra-tick-and-label
    #theme(legend.position = c(.85, .5), element_text(family = 'Arial Unicode MS')) # error ??
    theme(legend.position = c(.85, .5))
}