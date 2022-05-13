


#' @title Chapter 5, 6 and 7
#' 
#' @description 
#' 
#' Functions for Chapter 5, \emph{Some Important Sampling Distributions},
#' Chapter 6, \emph{Estimation} and 
#' Chapter 7, \emph{Hypothesis Testing}.
#' 
#' @param x \link[base]{integer} scalar or length-two vector, positive count(s) of binary variable(s)
#' 
#' @param xbar \link[base]{numeric} scalar or length-two vector, sample mean(s) for numeric variable(s) or
#' sample proportion(s) for binary variable(s). 
#' In the case of two-sample tests, this could also be a \link[base]{numeric} scalar indicating the difference in 
#' sample means or sample proportions.
#' 
#' @param xsd \link[base]{numeric} scalar or length-two vector, sample standard deviation(s)
#' 
#' @param sd \link[base]{numeric} scalar or length-two vector, population standard deviation(s)
#' 
#' @param n \link[base]{integer} scalar or length-two vector, sample size(s)
#' 
#' @param prob \link[base]{numeric} scalar or length-two vector, population probability or probabilities
#' 
#' @param null.value \link[base]{numeric} scalar, null value of hypothesis testing
#' 
#' @param alternative \link[base]{character} scalar, alternative hypothesis,
#' either \code{'two.sided'} (default), \code{'greater'} or \code{'less'}
#' 
#' @param conf.level \link[base]{numeric} scalar, confidence level, default 0.95
#' 
#' @param var.equal \link[base]{logical} scalar, whether to treat the two population variances as being equal 
#' (default \code{FALSE})
#' 
#' @param p.equal \link[base]{logical} scalar, whether to use pooled sample proportion 
#' (default \code{FALSE}) if \code{prob} is not provided
#' 
#' @param ... potential arguments, not in use currently
#' 
#' @details  
#' 
#' \link{aggregated_z_test} performs one-sample or two-sample \eqn{z}-test 
#' using aggregated statistics of sample mean(s) and sample size(s).
#' 
#' \link{aggregated_t_test} performs one-sample or two-sample \eqn{t}-test 
#' using aggregated statistics of sample mean(s), sample standard deviation(s) and sample size(s).
#' 
#' \link{prop_test_CLT} performs one-sample or two-sample \eqn{z}-test on proportion(s),
#' using Central Limit Theorem
#' 
#' \link{aggregated_var_test} performs \eqn{\chi^2}-test on one-sample variance, 
#' or \eqn{F}-test on two-sample variances, using aggregated statistics of sample standard deviation(s) and sample size(s).
#' 
#' @return 
#' \link{aggregated_z_test} returns an \code{'htest'} object.
#' 
#' \link{aggregated_t_test} returns an \code{'htest'} object.
#' 
#' \link{prop_test_CLT} returns an \code{'htest'} object.
#' 
#' \link{aggregated_var_test} returns an \code{'htest'} object.
#' 
#' @seealso \link[stats]{t.test} \link[stats]{prop.test} \link[stats]{var.test}
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @example inst/example/Chapter5.6.7.R 
#' 
#' @name Chapter05to07
#' @export
aggregated_z_test <- function(xbar, n, sd, null.value = 0, alternative = c('two.sided', 'less', 'greater'), conf.level = .95, ...) {
  
  if (!is.numeric(xbar) || anyNA(xbar)) stop('Illegal sample mean(s)')
  if (!is.numeric(sd) || anyNA(sd) || any(sd <= 0)) stop('Illegal population standard deviation(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal sample size(s)')
  if (!is.numeric(null.value) || length(null.value) != 1L || anyNA(null.value)) stop('Hypothesized mean (difference) must be len-1 number')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  alternative <- match.arg(alternative)
  
  tmp <- data.frame(xbar = xbar, sd = sd, n = n) # vector recycling, let warn
  xbar <- tmp$xbar
  sd <- tmp$sd
  n <- tmp$n
  dname <- sprintf(fmt = '%.3g(\u00B1%.3g)', xbar, sd)
  
  if (length(xbar) == 1L) { # one sample z-test
    method <- 'One Sample z-test'
    stderr <- sd / sqrt(n)
    xbar0 <- xbar
    
  } else if (length(xbar) == 2L) { # two sample z-test
    method <- 'Two Sample z-test'
    stderr <- sqrt(sd[1L]^2/n[1L] + sd[2L]^2/n[2L])
    if (isTRUE(all.equal.numeric(xbar[1L], xbar[2L]))) {
      xbar0 <- xbar[1L] # input is actually (xbar1 - xbar2), difference of sample means
      dname <- sprintf(fmt = '\u0394x\u0304=%.1f (\u00B1%.3g vs. \u00B1%.3g)', xbar0, sd[1L], sd[2L])
    } else {
      xbar0 <- xbar[1L] - xbar[2L]
      dname <- paste(dname, collapse = ' vs. ')
    }
  } else stop('should not come here')
  
  zstat <- (xbar0 - null.value) / stderr
  switch(alternative, less = {
    pval <- pnorm(zstat, lower.tail = TRUE)
    cint0 <- c(-Inf, qnorm(conf.level, lower.tail = TRUE))
  }, greater = {
    pval <- pnorm(zstat, lower.tail = FALSE)
    cint0 <- c(qnorm(conf.level, lower.tail = FALSE), Inf)
  }, two.sided = {
    pval <- 2 * pnorm(abs(zstat), lower.tail = FALSE)
    cint0 <- c(-1, 1) * qnorm((1 - conf.level)/2, lower.tail = FALSE)
  })
  
  cint <- xbar0 + cint0 * stderr
  attr(cint, which = 'conf.level') <- conf.level
  
  ret <- list(
    statistic = setNames(zstat, nm = 'z'),
    p.value = pval, 
    conf.int = cint, 
    null.value = setNames(null.value, nm = switch(length(n), '1' = 'mean', '2' = 'mean-difference')), 
    stderr = stderr, alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}




#' @rdname Chapter05to07
#' @export
aggregated_t_test <- function(
    xbar, xsd, n, 
    null.value = 0, var.equal = FALSE, 
    alternative = c('two.sided', 'less', 'greater'), conf.level = .95, 
    ...
) {
  
  if (!is.numeric(xbar) || anyNA(xbar)) stop('Illegal sample mean(s)')
  if (!is.numeric(xsd) || anyNA(xsd) || any(xsd <= 0)) stop('Illegal sample standard deviation(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal sample size(s)')
  if (!is.numeric(null.value) || length(null.value) != 1L || anyNA(null.value)) stop('Hypothesized mean (difference) must be len-1 number')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  alternative <- match.arg(alternative)
  
  tmp <- data.frame(xbar = xbar, xsd = xsd, n = n) # vector recycling, let warn
  xbar <- tmp$xbar
  xsd <- tmp$xsd
  n <- tmp$n
  dname <- sprintf(fmt = '%.3g\u00B1%.3g', xbar, xsd)
  
  if (length(xbar) == 1L) { # one sample t-test
    method <- 'One Sample t-test'
    df <- n - 1L
    stderr <- xsd / sqrt(n)
    xbar0 <- xbar
    
  } else if (length(xbar) == 2L) { # two sample t-test
    method <- if (var.equal) 'Two Sample t-test (Equal-Variance)' else 'Welch Two Sample t-test'
    
    df <- Gosset_Welch(s1 = xsd[1L], s2 = xsd[2L], n1 = n[1L], n2 = n[2L], var.equal = var.equal)
    stderr <- attr(df, which = 'stderr', exact = TRUE)
    if (isTRUE(all.equal.numeric(xbar[1L], xbar[2L]))) {
      xbar0 <- xbar[1L] # input is actually (xbar1 - xbar2), difference of sample means
      dname <- sprintf(fmt = '\u0394x\u0304=%.1f (\u00B1%.3g vs. \u00B1%.3g)', xbar0, xsd[1L], xsd[2L])
    } else {
      xbar0 <- xbar[1L] - xbar[2L]
      dname <- paste(dname, collapse = ' vs. ')
    }
  } else stop('should not come here')
  
  tstat <- (xbar0 - null.value) / stderr
  switch(alternative, less = {
    pval <- pt(tstat, df = df, lower.tail = TRUE)
    cint0 <- c(-Inf, qt(conf.level, df = df, lower.tail = TRUE))
  }, greater = {
    pval <- pt(tstat, df = df, lower.tail = FALSE)
    cint0 <- c(qt(conf.level, df = df, lower.tail = FALSE), Inf)
  }, two.sided = {
    pval <- 2 * pt(abs(tstat), df = df, lower.tail = FALSE)
    cint0 <- c(-1, 1) * qt((1 - conf.level)/2, df = df, lower.tail = FALSE)
  })
  
  cint <- xbar0 + cint0 * stderr
  attr(cint, which = 'conf.level') <- conf.level
  
  ret <- list(
    statistic = setNames(tstat, nm = 't'), parameter = setNames(df, nm = 'df'), p.value = pval, 
    conf.int = cint, 
    null.value = setNames(null.value, nm = switch(length(xbar), '1' = 'mean', '2' = 'mean-difference')),
    stderr = stderr, alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}



#' @rdname Chapter05to07
#' @export
prop_test_CLT <- function(x, n, xbar = x/n, prob = xbar, p.equal = FALSE,
    null.value = 0, alternative = c('two.sided', 'less', 'greater'), conf.level = .95, 
    ...
) {
  
  if (!is.numeric(xbar) || anyNA(xbar) || any(xbar < 0)) stop('Illegal sample proportion(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal total count(s)')
  if (!is.numeric(prob) || anyNA(prob) || any(prob < 0) || any(prob > 1)) stop('Illegal population proportion(s)')
  if (!missing(null.value)) {
    if (!is.numeric(null.value) || length(null.value) != 1L || anyNA(null.value)) stop('Hypothesized proportion (difference) must be len-1 number')
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  alternative <- match.arg(alternative)
  
  tmp <- if (missing(x)) {
    data.frame(xbar = xbar, n = n, prob = prob) # vector recycling, let warn
  } else data.frame(x = x, xbar = xbar, n = n, prob = prob)
  xbar <- tmp[['xbar']]
  n <- tmp$n
  dname <- sprintf(fmt = '%.1f%% (n=%d)', 1e2*xbar, n)
  
  if (length(n) == 1L) { # one sample test
    method <- 'One Sample z-test on Proportion'
    prob <- tmp[['prob']]
    stderr <- sqrt(prob*(1-prob)/n)
    xbar0 <- xbar
    if (missing(null.value)) null.value <- prob
    
  } else if (length(n) == 2L) { # two sample test
    method <- 'Two Sample z-test on Proportions'
    
    if (missing(prob)) {
      if (length(x <- tmp[['x']]) && p.equal) {
        prob <- rep(sum(x)/sum(n), times = 2L)
      } else prob <- tmp[['prob']]
    } else prob <- tmp[['prob']]
    stderr <- sqrt(prob[1L]*(1-prob[1L])/n[1L] + prob[2L]*(1-prob[2L])/n[2L])
    if (isTRUE(all.equal.numeric(xbar[1L], xbar[2L]))) {
      xbar0 <- xbar[1L] # input is actually (xbar1 - xbar2), difference of sample proportions
      dname <- sprintf(fmt = '\u0394p\u0302=%.1f%%', 1e2*xbar0)
    } else {
      xbar0 <- xbar[1L] - xbar[2L]
      dname <- paste(dname, collapse = ' vs. ')
    }
    if (missing(null.value)) null.value <- prob[1L] - prob[2L]
    
  } else stop('should not come here')
  
  zstat <- (xbar0 - null.value) / stderr
  switch(alternative, less = {
    pval <- pnorm(zstat, lower.tail = TRUE)
    cint0 <- c(-Inf, qnorm(conf.level, lower.tail = TRUE))
  }, greater = {
    pval <- pnorm(zstat, lower.tail = FALSE)
    cint0 <- c(qnorm(conf.level, lower.tail = FALSE), Inf)
  }, two.sided = {
    pval <- 2 * pnorm(abs(zstat), lower.tail = FALSE)
    cint0 <- c(-1, 1) * qnorm((1 - conf.level)/2, lower.tail = FALSE)
  })
  
  cint <- xbar0 + cint0 * stderr
  attr(cint, which = 'conf.level') <- conf.level
  
  ret <- list(
    statistic = setNames(zstat, nm = 'z'),
    p.value = pval, 
    conf.int = cint, 
    null.value = setNames(null.value, nm = switch(length(n), '1' = 'proportion', '2' = 'proportion-difference')),
    stderr = stderr, alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}





#' @rdname Chapter05to07
#' @export
aggregated_var_test <- function(xsd, n, null.value = 1, alternative = c('two.sided', 'less', 'greater'), conf.level = .95, ...) {
  
  if (!is.numeric(xsd) || anyNA(xsd) || any(xsd <= 0)) stop('Illegal sample variances(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal sample size(s)')
  if (!is.numeric(null.value) || length(null.value) != 1L || anyNA(null.value)) stop('Hypothesized mean (difference) must be len-1 number')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  alternative <- match.arg(alternative)
  
  tmp <- data.frame(xvar = xsd^2, n = n)
  xvar <- tmp[['xvar']]
  n <- tmp[['n']]
  df <- n - 1L
  dname <- sprintf(fmt = '\u03c32=%.2f (n=%d)', xvar, n)
  
  if (length(xvar) == 1L) {
    # essentially ?EnvStats::varTest
    estimate <- NULL # actually `xvar`, no need to output
    null.value <- setNames(null.value, nm = 'variance')
    method <- 'Chi-squared test on one-sample variance'
    v_stat <- setNames((df * xvar) / null.value, nm = 'Chi-Squared')
    df <- setNames(df, nm = 'df')
    pval <- unname(pchisq(v_stat, df = df))
    switch(alternative, two.sided = {
      pval <- 2 * min(pval, 1-pval)
      a2 <- (1 - conf.level)/2
      cint <- df * xvar / qchisq(c(1-a2, a2), df = df)
    }, less = {
      cint <- c(0, df * xvar / qchisq(1 - conf.level, df = df))
    }, greater = {
      pval <- 1 - pval
      cint <- c(df * xvar / qchisq(conf.level, df = df), Inf)
    })
    
  } else if (length(xvar) == 2L) {
    estimate <- setNames(xvar[1L] / xvar[2L], nm = 'Estimated Variances-Ratio')
    null.value <- setNames(null.value, nm = 'variances-ratio')
    method <- 'F test to compare two variances'
    dname <- paste(dname, collapse = ' vs. ')
    v_stat <- setNames(estimate / null.value, nm = 'F')
    df <- setNames(df, nm = c('num df', 'denom df'))
    pval <- pf(v_stat, df1 = df[1L], df2 = df[2L])
    switch(alternative, two.sided = {
      pval <- 2 * min(pval, 1-pval)
      a2 <- (1 - conf.level)/2
      cint <- estimate / qf(c(1-a2, a2), df1 = df[1L], df2 = df[2L])
    }, less = {
      cint <- c(0, estimate/qf(1 - conf.level, df1 = df[1L], df2 = df[2L]))
    }, greater = {
      pval <- 1 - pval
      cint <- c(estimate/qf(conf.level, df1 = df[1L], df2 = df[2L]), Inf)
    })
    
  } else stop('should not come here')
  
  cint <- unname(cint)
  attr(cint, which = 'conf.level') <- conf.level
  ret <- list(
    statistic = v_stat, parameter = df,
    p.value = pval, conf.int = cint, 
    estimate = estimate, null.value = null.value,
    alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}












