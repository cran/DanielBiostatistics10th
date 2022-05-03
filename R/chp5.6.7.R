


#' @title Chapter 5, 6 and 7
#' 
#' @description 
#' 
#' Functions for Chapter 5, \emph{Some Important Sampling Distributions},
#' Chapter 6, \emph{Estimation} and 
#' Chapter 7, \emph{Hypothesis Testing} of Wayne W. Daniel's 
#' \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' 
#' @param x \link[base]{integer} scalar or length-2 vector of positive count(s) of binary variable(s)
#' 
#' @param xbar \link[base]{numeric} scalar or length-2 vector, sample mean(s)
#' 
#' @param xsd \link[base]{numeric} scalar or length-2 vector, sample standard deviation(s)
#' 
#' @param xvar \link[base]{numeric} scalar or length-2 vector, sample variance(s)
#' 
#' @param sd \link[base]{numeric} scalar or length-2 vector, population standard deviation(s)
#' 
#' @param n \link[base]{integer} scalar or length-2 vector, sample size(s)
#' 
#' @param prob \link[base]{numeric} scalar or length-2 vector, population probability or population probabilities
#' 
#' @param phat \link[base]{numeric} scalar or length-2 vector, sample proportions or difference of sample proportions
#' 
#' @param null.value \link[base]{numeric} scalar, null value of hypothesis testing
#' 
#' @param alternative \link[base]{character} scalar, alternative hypothesis,
#' either \code{'two.sided'} (default), \code{'greater'} or \code{'less'}. 
#' See \code{\link[stats]{t.test}}.
#' 
#' @param conf.level \link[base]{numeric} scalar, confidence level, default 0.95.  
#' See \code{\link[stats]{t.test}}.
#' 
#' @param var.equal \link[base]{logical} scalar, whether to treat the two population variances as being equal 
#' (default \code{FALSE}).  See \code{\link[stats]{t.test}}.
#' 
#' @param p.equal \link[base]{logical} scalar, whether to use pooled sample proportion 
#' (default \code{FALSE}).
#' 
#' @param ... potential arguments, not in use currently
#' 
#' @return 
#' 
#' \code{\link{aggregated_z_test}} performs one-sample or two-sample z-test 
#' using aggregated statistics of sample mean(s) and sample size(s).
#' 
#' \code{\link{aggregated_t_test}} performs one-sample or two-sample t-test 
#' using aggregated statistics of sample mean(s), sample standard deviation(s) and sample size(s).
#' 
#' \code{\link{prop_test_CLT}} performs one-sample or two-sample z-test on proportions,
#' using Central Limit Theorem
#' 
#' \code{\link{aggregated_var_test}} performs chi-squared test on one-sample variance, 
#' or F-test on two-sample variances, using aggregated statistics of sample variance(s) and sample size(s).
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @examples 
#' 
#' # Page 142, Example 5.3.2
#' aggregated_z_test(xbar = 190, sd = 12.7, n = 10L, null.value = 185.6, alternative = 'greater')
#' # Page 143, Example 5.3.3
#' pnorm(125, mean = 120, sd = 15/sqrt(50)) - pnorm(115, mean = 120, sd = 15/sqrt(50))
#'  
#' # Page 145, Example 5.4.1
#' aggregated_z_test(xbar = c(92, 105), sd = 20, n = 15L, alternative = 'less') 
#' # Page 148, Example 5.4.2
#' aggregated_z_test(xbar = 20, sd = c(15, 20), n = c(35L, 40L), null.value = 45-30, 
#'   alternative = 'greater')
#' 
#' # Page 150, Example 5.5.1
#' prop_test_CLT(phat = .4, n = 150L, prob = .357, alternative = 'greater')
#' # Page 152, Example 5.5.2
#' prop_test_CLT(phat = .45, n = 200L, prob = .51, alternative = 'less')
#' 
#' # Page 155, Example 5.6.1
#' prop_test_CLT(phat = .1, prob = c(.28, .21), n = 100L, alternative = 'greater')
#' # Page 155, Example 5.6.2
#' prop_test_CLT(phat = .05, prob = c(.34, .26), n = c(250L, 200L), alternative = 'less')
#' 
#' # Page 166, Example 6.2.1
#' aggregated_z_test(xbar = 22, n = 10L, sd = sqrt(45))
#' # Page 168, Example 6.2.2
#' aggregated_z_test(xbar = 84.3, n = 15L, sd = sqrt(144), conf.level = .99)
#' # Page 168, Example 6.2.3
#' aggregated_z_test(xbar = 17.2, n = 35L, sd = 8, conf.level = .9)
#' # Page 169, Example 6.2.4
#' dat_624 = read.csv(system.file('extdata', 'EXA_C06_S02_04.csv', 
#'   package = 'DanielBiostatistics10th'))
#' head(dat_624)
#' aggregated_z_test(xbar = mean(dat_624$ACTIVITY), n = nrow(dat_624), sd = sqrt(.36))
#' 
#' # Page 173, Example 6.3.1
#' aggregated_t_test(xbar = 250.8, xsd = 130.9, n = 19L)
#' 
#' # Page 177, Example 6.4.1
#' aggregated_z_test(xbar = c(4.5, 3.4), sd = sqrt(c(1, 1.5)), n = c(12L, 15L))
#' # Page 178, Example 6.4.2
#' aggregated_z_test(xbar = c(4.3, 13), sd = c(5.22, 8.97), n = c(328L, 64L), conf.level = .99)
#' # Page 180, Example 6.4.3
#' aggregated_t_test(xbar = c(4.7, 8.8), xsd = c(9.3, 11.5), n = c(18L, 10L), var.equal = TRUE)
#' # Page 181, Example 6.4.4
#' aggregated_t_test(xbar = c(4.7, 8.8), xsd = c(9.3, 11.5), n = c(18L, 10L)) 
#' # Welch slightly different from Cochran; textbook explained on Page 182
#' 
#' # Page 185, Example 6.5.1
#' prop_test_CLT(phat = .18, n = 1220L)
#' 
#' # Page 187, Example 6.6.1
#' prop_test_CLT(x = c(31L, 53L), n = c(68L, 255L), conf.level = .99)
#' prop.test(x = c(31L, 53L), n = c(68L, 255L), conf.level = .99, correct = FALSE)
#' 
#' # Page 190, Example 6.7.1
#' n_671 = uniroot(f = function(n, sd, level = .95) {
#'   qnorm(1-(1-level)/2) * sd/sqrt(n) - 5 # half-width of CI <= 5 grams
#' }, interval = c(0, 2e2), sd = 20
#' )
#' sprintf('Example 6.7.1 requires a sample size of %d.', ceiling(n_671$root))
#' 
#' # Page 192, Example 6.8.1
#' n_681 = uniroot(f = function(n, p, level = .95) {
#'   qnorm(1-(1-level)/2) * sqrt(p*(1-p)/n) - .05
#' }, interval = c(0, 1e3), p = .35)
#' sprintf('Example 6.8.1 requires a sample size of %d.', ceiling(n_681$root))
#' 
#' # Page 196, Example 6.9.1
#' d691 = c(9.7, 12.3, 11.2, 5.1, 24.8, 14.8, 17.7)
#' sqrt(aggregated_var_test(xvar = var(d691), n = length(d691))$conf.int)
#' 
#' # Page 200, Example 6.10.1
#' aggregated_var_test(xvar = c(8.1, 5.9)^2, n = c(16L, 4L))
#' 
#' # Page 222, Example 7.2.1
#' aggregated_z_test(xbar = 27, sd = sqrt(20), n = 10L, null.value = 30)
#' # Page 226, Example 7.2.2
#' aggregated_z_test(xbar = 27, sd = sqrt(20), n = 10L, null.value = 30, alternative = 'less')
#' # Page 228, Example 7.2.3
#' head(d723 <- read.csv(system.file('extdata', 'EXA_C07_S02_03.csv', 
#'   package = 'DanielBiostatistics10th')))
#' t.test(d723$DAYS, mu = 15)
#' # Page 231, Example 7.2.4
#' aggregated_z_test(xbar = 146, sd = 27, n = 157L, null.value = 140, alternative = 'greater')
#' # Page 232, Example 7.2.5
#' d725 = c(33.38, 32.15, 34.34, 33.95, 33.46, 34.13, 33.99, 34.10, 33.85, 
#'   34.23, 34.45, 34.19, 33.97, 32.73, 34.05)
#' t.test(d725, mu = 34.5)
#' 
#' # Page 237, Example 7.3.1
#' aggregated_z_test(xbar = c(4.5, 3.4), sd = sqrt(c(1, 1.5)), n = c(12L, 15L))
#' # Page 239, Example 7.3.2
#' head(d732 <- read.csv(system.file('extdata', 'EXA_C07_S03_02.csv', 
#'   package = 'DanielBiostatistics10th')))
#' with(d732, t.test(x = CONTROL, y = SCI, alternative = 'less', var.equal = TRUE))
#' # Page 240, Example 7.3.3
#' aggregated_t_test(xbar = c(19.16, 9.53), xsd = c(5.29, 2.69), n = c(15L, 30L))
#' # Page 242, Example 7.3.4
#' aggregated_z_test(xbar = c(59.01, 46.61), sd = c(44.89, 34.85), n = c(53L, 54L), 
#'   alternative = 'greater')
#' 
#' # Page 251, Example 7.4.1
#' head(d741 <- read.csv(system.file('extdata', 'EXA_C07_S04_01.csv', 
#'   package = 'DanielBiostatistics10th')))
#' with(d741, t.test(x = POSTOP, y = PREOP, alternative = 'greater', paired = TRUE))
#' 
#' # Page 258, Example 7.5.1
#' prop_test_CLT(x = 24L, n = 301L, prob = .063, alternative = 'greater') 
#' 
#' # Page 261, Example 7.6.1
#' prop_test_CLT(x = c(24L, 11L), n = c(44L, 29L), p.equal = TRUE, alternative = 'greater')
#' 
#' # Page 264, Example 7.7.1
#' aggregated_var_test(xvar = 670.81, n = 16L, null.value = 600)
#' 
#' # Page 268, Example 7.8.1
#' aggregated_var_test(xvar = c(30.62, 11.37)^2, n = 6L, alternative = 'greater')
#' # Page 270, Example 7.8.2
#' with(d732, var.test(x = CONTROL, y = SCI))
#' 
#' @name Chp567
#' @export
aggregated_z_test <- function(
    xbar, n, 
    sd,
    null.value = 0, alternative = c('two.sided', 'less', 'greater'), conf.level = .95, 
    ...
) {
  
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
      xbar0 <- xbar[1L]
      dname <- sprintf(fmt = '\u0394=%.1f', xbar0)
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
    null.value = setNames(null.value, nm = switch(length(xbar), '1' = 'mean', '2' = 'mean-difference')), 
    stderr = stderr, alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}




#' @rdname Chp567
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
      xbar0 <- xbar[1L] 
      dname <- sprintf(fmt = '\u0394=%.1f', xbar0)
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



#' @rdname Chp567
#' @export
prop_test_CLT <- function(
    x, # positive count(s)
    n,
    phat = x/n,
    prob = phat,
    p.equal = FALSE,
    null.value = 0,
    alternative = c('two.sided', 'less', 'greater'), conf.level = .95, 
    ...
) {
  
  if (!is.numeric(phat) || anyNA(phat) || any(phat < 0)) stop('Illegal sample proportion(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal total count(s)')
  if (!is.numeric(prob) || anyNA(prob) || any(prob < 0) || any(prob > 1)) stop('Illegal population proportion(s)')
  if (!missing(null.value)) {
    if (!is.numeric(null.value) || length(null.value) != 1L || anyNA(null.value)) stop('Hypothesized proportion (difference) must be len-1 number')
  }
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  alternative <- match.arg(alternative)
  
  tmp <- if (missing(x)) {
    data.frame(phat = phat, n = n, prob = prob) # vector recycling, let warn
  } else data.frame(x = x, phat = phat, n = n, prob = prob)
  phat <- tmp[['phat']]
  n <- tmp$n
  dname <- sprintf(fmt = '%.1f%% (n=%d)', 1e2*phat, n)
  
  if (length(n) == 1L) { # one sample test
    method <- 'One Sample z-test on Proportion'
    prob <- tmp[['prob']]
    stderr <- sqrt(prob*(1-prob)/n)
    phat0 <- phat
    if (missing(null.value)) null.value <- prob
    
  } else if (length(n) == 2L) { # two sample test
    method <- 'Two Sample z-test on Proportions'
    
    if (missing(prob) && length(x <- tmp[['x']]) && p.equal) {
      prob <- rep(sum(x)/sum(n), times = 2L)
    } else prob <- tmp[['prob']]
    stderr <- sqrt(prob[1L]*(1-prob[1L])/n[1L] + prob[2L]*(1-prob[2L])/n[2L])
    if (isTRUE(all.equal.numeric(phat[1L], phat[2L]))) {
      phat0 <- phat[1L] 
      dname <- sprintf(fmt = '\u0394=%.1f%%', 1e2*phat0)
    } else {
      phat0 <- phat[1L] - phat[2L]
      dname <- paste(dname, collapse = ' vs. ')
    }
    if (missing(null.value)) null.value <- prob[1L] - prob[2L]
    
  } else stop('should not come here')
  
  zstat <- (phat0 - null.value) / stderr
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
  
  cint <- phat0 + cint0 * stderr
  attr(cint, which = 'conf.level') <- conf.level
  
  ret <- list(
    statistic = setNames(zstat, nm = 'z'),
    p.value = pval, 
    conf.int = cint, 
    null.value = setNames(null.value, nm = switch(length(phat), '1' = 'proportion', '2' = 'proportion-difference')),
    stderr = stderr, alternative = alternative, method = method, 
    data.name = dname
  )
  class(ret) <- 'htest'
  return(ret)
  
}



#' @rdname Chp567
#' @export
aggregated_var_test <- function(
    xvar, n,
    null.value = 1,
    alternative = c('two.sided', 'less', 'greater'), conf.level = .95,
    ...
) {
  
  if (!is.numeric(xvar) || anyNA(xvar) || any(xvar <= 0)) stop('Illegal sample variances(s)')
  if (!is.integer(n) || anyNA(n) || any(n <= 1L)) stop('Illegal sample size(s)')
  if (!is.numeric(null.value) || length(null.value) != 1L || anyNA(null.value)) stop('Hypothesized mean (difference) must be len-1 number')
  if (!is.numeric(conf.level) || length(conf.level) != 1L || anyNA(conf.level) || conf.level < 0 || conf.level > 1) stop('\'conf.level\' must be len-1 number between 0 and 1')
  alternative <- match.arg(alternative)
  
  tmp <- data.frame(xvar = xvar, n = n)
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










