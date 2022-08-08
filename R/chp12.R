

#' @title Chapter 12
#' 
#' @description 
#' 
#' Functions for Chapter 12, \emph{The Chi-Square Distribution and The Analysis of Frequencies}.
#' 
#' @param A a \linkS4class{BooleanRisk} object, or 
#' a two-by-two \link[base]{integer} \link[base]{matrix} of the same with layout as outlined in \linkS4class{BooleanRisk}
#' 
#' @param O \link[base]{integer} vector, observed counts
#' 
#' @param prob \link[base]{numeric} vector, anticipated probability.
#' If missing (default), an uniform distribution across all categories are used.
#' 
#' @return
#' 
#' \link{relativeRisk} returns a \code{'logRelativeRisk'} object, for which
#' a \link[stats]{vcov} method and a \link[base]{print} method are defined.
#' 
#' \link{oddsRatio} returns a \code{'logOddsRatio'} object, for which
#' a \link[stats]{vcov} method and a \link[base]{print} method are defined.
#' 
#' \link{print_OE} prints a table with observed and expected frequencies, as well as 
#' the category-wise \eqn{\chi^2} statistics.  
#' A \link[base]{double} vector of the category-wise \eqn{\chi^2} statistics is returned invisibly.
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @example inst/example/Chapter12.R 
#' 
#' @name Chapter12
#' @export
relativeRisk <- function(A) {
  tmp <- if (inherits(A, what = 'BooleanRisk')) A else new(Class = 'BooleanRisk', A)
  cat('\n'); show(tmp); cat('\n')
  risks <- A[,1L] / .rowSums(A, m = 2L, n = 2L)
  chisq <- unname(chisq.test(A, correct = FALSE)$statistic)
  # all.equal(chisq, sum(A) * (A[1L,1L]*A[2L,2L] - A[2L,1L]*A[1L,2L])^2 / prod(rowSums(A), colSums(A)))
  
  # Equation (12.7.2) (Page 644)
  logRR <- unname(log(risks[1L]) - log(risks[2L]))
  std.err <- logRR / sqrt(chisq)
  ret <- list(
    coefficients = c(logRR = logRR), 
    vcov = array(std.err^2, dim = c(1L, 1L), dimnames = list('logRR', 'logRR')))
  class(ret) <- 'logRelativeRisk'
  return(ret)
}


#' @rdname Chapter12
#' @export
oddsRatio <- function(A) {
  tmp <- if (inherits(A, what = 'BooleanRisk')) A else new(Class = 'BooleanRisk', A)
  cat('\n'); show(tmp); cat('\n')
  odds <- A[,1L] / A[,2L]
  chisq <- unname(chisq.test(A, correct = FALSE)$statistic)
  # Equation (12.7.4) (Page 646)
  logOR <- unname(log(odds[1L]) - log(odds[2L]))
  std.err <- logOR / sqrt(chisq)
  ret <- list(
    coefficients = c(logOR = logOR), 
    vcov = array(std.err^2, dim = c(1L, 1L), dimnames = list('logOR', 'logOR')))
  class(ret) <- 'logOddsRatio'
  return(ret)
}

# ?stats::vcov
#' @export
vcov.logRelativeRisk <- function(object, ...) object[['vcov']]
#' @export
vcov.logOddsRatio <- function(object, ...) object[['vcov']]


# ?base::print
#' @export
print.logRelativeRisk <- function(x, level = .95, ...) {
  ci <- c(exp(confint(x, level = level)))
  cat(sprintf(fmt = 'Relative Risk %.2f, %.0f%% confidence interval (%.2f, %.2f)\n', 
              exp(coef(x)), 1e2*level, ci[1L], ci[2L]))
}
#' @export
print.logOddsRatio <- function(x, level = .95, ...) {
  ci <- c(exp(confint(x, level = level)))
  cat(sprintf(fmt = 'Odds Ratio %.2f, %.0f%% confidence interval (%.2f, %.2f)\n', 
              exp(coef(x)), 1e2*level, ci[1L], ci[2L]))
}




#' @rdname Chapter12
#' @export
print_OE <- function(O, prob) {
  if (!is.integer(O) || (nO <- length(O)) <= 1L || anyNA(O) || any(O < 0L)) stop('observed data must be non-negative integer')
  if (missing(prob)) {
    prob <- rep(1/nO, times = nO)
  } else {
    if (!is.numeric(prob) || length(prob) != nO || anyNA(prob) || any(prob < 0)) stop('prob must be non-negative numerics')
    prob <- prob / sum(prob)
  }
  E <- sum(O) * prob
  chisq <- (O - E)^2 / E
  ret <- cbind(
    'Observed Freq' = O,
    'Expected Freq (%)' = sprintf(fmt = '%.2f (%.2f%%)', E, 1e2*prob),
    '(O-E)^2/E' = sprintf(fmt = '%.3f', chisq)
  )
  print.noquote(noquote(ret, right = TRUE))
  cat(sprintf(fmt = '\nSum (O-E)^2/E = %.2f\n', sum(chisq)))
  return(invisible(chisq))
}
