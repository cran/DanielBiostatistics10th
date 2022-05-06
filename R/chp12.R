

#' @title Chapter 12
#' 
#' @description 
#' 
#' Functions for Chapter 12, \emph{The Chi-Square Distribution and The Analysis of Frequencies} 
#' of Wayne W. Daniel's 
#' \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' 
#' @param A a 2-by-2 \link[base]{matrix}, contingency table of risk factor and disease status
#' 
#' @return
#' 
#' \code{\link{relativeRisk}} returns a \code{'logRelativeRisk'} object.
#' 
#' \code{\link{oddsRatio}} returns a \code{'logOddsRatio'} object.
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @example inst/example/chp12.R 
#' 
#' @name Chp12
#' @export
relativeRisk <- function(A) {
  .inspect_2by2(A)
  risks <- A[,1L] / .rowSums(A, m = 2L, n = 2L)
  chisq <- unname(chisq.test(A, correct = FALSE)$statistic)
  # all.equal(chisq, sum(A) * (A[1L,1L]*A[2L,2L] - A[2L,1L]*A[1L,2L])^2 / prod(rowSums(A), colSums(A)))
  
  # Equation (12.7.2) (Page 644)
  logRR <- unname(log(risks[1L]) - log(risks[2L]))
  stderr <- logRR / sqrt(chisq)
  ret <- list(
    coefficients = c(logRR = logRR), 
    vcov = array(stderr^2, dim = c(1L, 1L), dimnames = list('logRR', 'logRR')))
  class(ret) <- 'logRelativeRisk'
  return(ret)
}


#' @rdname Chp12
#' @export
oddsRatio <- function(A) {
  .inspect_2by2(A)
  odds <- A[,1L] / A[,2L]
  chisq <- unname(chisq.test(A, correct = FALSE)$statistic)
  # Equation (12.7.4) (Page 646)
  logOR <- unname(log(odds[1L]) - log(odds[2L]))
  stderr <- logOR / sqrt(chisq)
  ret <- list(
    coefficients = c(logOR = logOR), 
    vcov = array(stderr^2, dim = c(1L, 1L), dimnames = list('logOR', 'logOR')))
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


.inspect_2by2 <- function(A) {
  if (!is.matrix(A) || !is.integer(A) || !all(dim(A) == 2L) || anyNA(A) || any(A <= 0L)) stop('A must be 2*2 integer matrix')
  dnm <- dimnames(A)
  dimnm <- names(dnm)
  if (length(dimnm) == 2L && !all(nzchar(dimnm))) stop('name of dimensions must be specified')
  if (is.null(rnm <- dnm[[1L]]) || is.null(cnm <- dnm[[2L]])) stop('A must have colnames and rownames')
  message(sprintf('Risk Factor %s: ', sQuote(dimnm[1L])), paste0(rnm, '(', c('+', '-'), ')', collapse = ' vs. '))
  message(sprintf('Disease Status %s: ', sQuote(dimnm[2L])), paste0(cnm, '(', c('+', '-'), ')', collapse = ' vs. '))
}

