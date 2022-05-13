

#' @title Chapter 3
#' 
#' @description 
#' 
#' Functions for Chapter 3, \emph{Some Basic Probability Concepts}.
#' 
#' @param A \link[base]{integer} \link[base]{matrix} of contingency table.  
#' For \link{predictive_value} function, this must be a 2-by-2 contingency table.
#' 
#' @param sensitivity \link[base]{numeric} scalar, sensitivity of a test
#' 
#' @param specificity \link[base]{numeric} scalar, specificity of a test
#' 
#' @param prevalence (optional) \link[base]{numeric} scalar, prevalence of a disease
#' 
#' @details 
#' 
#' \link{addProbs} provides the joint, marginal and conditional probabilities of a contingency table.

#' \link{predictive_value} provides the predictive values based on (the sensitivity and specificity of) a test, 
#' based on the 2-by-2 test-disease contingency table,
#' and the disease prevalence.
#' 
#' @return 
#' 
#' \link{addProbs} returns an \code{'addProbs'} object.
#' 
#' \link{predictive_value} returns a \code{'predictiveValue'} object.
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @seealso \link[stats]{addmargins} \link[base]{rowSums} \link[base]{colSums} \link[base]{proportions} 
#' 
#' @example inst/example/Chapter3.R 
#' 
#' @name Chapter03
#' @export
addProbs <- function(A) {
  if (anyNA(A)) stop('do not allow missingness in `A`')
  if (!is.matrix(A) || !is.integer(A)) stop('Must be 2-way contingency table in integer')
  dnm <- names(dimnames(A))
  if (length(dnm) != 2L) stop('must have names of both dimension-names')
  
  m <- addmargins(A = A)
  m[] <- sprintf(fmt = '%d (%.1f%%)', m, m / sum(A) * 100)
  m1 <- addmargins(A = A, margin = 1L)
  m1[] <- sprintf(fmt = '%d (%.1f%%)', m1, t(t(m1) / colSums(A)) * 100)
  m2 <- addmargins(A = A, margin = 2L)
  m2[] <- sprintf(fmt = '%d (%.1f%%)', m2, m2 / rowSums(A) * 100)
  
  ret <- list(
    joint = noquote(m, right = TRUE),
    margin1 = noquote(m1, right = TRUE),
    margin2 = noquote(m2, right = TRUE),
    dnm = dnm
  )
  class(ret) <- 'addProbs'
  return(ret)
}


#' @export
print.addProbs <- function(x, ...) {
  cat('\n==== Joint & Marginal Probabilities ====\n\n')
  print.noquote(x$joint)
  cat('\n==== Conditional Probabilities (on ', x$dnm[2L], ') ====\n\n', sep = '')
  print.noquote(x$margin1)
  cat('\n==== Conditional Probabilities (on ', x$dnm[1L], ') ====\n\n', sep = '')
  print.noquote(x$margin2)
  cat('\n')
  return(invisible())
}






#' @rdname Chapter03
#' @export
predictive_value <- function(A, sensitivity = A[1,1]/sum(A[,1]), specificity = A[2,2]/sum(A[,2]), prevalence = stop('must provide prevalence')) {
  if (!missing(A)) {
    if (!is.matrix(A) || !is.integer(A) || any(dim(A) != 2L)) stop('Test-Disease table must be 2*2 integer matrix')
    if (length(dnm <- dimnames(A)) != 2L) stop('must provide complete dimension names')
    if (!length(nm_dnm <- names(dnm)) || any(!nzchar(nm_dnm))) stop('dimension names must have names')
    message('Confirm that your data setup satisfies that')
    message('{', nm_dnm[1L], ' = ', dnm[[1L]][1L], '} is test-positive')
    message('{', nm_dnm[2L], ' = ', dnm[[2L]][1L], '} is disease-positive')
    #tmp <- readline(prompt = 'Press Enter to confirm, or any other key to abort: ')
    #if (nzchar(tmp)) stop('Aborted by user')
  }
  
  if (!is.numeric(prevalence) || length(prevalence) != 1L || anyNA(prevalence) ||
      prevalence < 0 || prevalence > 1) stop('`prevalence` must be between 0 and 1 (inclusive)')
  
  pvp <- (sensitivity * prevalence) / (sensitivity * prevalence + (1-specificity) * (1-prevalence))
  pvn <- (specificity * (1-prevalence)) / (specificity * (1-prevalence) + (1-sensitivity) * prevalence)
  ret <- list(Sensitivity = sensitivity, Specificity = specificity, Prevalence = prevalence, pvp = pvp, pvn = pvn)
  class(ret) <- 'predictiveValue'
  return(ret)
}

#' @export
print.predictiveValue <- function(x, ...) {
  cat(with(x, sprintf(
    fmt = 'Sensitivity = %.1f%%\nSpecificity = %.1f%%\nPrevalence = %.1f%%\nPredictive Value Positive (PVP) = %.1f%%\nPredictive Value Negative (PVN) = %.1f%%\n', 
    100 * Sensitivity, 100 * Specificity, 100 * Prevalence, 100 * pvp, 100 * pvn)))
  return(invisible(x))
}






