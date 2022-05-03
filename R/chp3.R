

#' @title Chapter 3
#' 
#' @description 
#' 
#' Functions for Chapter 3, \emph{Some Basic Probability Concepts} of Wayne W. Daniel's 
#' \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' 
#' @param A \link[base]{matrix} of contingency table.  
#' In \code{\link{predictive_value}}, this must be a 2-by-2 contingency table.
#' 
#' @param sensitivity \link[base]{numeric} scalar, sensitivity of a test
#' 
#' @param specificity \link[base]{numeric} scalar, specificity of a test
#' 
#' @param prevalence (optional) \link[base]{numeric} scalar, prevalence of a disease
#' 
#' @return 
#' 
#' \code{\link{addProbs}} prints the joint, marginal and conditional probabilities of a \link[base]{matrix}. No value is returned.
#' 
#' \code{\link{predictive_value}} returns the sensitivity and specificity of a test, 
#' based on the 2-by-2 test-disease contingency table.
#' If the disease prevalence is provided, the predictive values will also be provided.
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @examples 
#' # Page 69, Example 3.4.1
#' (d341 = matrix(c(28L, 19L, 41L, 53L, 35L, 38L, 44L, 60L), ncol = 2L, 
#'  dimnames = list(
#'    FamilyHx = c('none', 'Bipolar', 'Unipolar', 'UniBipolar'), 
#'    Onset = c('Early', 'Late'))))
#' class(d341) # 'matrix', i.e., a two-dimensional array
#' addProbs(d341)
#' 
#' # Page 81, Example 3.5.1
#' (d351 = matrix(c(436L, 14L, 5L, 495L), nrow = 2L, 
#'   dimnames = list(Test = c('Positive', 'Negative'), Alzheimer = c('Yes', 'No'))))
#' predictive_value(d351, prevalence = .113)
#' 
#' @name Chp3
#' @export
addProbs <- function(A) {
  if (anyNA(A)) stop('do not allow missingness in `A`')
  if (!is.matrix(A) || !is.numeric(A)) stop('Currently only deals with 2-way numeric table')
  dnm <- names(dimnames(A))
  if (length(dnm) != 2L) stop('must have names of dimension-names')
  
  x_m <- addmargins(A = A)
  x_m[] <- sprintf(fmt = '%d (%.1f%%)', x_m, x_m / sum(A) * 100)
  x_m1 <- addmargins(A = A, margin = 1L)
  x_m1[] <- sprintf(fmt = '%d (%.1f%%)', x_m1, t(t(x_m1) / colSums(A)) * 100)
  x_m2 <- addmargins(A = A, margin = 2L)
  x_m2[] <- sprintf(fmt = '%d (%.1f%%)', x_m2, x_m2 / rowSums(A) * 100)
  
  ret <- list(
    joint = noquote(x_m, right = TRUE),
    margin1 = noquote(x_m1, right = TRUE),
    margin2 = noquote(x_m2, right = TRUE),
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






#' @rdname Chp3
#' @export
predictive_value <- function(
    A, # 2*2 Test-Disease table
    sensitivity = A[1,1]/sum(A[,1]), 
    specificity = A[2,2]/sum(A[,2]), 
    prevalence
) {
  if (!missing(A)) {
    if (!is.matrix(A) || any(dim(A) != 2L)) stop('Test-Disease table must be 2*2')
    if (length(dnm <- dimnames(A)) != 2L) stop('must provide complete dimension names')
    if (!length(nm_dnm <- names(dnm)) || any(!nzchar(nm_dnm))) stop('dimension names must have names')
    message('Confirm that your data setup satisfies that')
    message('{', nm_dnm[1L], ' = ', dnm[[1L]][1L], '} is test-positive')
    message('{', nm_dnm[2L], ' = ', dnm[[2L]][1L], '} is disease-positive')
    message('')
    #tmp <- readline(prompt = 'Press Enter to confirm, or any other key to abort: ')
    #if (nzchar(tmp)) stop('Aborted by user')
  }
  
  message(sprintf(fmt = 'Sensitivity = %.1f%%', 100*sensitivity))
  message(sprintf(fmt = 'Specificity = %.1f%%', 100*specificity))
  ret <- c(Sensitivity = sensitivity, Specificity = specificity)
  if (!missing(prevalence)) {
    pvp <- (sensitivity * prevalence) / (sensitivity * prevalence + (1-specificity) * (1-prevalence))
    pvn <- (specificity * (1-prevalence)) / (specificity * (1-prevalence) + (1-sensitivity) * prevalence)
    message(sprintf(fmt = 'Predictive value positive (PVP) = %.1f%%', 100*pvp))
    message(sprintf(fmt = 'Predictive value negative (PVN) = %.1f%%', 100*pvn))
    ret <- c(ret, pvp = pvp, pvn = pvn)
  }
  return(invisible(ret))
}