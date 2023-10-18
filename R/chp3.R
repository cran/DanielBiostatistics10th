

#' @title Chapter 3
#' 
#' @description 
#' 
#' Examples in Chapter 3, \emph{Some Basic Probability Concepts}.
#' 
# @param A \link[base]{integer} \link[base]{matrix}, two-dimensional contingency table.  
# @param margin \link[base]{integer} scalar or vector, see \link[stats]{addmargins} for detail
# @details 
# 
# \link{addProbs} provides the joint, marginal (using \code{margin = 1:2}) 
# and conditional (using \code{margin = 1L} or \code{margin = 2L}) 
# probabilities of a contingency table.
# 
#' @return 
#' This is an example-only documentation.
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
# @seealso \link[stats]{addmargins} \link[base]{rowSums} \link[base]{colSums} \link[base]{proportions} 
#' 
#' @example inst/example/Chapter3.R 
#' 
#' @name Chapter03
# @export



if (FALSE) {
  addProbs <- function(A, margin = seq_along(dim(A))) {
    if (!is.matrix(A) || !is.integer(A) || anyNA(A)) stop('Input `A` must be 2-way contingency table in integer, without missingness')
    dnm <- dimnames(A)
    if (length(dnm) != 2L || any(!lengths(dnm))) stop('must have complete of dimension-names')
    
    if (!is.integer(margin)) stop('Use integer `margin`, i.e., 1L instead of 1')
    
    if (identical(margin, 1:2)) {
      ret <- adm <- addmargins(A = A)
      ret[] <- sprintf(fmt = '%d (%.1f%%)', ret, ret / sum(A) * 1e2)
    } else if (identical(margin, 1L)) {
      ret <- adm <- addmargins(A = A, margin = 1L)
      ret[] <- sprintf(fmt = '%d (%.1f%%)', ret, t.default(t.default(ret) / colSums(A)) * 100)
    } else if (identical(margin, 2L)) {
      ret <- adm <- addmargins(A = A, margin = 2L)
      ret[] <- sprintf(fmt = '%d (%.1f%%)', ret, ret / rowSums(A) * 100)
    } else stop('illegal `margin`')
    
    ret[adm == 0L] <- '.' # otherwise too crowded
    
    return(noquote(ret, right = TRUE))
  }
  
  
  
  
  
  
}
