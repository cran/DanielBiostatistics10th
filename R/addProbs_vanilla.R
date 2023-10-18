

#' @title \linkS4class{addedProbs}
#' 
#' @description ..
#' 
#' @slot .Data \link[base]{matrix}
#' 
#' @slot margin \link[base]{integer} scalar or \link[base]{vector}
#' 
#' @export 
setClass(Class = 'addedProbs', contains = 'array', slots = c(
  margin = 'integer'
))



#' @title Add Conditional and/or Marginal Probabilities to an \link[base]{integer} \link[base]{matrix}
#' 
#' @description
#' Add conditional and/or marginal probabilities to a two-way contingency table.
#' 
#' @param A \link[base]{matrix} of \link[base]{typeof} \link[base]{integer}, 
#' two-dimensional contingency table.  See \link[stats]{addmargins}
#' 
#' @param margin \link[base]{integer} scalar or \link[base]{vector}, see \link[stats]{addmargins}
#' 
#' @details 
#' 
#' Function [addProbs()] provides the joint, marginal (using `margin = 1:2`) 
#' and conditional (using `margin = 1L` or `margin = 2L`) 
#' probabilities of a two-dimensional contingency table.
#' 
#' @note 
#' \link[base]{margin.table} (which is to be renamed as \link[base]{marginSums}) 
#' is much slower than \link[base]{colSums}.
#' 
#' @returns 
#' Function [addProbs()] returns a \link[base]{noquote} \link[base]{matrix} of subclass `'addedProbs'`.
#' 
#' 
#' @seealso 
#' \link[stats]{addmargins} \link[base]{rowSums} \link[base]{colSums} \link[base]{proportions} 
#' 
#' @examples 
#' (y1 = addProbs(table(warpbreaks$tension)))
#' 
#' storage.mode(WorldPhones) = 'integer'
#' (y2 = addProbs(WorldPhones))
#' 
#' 
#' @importFrom stats addmargins
#' @export
addProbs <- function(A, margin = seq_len(nd)) {
  if (!length(A)) return(invisible())
  if (inherits(A, what = 'formula')) .Defunct('addProbs(xtabs(A, data = data, addNA = TRUE))')
  if (!is.array(A) || typeof(A) != 'integer') stop('input must be integer array')
  if (anyNA(A)) stop('do not allow NA in input!')
  
  dm <- dim(A)
  nd <- length(dm)
  if (nd > 2L) .Defunct('addProbs.ftable(as_ftable.table(A))')
  
  Ndnm <- names(dnm <- dimnames(A))
  # preferred for 'flextable' output, but disabled for 'markdown' output
  #for (i in seq_along(Ndnm)) {
  #  if (nzchar(Ndnm[i])) dimnames(A)[[i]] <- paste(Ndnm[i], '=\n', dnm[[i]])
  #}
  
  if (!is.integer(margin)) stop('Use integer `margin`, i.e., 1L instead of 1')
  
  ret <- x <- addmargins(A, margin = margin, FUN = sum, quiet = TRUE)
  
  if (identical(margin, seq_len(nd))) {
    # joint probabilities
    ret[] <- sprintf(fmt = '%d (%.1f%%)', ret, 1e2 * ret / sum(A))
    #if (nd == 1L) {
    #  ret <- array(ret, dim = c(1L, dm + 1L), dimnames = list(NULL, names(ret)))
    #} # else do nothing
  } else {
    if (length(margin) != 1L) stop('length of `margin` must be 1')
    if (margin == 1L) { # colSums
      if (nd == 1L) stop('already dealt with in the first `if`')
      ret[] <- sprintf(fmt = '%d (%.1f%%)', ret, 1e2 * t.default(t.default(ret) / .colSums(A, m = dm[1L], n = dm[2L], na.rm = FALSE)))
    } else if (margin == 2L) { # rowSums
      ret[] <- sprintf(fmt = '%d (%.1f%%)', ret, 1e2 * ret / .rowSums(A, m = dm[1L], n = dm[2L], na.rm = FALSE))
    } else stop('wont come here')
  }
  ret[x == 0L] <- '.' # otherwise too crowded
  new(Class = 'addedProbs', ret, margin = margin)
}



#' @title Show \linkS4class{addedProbs}
#' 
#' @description ..
#' 
#' @param object \linkS4class{addedProbs} object
#' 
#' @returns
#' The \link[methods]{show} method for \linkS4class{addedProbs} object 
#' does not have a returned value.
#' 
#' 
#' @export
setMethod(show, signature = signature(object = 'addedProbs'), definition = function(object) {
  print.noquote(noquote(object@.Data, right = TRUE))
})



