
# in English
# 'Boolean' indicates 0/1
# 'binary' indicates base 2 numerical notation, i.e., er-jin-zhi

# if \code{contains = 'table'}, then \code{unclass(object)} will have \code{attr(,'.S3Class') = 'table'}

validity_Boolean <- function(object) {
  x <- unclass(object)
  if (!is.integer(x) || anyNA(x) || any(x < 0L)) stop('Contingency counts must be integer matrix without missing value')
  if (any(dim(x) != 2L)) stop('must be 2*2 Test-Disease Table')
}


#' @title Boolean Test-&-Disease Table 
#' 
#' @slot .Data two-by-two \link[base]{integer} \link[base]{matrix}, contingency table of a Boolean
#' test-&-disease decisions with layout
#' \tabular{lcc}{
#'  \tab Disease (\eqn{+}) \tab Disease (\eqn{-}) \cr
#' Test (\eqn{+}) \tab \eqn{x_{++}} \tab \eqn{x_{+-}} \cr
#' Test (\eqn{-}) \tab \eqn{x_{-+}} \tab \eqn{x_{--}} \cr
#' }
#' 
#' @export
setClass(Class = 'BooleanTest', contains = 'matrix', validity = validity_Boolean)




#' @title Boolean Risk-&-Disease Table 
#' 
#' @slot .Data two-by-two \link[base]{integer} \link[base]{matrix}, contingency table of a Boolean
#' test-&-disease decisions with layout
#' \tabular{lcc}{
#'  \tab Disease (\eqn{+}) \tab Disease (\eqn{-}) \cr
#' Risk Factor (\eqn{+}) \tab \eqn{x_{++}} \tab \eqn{x_{+-}} \cr
#' Risk Factor (\eqn{-}) \tab \eqn{x_{-+}} \tab \eqn{x_{--}} \cr
#' }
#' 
#' @export
setClass(Class = 'BooleanRisk', contains = 'matrix', validity = validity_Boolean)



show_Boolean <- function(object, type = c('Test', 'Risk')) {
  type <- match.arg(type)
  x <- unclass(object)
  txt <- c('(+)', '(-)')
  if (!length(dnm <- dimnames(x))) {
    dnm <- dimnames(x) <- setNames(list(txt, txt), nm = c(type, 'Disease'))
  } 
  if (!length(ndnm <- names(dnm))) ndnm <- names(dnm) <- c(type, 'Disease')
  names(dnm)[1L] <- if (!nzchar(ndnm[1L])) type else if (ndnm[1L] != type) trimws(paste0(ndnm[1L], ' [', type, ']')) else ndnm[1L]
  names(dnm)[2L] <- if (!nzchar(ndnm[2L])) 'Disease' else if (ndnm[2L] != 'Disease') trimws(paste0(ndnm[2L], ' [Disease]')) else ndnm[2L]
  
  if (!length(dnm[[1L]])) dnm[[1L]] <- txt else {
    if (!all(nzchar(dnm[[1L]]))) stop('do not allow zchar rownames')
    if (dnm[[1L]][1L] != '(+)') dnm[[1L]][1L] <- paste(dnm[[1L]][1L], '(+)')
    if (dnm[[1L]][2L] != '(-)') dnm[[1L]][2L] <- paste(dnm[[1L]][2L], '(-)')
  }
  if (!length(dnm[[2L]])) dnm[[2L]] <- txt else {
    if (!all(nzchar(dnm[[2L]]))) stop('do not allow zchar rownames')
    if (dnm[[2L]][1L] != '(+)') dnm[[2L]][1L] <- paste(dnm[[2L]][1L], '(+)')
    if (dnm[[2L]][2L] != '(-)') dnm[[2L]][2L] <- paste(dnm[[2L]][2L], '(-)')
  }
  dimnames(x) <- dnm
  print(addmargins(x))
}

#' @title Show \linkS4class{BooleanTest} Object
#' 
#' @description Show \linkS4class{BooleanTest} object
#' 
#' @param object a \linkS4class{BooleanTest} object
#' 
#' @return 
#' The \link[methods]{show} method for \linkS4class{BooleanTest} object 
#' does not have a returned value.
#' 
#' @export
setMethod(show, signature(object = 'BooleanTest'), definition = function(object) {
  show_Boolean(object, type = 'Test')
})

#' @title Show \linkS4class{BooleanRisk} Object
#' 
#' @description Show \linkS4class{BooleanRisk} object
#' 
#' @param object a \linkS4class{BooleanRisk} object
#' 
#' @return 
#' The \link[methods]{show} method for \linkS4class{BooleanRisk} object 
#' does not have a returned value.
#' 
#' @export
setMethod(show, signature(object = 'BooleanRisk'), definition = function(object) {
  show_Boolean(object, type = 'Risk')
})