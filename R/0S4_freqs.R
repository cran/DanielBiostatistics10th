

#' @title S4 class \linkS4class{freqs}
#' 
#' @slot .Data \link[base]{integer} vector, frequency counts
#' 
#' @slot data.name \link[base]{character} integer, name of the data, only used in output
#' 
#' @export
setClass(Class = 'freqs', contains = 'integer', slots = c(
  #endpoint = 'character'# , # name of the variable ()
  data.name = 'character'
), validity = function(object) {
  if (!length(x <- unclass(object)) || anyNA(x) || any(x < 0L)) stop('counts must be all-non-missing integers')
  nm <- names(x)
  if (!length(nm) || anyNA(nm) || !all(nzchar(nm))) stop('illegal category names')
})

# old name 'prop'

