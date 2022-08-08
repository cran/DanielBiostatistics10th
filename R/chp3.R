

#' @title Chapter 3
#' 
#' @description 
#' 
#' Functions for Chapter 3, \emph{Some Basic Probability Concepts}.
#' 
#' @param A \link[base]{integer} \link[base]{matrix}, two-dimensional contingency table.  
#' For \link{predictiveValues} function, this must be a \linkS4class{BooleanTest} object, 
#' or a two-by-two \link[base]{integer} \link[base]{matrix} of the same with layout as outlined in \linkS4class{BooleanTest}
#' 
#' @param sensitivity,specificity \link[base]{numeric} scalars, 
#' sensitivity and specificity of a test.
#' By default, these are calculated by the test-disease contingency table \code{A}
#' 
#' @param prevalence \link[base]{numeric} scalar or vector, prevalence(s) of disease
#' 
#' @details 
#' 
#' \link{addProbs} provides the joint, marginal and conditional probabilities of a contingency table.
#' 
#' \link{predictiveValues} provides the predictive values based on 
#' the sensitivity, specificity of a test, as well as the disease prevalence.
#' 
#' @return 
#' 
#' \link{addProbs} returns an \link{addProbs} object, which is a \link[base]{list} consisting of 
#' a \link[base]{noquote} \link[base]{matrix} of joint probabilities,
#' and two \link[base]{noquote} \link[base]{matrix} of conditional probabilities.
#' A \link[base]{print} method is defined for \link{addProbs} object.
#' 
#' \link{predictiveValues} returns a \link{predictiveValues} object, which is a 
#' \link[base]{list} of three \link[base]{double} vector elements named 
#' \code{'Prevalence'}, \code{'PVP'} and \code{'PVN'}.  
#' A \link[base]{print} method and an \link[ggplot2]{autoplot} method are defined 
#' for \link{predictiveValues} object.
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
  
  jnt <- addmargins(A = A)
  jnt[] <- sprintf(fmt = '%d (%.1f%%)', jnt, jnt / sum(A) * 100)
  cond1 <- addmargins(A = A, margin = 1L)
  cond1[] <- sprintf(fmt = '%d (%.1f%%)', cond1, t(t(cond1) / colSums(A)) * 100)
  cond2 <- addmargins(A = A, margin = 2L)
  cond2[] <- sprintf(fmt = '%d (%.1f%%)', cond2, cond2 / rowSums(A) * 100)
  
  ret <- list(
    joint = noquote(jnt, right = TRUE),
    cond1 = noquote(cond1, right = TRUE),
    cond2 = noquote(cond2, right = TRUE)
  )
  attr(ret, which = 'nm') <- dnm
  class(ret) <- 'addProbs'
  return(ret)
}


#' @export
print.addProbs <- function(x, ...) {
  dnm <- attr(x, which = 'nm', exact = TRUE)
  cat('\n==== Joint & Marginal Probabilities ====\n\n')
  print.noquote(x$joint)
  cat('\n==== Conditional Probabilities (on ', dnm[2L], ') ====\n\n', sep = '')
  print.noquote(x$cond1)
  cat('\n==== Conditional Probabilities (on ', dnm[1L], ') ====\n\n', sep = '')
  print.noquote(x$cond2)
  cat('\n')
  return(invisible())
}






#' @rdname Chapter03
#' @export
predictiveValues <- function(A, sensitivity = A[1,1]/sum(A[,1]), specificity = A[2,2]/sum(A[,2]), prevalence = stop('must provide prevalence')) {
  if (!missing(A)) {
    tmp <- if (inherits(A, what = 'BooleanTest')) A else new(Class = 'BooleanTest', A)
    cat('\n'); show(tmp); cat('\n')
  }
  
  force(sensitivity); force(specificity)
  if (!is.double(sensitivity) || length(sensitivity) != 1L || is.na(sensitivity) || sensitivity < 0 || sensitivity > 1) stop('illegal sensitivity')
  if (!is.double(specificity) || length(specificity) != 1L || is.na(specificity) || specificity < 0 || specificity > 1) stop('illegal specificity')
  if (!is.double(prevalence) || !length(prevalence) || anyNA(prevalence) ||
      any(prevalence < 0, prevalence > 1)) stop('`prevalence` must be between 0 and 1 (inclusive)')
  
  PVP <- (sensitivity * prevalence) / (sensitivity * prevalence + (1-specificity) * (1-prevalence))
  PVN <- (specificity * (1-prevalence)) / (specificity * (1-prevalence) + (1-sensitivity) * prevalence)
  ret <- list(Prevalence = prevalence, PVP = PVP, PVN = PVN)
  attr(ret, which = 'sensitivity') <- sensitivity
  attr(ret, which = 'specificity') <- specificity
  class(ret) <- 'predictiveValues'
  return(ret)
}



#' @export
print.predictiveValues <- function(x, ...) {
  sens <- attr(x, which = 'sensitivity', exact = TRUE)
  spec <- attr(x, which = 'specificity', exact = TRUE)
  cat(sprintf(fmt = 'Sensitivity = %.1f%%\nSpecificity = %.1f%%\n\n', 100 * sens, 100 * spec))
  print.data.frame(data.frame(
    Prevalence = sprintf(fmt = '%.1f%%', 100 * x$Prevalence), 
    PVP = sprintf(fmt = '%.1f%%', 100 * x$PVP), 
    PVN = sprintf(fmt = '%.1f%%', 100 * x$PVN)
  ), quote = FALSE, row.names = FALSE)
  print(autoplot.predictiveValues(x, ...))
  return(invisible(x))
}


#' @export
autoplot.predictiveValues <- function(object, ...) {
  sens <- attr(object, which = 'sensitivity', exact = TRUE)
  spec <- attr(object, which = 'specificity', exact = TRUE)
  ggplot() + autolayer.predictiveValues(object, ...) + 
    scale_y_continuous(labels = percent) + 
    scale_x_continuous(labels = percent) +
    labs(x = 'Prevalence', y = 'Predictive Values',
         caption = sprintf(fmt = 'Sensitivity %.1f%%; Specificity %.1f%%', 1e2*sens, 1e2*spec)) +
    theme_bw()
}


#' @export
autolayer.predictiveValues <- function(object, xlim = c(0, 1), n = 501L, legend_title = 'Predictive\nValues', ...) {
  if (!is.numeric(xlim) || length(xlim) != 2L || anyNA(xlim) || xlim[1L] >= xlim[2L]) stop('illegal xlim')
  xlim <- c(max(xlim[1L], 0), min(xlim[2L], 1))
  prev0 <- object[['Prevalence']]
  prev <- prev0[prev0 >= xlim[1L] & prev0 <= xlim[2L]]
  sens <- attr(object, which = 'sensitivity', exact = TRUE)
  spec <- attr(object, which = 'specificity', exact = TRUE)
  pvp <- function(x) (sens * x) / (sens * x + (1-spec) * (1-x))
  pvn <- function(x) (spec * (1-x)) / (spec * (1-x) + (1-sens) * x)
  list(
    stat_function(mapping = aes(colour = 'a'), fun = pvp, n = n, xlim = xlim, ...),
    stat_function(mapping = aes(colour = 'b'), fun = pvn, n = n, xlim = xlim, ...),
    geom_point(mapping = aes(x = prev, y = pvp(prev), colour = 'a'), size = 2L),
    geom_label_repel(mapping = aes(x = prev, y = pvp(prev), colour = 'a', label = sprintf(fmt = '%.1f%%', 1e2*pvp(prev))), size = 3.5),
    geom_point(mapping = aes(x = prev, y = pvn(prev), colour = 'b'), size = 2L),
    geom_label_repel(mapping = aes(x = prev, y = pvn(prev), colour = 'b', label = sprintf(fmt = '%.1f%%', 1e2*pvn(prev))), size = 3.5),
    scale_colour_discrete(name = legend_title, breaks = letters[1:2], labels = c('PV Positive', 'PV Negative'))
  )
}



