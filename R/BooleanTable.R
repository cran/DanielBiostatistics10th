
# Do NOT edit DanielBiostatistics10th/R/BooleanTable.R  
# Edit tzh/R/BooleanTable.R


# in English
# 'Boolean' indicates 0/1; https://en.wikipedia.org/wiki/Boolean_data_type
# 'binary' indicates base-2 numerical system, i.e., er-jin-zhi; https://en.wikipedia.org/wiki/Binary_number

# if `contains = 'table'`, then `unclass(object)` will have `attr(,'.S3Class') = 'table'`




#' @title \linkS4class{BooleanTable}: Boolean Test-&-Disease and/or Risk-&-Disease Table 
#' 
#' @description
#' ..
#' 
#' 
#' @slot .Data two-by-two \link[base]{integer} \link[base]{matrix}, 
#' contingency table of a Boolean test-&-disease table with layout
#' \tabular{lcc}{
#'  \tab Test (\eqn{+}) \tab Test (\eqn{-}) \cr
#' Disease (\eqn{+}) \tab \eqn{x_{++}} \tab \eqn{x_{+-}} \cr
#' Disease (\eqn{-}) \tab \eqn{x_{-+}} \tab \eqn{x_{--}} \cr
#' }
#' or a Boolean risk-&-disease table with layout
#' \tabular{lcc}{
#'  \tab Risk Factor (\eqn{+}) \tab Risk Factor (\eqn{-}) \cr
#' Disease (\eqn{+}) \tab \eqn{x_{++}} \tab \eqn{x_{+-}} \cr
#' Disease (\eqn{-}) \tab \eqn{x_{-+}} \tab \eqn{x_{--}} \cr
#' }
#' The endpoint (i.e., disease) must be on the rows and the test/risk be on the columns.
#' This set up is to accommodate \link[stats]{model.frame.default} and let end user use formula
#' `endpoint ~ test` or `endpoint ~ risk`.
#' 
#' @name BooleanTable
#' @aliases BooleanTable-class
#' @export
setClass(Class = 'BooleanTable', contains = 'matrix', validity = function(object) {
  x <- unclass(object)
  if (!is.integer(x) || anyNA(x) || any(x < 0L)) stop('Contingency counts must be integer matrix without missing value')
  if (any(dim(x) != 2L)) stop('must be 2*2 table')
})




#' @rdname BooleanTable
#' 
#' @param x two-by-two \link[base]{integer} \link[base]{matrix}, contingency table of two Boolean variables, 
#' or an R object convertible to a two-by-two \link[base]{integer} \link[base]{matrix}.
#' The endpoint (i.e., disease) is on rows and the test/risk is on columns.
#' 
#' @details ..
#' 
#' @returns 
#' Function [BooleanTable()] returns a \linkS4class{BooleanTable} object.
#' 
#' @seealso 
#' End-user may also use \link[caret]{confusionMatrix}, 
#' which does not provide confidence intervals of 
#' sensitivity, specificity, etc.
#' 
#' @examples 
#' x = matrix(c(7L, 3L, 8L, 6L), nrow = 2L)
#' BooleanTable(x)
#' (x1 = matrix(c(7L, 3L, 8L, 6L), nrow = 2L, dimnames = list(X = c('a','b'), NULL)))
#' BooleanTable(x1)
#' 
#' @export
BooleanTable <- function(x) {
  if (!is.matrix(x) || (typeof(x) != 'integer') || any(dim(x) != 2L)) stop('Boolean table must be 2*2 integer matrix')
  
  nm1 <- c('(+)', '(-)')
  nm2 <- c('Endpoint', 'Test/Risk')
  
  if (!length(dnm <- dimnames(x))) {
    dimnames(x) <- setNames(list(nm1, nm1), nm = nm2)
    return(new(Class = 'BooleanTable', x))
  } 
  
  if (!length(names(dnm))) {
    names(dnm) <- nm2 
  } else {
    names(dnm)[1L] <- if (!nzchar(names(dnm)[1L]) || (names(dnm)[1L] == 'Endpoint')) 'Endpoint' else trimws(paste0(names(dnm)[1L], ' [Endpoint]'))
    names(dnm)[2L] <- if (!nzchar(names(dnm)[2L]) || (names(dnm)[2L] == 'Test/Risk')) 'Test/Risk' else trimws(paste0(names(dnm)[2L], ' [Test/Risk]'))
  }
  
  if (!length(dnm[[1L]])) dnm[[1L]] <- nm1 else {
    if (!all(nzchar(dnm[[1L]]))) stop('do not allow zchar rownames')
    if (dnm[[1L]][1L] != '(+)') dnm[[1L]][1L] <- paste(dnm[[1L]][1L], '(+)')
    if (dnm[[1L]][2L] != '(-)') dnm[[1L]][2L] <- paste(dnm[[1L]][2L], '(-)')
  }
  if (!length(dnm[[2L]])) dnm[[2L]] <- nm1 else {
    if (!all(nzchar(dnm[[2L]]))) stop('do not allow zchar rownames')
    if (dnm[[2L]][1L] != '(+)') dnm[[2L]][1L] <- paste(dnm[[2L]][1L], '(+)')
    if (dnm[[2L]][2L] != '(-)') dnm[[2L]][2L] <- paste(dnm[[2L]][2L], '(-)')
  }
  dimnames(x) <- dnm
  return(new(Class = 'BooleanTable', x))
}








#' @title Show \linkS4class{BooleanTable} Object
#' 
#' @description Show \linkS4class{BooleanTable} object
#' 
#' @param object a \linkS4class{BooleanTable} object
#' 
#' @returns 
#' The \link[methods]{show} method for \linkS4class{BooleanTable} object 
#' does not have a returned value.
#' 
#' @export
setMethod(show, signature(object = 'BooleanTable'), definition = function(object) {
  print(addmargins(unclass(object)))
  return(invisible())
})




# positive predictive values
ppv <- function(prevalence, sensitivity, specificity) {
  if (!is.double(sensitivity) || length(sensitivity) != 1L || is.na(sensitivity) || sensitivity < 0 || sensitivity > 1) stop('illegal sensitivity')
  if (!is.double(specificity) || length(specificity) != 1L || is.na(specificity) || specificity < 0 || specificity > 1) stop('illegal specificity')
  if (!is.double(prevalence) || !length(prevalence) || anyNA(prevalence) ||
      any(prevalence < 0, prevalence > 1)) stop('`prevalence` must be between 0 and 1 (inclusive)')
  (sensitivity * prevalence) / (sensitivity * prevalence + (1-specificity) * (1-prevalence))
}

# negative predictive values
npv <- function(prevalence, sensitivity, specificity) {
  if (!is.double(sensitivity) || length(sensitivity) != 1L || is.na(sensitivity) || sensitivity < 0 || sensitivity > 1) stop('illegal sensitivity')
  if (!is.double(specificity) || length(specificity) != 1L || is.na(specificity) || specificity < 0 || specificity > 1) stop('illegal specificity')
  if (!is.double(prevalence) || !length(prevalence) || anyNA(prevalence) ||
      any(prevalence < 0, prevalence > 1)) stop('`prevalence` must be between 0 and 1 (inclusive)')
  (specificity * (1-prevalence)) / (specificity * (1-prevalence) + (1-sensitivity) * prevalence)
}


#' @title Summarize Boolean Test-&-Disease and/or Risk-&-Disease Table 
#' 
#' @description
#' Summarize Boolean test-&-disease and/or risk-&-disease table using 
#' sensitivity, specificity, diagnostic accuracy, predictive values, relative risk 
#' and odds ratio, 
#' together with their \eqn{95\%} Clopper-Pearson exact confidence intervals.
#' 
#' @param object a \linkS4class{BooleanTable} object
#' 
#' @param prevalence (optional) \link[base]{numeric} scalar, prevalence of disease
#' 
#' @param ... potential parameters, currently not in use 
#' 
#' @details ..
#' 
#' @returns 
#' 
#' [summary.BooleanTable] do not have a returned value.
#' 
#' @references 
#' \url{https://en.wikipedia.org/wiki/Diagnostic_odds_ratio}
#' 
#' @examples 
#' (x = array(c(95L, 10L, 31L, 82L), dim = c(2L, 2L)))
#' summary(BooleanTable(x))
#' summary(BooleanTable(x), prevalence = .14)
#' 
#' @importFrom e1071 classAgreement
#' @importFrom stats binom.test qnorm
#' @export summary.BooleanTable
#' @export
summary.BooleanTable <- function(object, prevalence, ...) {
  
  show(object); cat('\n')
  
  x <- unclass(object)
  x11 <- x[1L,1L] # (+,+)
  x00 <- x[2L,2L] # (-,-)
  xr <- .rowSums(x, m = 2L, n = 2L) # Disease (+) and (-)
  xc <- .colSums(x, m = 2L, n = 2L) # Test (+) and (-)
  
  sens <- x11 / xr[1L]
  spec <- x00 / xr[2L]
  cat(do.call(sprintf, c(list(
    fmt = 'Sensitivity: %.1f%% (=%d/%d), 95%% CI (%.1f%%, %.1f%%)\n', 
    1e2 * sens, x11, xr[1L]), as.list.default(1e2 * binom.test(x = x11, n = xr[1L])$conf.int))))
  cat(do.call(sprintf, c(list(
    fmt = 'Specificity: %.1f%% (=%d/%d), 95%% CI (%.1f%%, %.1f%%)\n',
    1e2 * spec, x00, xr[2L]), as.list.default(1e2 * binom.test(x = x00, n = xr[2L])$conf.int))))
  
  if (!missing(prevalence)) {
    if (!is.double(prevalence) || !length(prevalence) || anyNA(prevalence) ||
        any(prevalence < 0, prevalence > 1)) stop('`prevalence` must be between 0 and 1 (inclusive)')
    ppv_ <- ppv(prevalence, sensitivity = sens, specificity = spec)
    npv_ <- npv(prevalence, sensitivity = sens, specificity = spec)
    cat(sprintf(fmt = 'Positive Predictive Value: %.1f%% (prevalence %.1f%%)\n', 1e2*ppv_, 1e2*prevalence))
    cat(sprintf(fmt = 'Negative Predictive Value: %.1f%% (prevalence %.1f%%)\n', 1e2*npv_, 1e2*prevalence))
  } else {
    cat(do.call(sprintf, c(list(
      fmt = 'Positive Predictive Value (unknown prevalence): %.1f%% (=%d/%d), 95%% CI (%.1f%%, %.1f%%)\n',
      1e2 * x11/xc[1L], x11, xc[1L]), as.list.default(1e2 * binom.test(x = x11, n = xc[1L])$conf.int))))
    cat(do.call(sprintf, c(list(
      fmt = 'Negative Predictive Value (unknown prevalence): %.1f%% (=%d/%d), 95%% CI (%.1f%%, %.1f%%)\n',
      1e2 * x00/xc[2L], x00, xc[2L]), as.list.default(1e2 * binom.test(x = x00, n = xc[2L])$conf.int))))
  }

  cat('\n')
  chisq <- sum(x) * (x11*x00 - x[2L,1L]*x[1L,2L])^2 / prod(xr, xc)
  #stopifnot(all.equal(chisq, unname(chisq.test(x, correct = FALSE)$statistic)))
  
  # relative risk
  risks <- x[1L,] / .colSums(x, m = 2L, n = 2L)
  logRR <- unname(log(risks[1L]) - log(risks[2L])) # Equation (12.7.2) (Page 644), Daniel Biostatistics, 10th
  logRR_sd <- logRR / sqrt(chisq)
  cat(do.call(sprintf, args = c(list(
    fmt = 'Relative Risk: %.2f (=(%d/%d)/(%d/%d)), 95%% CI (%.2f, %.2f), p = %.3f\n',
    exp(logRR), x11, xc[1L], x[1L,2L], xc[2L]
  ), 
  as.list.default(exp(logRR + qnorm(c(.025, .975)) * logRR_sd)),
  list(pnorm(abs(logRR), sd = logRR_sd, lower.tail = FALSE)))))
  
  # odds ratio
  odds <- x[1L,] / x[2L,]
  logOR <- unname(log(odds[1L]) - log(odds[2L])) # Equation (12.7.4) (Page 646), Daniel Biostatistics, 10th
  logOR_sd <- logOR / sqrt(chisq)
  cat(do.call(sprintf, args = c(list(
    fmt = 'Odds Ratio: %.2f (=(%d/%d)/(%d/%d)), 95%% CI (%.2f, %.2f), p = %.3f\n',
    exp(logOR), x11, x[2L,1L], x[1L,2L], x00
  ), 
  as.list.default(exp(logOR + qnorm(c(.025, .975)) * logOR_sd)),
  list(pnorm(abs(logOR), sd = logOR_sd, lower.tail = FALSE)))))
  
  cat('\n')
  cat(do.call(sprintf, c(list(
    fmt = 'Diagnose Accuracy: %.1f%% (=(%d+%d)/%d, 95%% CI %.1f%%~%.1f%%)\n',
    1e2 * (x11+x00)/sum(x), x11, x00, sum(x)), as.list.default(1e2 * binom.test(x = x11+x00, n = sum(x))$conf.int))))
  
  #kp <- classAgreement(x)$kappa
  #cat(sprintf(fmt = 'Cohen\'s Inter-Rater Agreement \u03ba = %.3f (%s)\n', kp, as.character.factor(cut_kappa(kp))))
  # need to @include cut_kappa.R
  
  return(invisible(list(
    sens = sens, spec = spec
  )))
  
}



#' @title Plot of Predictive Values of Boolean Test-&-Disease Table
#' 
#' @description 
#' Plot of predictive values of Boolean test-&-disease table
#' 
#' @param object a \linkS4class{BooleanTable} object
#' 
#' @param prevalence (optional) \link[base]{numeric} scalar, prevalence of disease
#' 
#' @param ... potential parameters, currently not in use 
#' 
#' @returns 
#' [autoplot.BooleanTable] returns a \link[ggplot2]{ggplot} figure,
#' which shows the curves of positive and negative predictive values for prevalence from 0 to 1.
#' 
#' @examples 
#' (x = array(c(95L, 10L, 31L, 82L), dim = c(2L, 2L)))
#' autoplot(BooleanTable(x))
#' autoplot(BooleanTable(x), prevalence = .13)
#' 
#' @importFrom ggplot2 autoplot ggplot geom_function geom_point aes scale_y_continuous scale_x_continuous scale_colour_discrete labs
#' @importFrom ggrepel geom_label_repel 
#' @importFrom scales percent
#' @importFrom utils capture.output
#' @seealso [summary.BooleanTable]
#' @export autoplot.BooleanTable
#' @export
autoplot.BooleanTable <- function(object, prevalence, ...) {
  
  capture.output(tmp <- summary.BooleanTable(object))
  sens <- tmp$sens
  spec <- tmp$spec
  
  p <- ggplot() + 
    geom_function(mapping = aes(colour = 'ppv'), fun = ppv, args = list(sensitivity = sens, specificity = spec), xlim = c(0, 1)) + 
    geom_function(mapping = aes(colour = 'npv'), fun = npv, args = list(sensitivity = sens, specificity = spec), xlim = c(0, 1)) + 
    scale_y_continuous(labels = percent) + 
    scale_x_continuous(labels = percent) +
    scale_colour_discrete(name = 'Predictive\nValues', breaks = c('ppv', 'npv'), labels = c('Positive PV', 'Negative PV')) +
    labs(x = 'Prevalence', y = 'Predictive Value', 
         caption = sprintf(fmt = 'Sensitivity = %.1f%%; Specificity = %.1f%%', 1e2*sens, 1e2*spec))
  
  if (!missing(prevalence)) {
    if (!is.double(prevalence) || !length(prevalence) || anyNA(prevalence) ||
        any(prevalence < 0, prevalence > 1)) stop('`prevalence` must be between 0 and 1 (inclusive)')
    ppv_ <- ppv(prevalence, sensitivity = sens, specificity = spec)
    npv_ <- npv(prevalence, sensitivity = sens, specificity = spec)
    p <- p + 
      geom_point(mapping = aes(x = prevalence, y = ppv_, colour = 'ppv'), size = 2L) +
      geom_label_repel(mapping = aes(x = prevalence, y = ppv_, colour = 'ppv', label = sprintf(fmt = '%.1f%%', 1e2*ppv_)), size = 3.5) +
      geom_point(mapping = aes(x = prevalence, y = npv_, colour = 'npv'), size = 2L) + 
      geom_label_repel(mapping = aes(x = prevalence, y = npv_, colour = 'npv', label = sprintf(fmt = '%.1f%%', 1e2*npv_)), size = 3.5)
  } 
  
  return(p)
  
}
