

#' @title Chapter 9
#' 
#' @description 
#' 
#' Functions for Chapter 9, \emph{Simple Linear Regression and Correlation}.
#' 
#' @param object \link[stats]{lm} object, with one and only one \link[base]{numeric} predictor
#' 
#' @param newx (optional) \link[base]{numeric} scalar or vector, new \eqn{x}-value(s) for which the fitted response(s) are to be reported
#' 
#' @param level \link[base]{numeric} scalar, tolerance/confidence level, default .95
#' 
#' @param ... potential arguments, not in use currently
#' 
#' @return
#' 
#' \link{predict_lm} returns a \link{predict_lm} object, for which 
#' a \link[base]{print} method, an \link[ggplot2]{autolayer} and an \link[ggplot2]{autoplot} method are defined.
#' 
#' @references
#' 
#' Wayne W. Daniel, \emph{Biostatistics: A Foundation for Analysis in the Health Sciences}, Tenth Edition.
#' Wiley, ISBN: 978-1-119-62550-6.
#' 
#' @seealso \link[stats]{predict.lm}
#' 
#' @example inst/example/Chapter9.R 
#' 
#' @name Chapter09
#' @export
predict_lm <- function(object, newx, level = .95, ...) {
  if (class(object)[1L] != 'lm') stop('input must be a simple lm object')
  xvar <- attr(object$terms, which = 'term.labels', exact = TRUE)
  if (length(xvar) != 1L) stop('input must contains one and only one predictor')
  if (!all(attr(object$terms, which = 'dataClasses', exact = TRUE) == 'numeric')) stop('endpoint and predictor must all be numeric')
  xval <- object$model[[xvar]]
  xseq <- seq.int(from = min(xval), to = max(xval), length.out = 1001L)
  newdata <- setNames(data.frame(xseq), nm = xvar)
  pred <- predict.lm(object, newdata = newdata, interval = 'predict', level = level)
  conf <- predict.lm(object, newdata = newdata, interval = 'confidence', level = level)
  ret <- list(lm = object, xseq = xseq, pred = pred, conf = conf, level = level)
  if (!missing(newx)) {
    # check `newx`, omitted for now
    newX <- setNames(data.frame(newx), nm = xvar)
    ret[['pred_newx']] <- data.frame(x = newx, fit = predict.lm(object, newdata = newX, interval = 'none'))
  }
  class(ret) <- 'predict_lm'
  return(ret)
}  


#' @export
print.predict_lm <- function(x, ...) print(autoplot.predict_lm(x, ...))

#' @export
autoplot.predict_lm <- function(object, ...) {
  ggplot() + autolayer.predict_lm(object, ...) + theme_bw()
}


# dont use autolayer.lm; dont want to break other packages
autolayer_lm <- function(object, xlab = deparse1(trms[[3L]]), ylab = deparse1(trms[[2L]]), title = NULL, caption = NULL, ...) {
  # skip the check: must be all-numeric, simple linear regression
  dat <- object[['model']]
  trms <- object$terms
  yval <- eval(trms[[2L]], envir = dat)
  xval <- eval(trms[[3L]], envir = dat)
  list(
    geom_point(mapping = aes(x = xval, y = yval)),
    geom_smooth(mapping = aes(x = xval, y = yval), method = 'lm', se = FALSE, formula = y ~ x),
    labs(x = xlab, y = ylab, title = title, caption = caption)
  )
}

#' @export
autolayer.predict_lm <- function(object, ...) {
  xseq <- object[['xseq']]
  pred <- object[['pred']]
  conf <- object[['conf']]
  newx <- object[['pred_newx']]
  c(autolayer_lm(object[['lm']], ...), list(
    geom_ribbon(mapping = aes(x = xseq, ymin = pred[,2L], ymax = pred[,3L], fill = 'Prediction'), alpha = .2),
    geom_ribbon(mapping = aes(x = xseq, ymin = conf[,2L], ymax = conf[,3L], fill = 'Confidence'), alpha = .2),
    scale_fill_discrete(name = 'Intervals'),
    if (length(newx)) geom_point(mapping = aes_string(x = 'x', y = 'fit'), data = newx, colour = 'blue', size = 2L),
    if (length(newx)) geom_label_repel(mapping = aes(x = newx[['x']], y = newx[['fit']], label = sprintf(fmt = '%.1f', newx[['fit']])), colour = 'blue', size = 3.5)
  ))
}