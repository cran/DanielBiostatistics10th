

#' @title Chapter 11: Regression Analysis: Some Additional Techniques
#' 
#' @description 
#' 
#' Functions for Chapter 11, \emph{Regression Analysis: Some Additional Techniques}.
#' 
#' @param object \link[stats]{glm} object with \link[stats]{binomial} link function, i.e., a logistic regression model,
#' as well as one and only one \link[base]{numeric} predictor
#' 
#' @param newx (optional) \link[base]{numeric} scalar or vector, new \eqn{x}-value(s) for which the fitted response(s) are to be reported
#' 
#' @param level \link[base]{numeric} scalar, tolerance/confidence level, default .95
#' 
#' @param ... potential arguments, not in use currently
#' 
#' @return
#' 
#' Function [predict_glm_binomial] returns a `'predict_glm_binomial'` object, for which 
#' a \link[base]{print} method, an \link[ggplot2]{autolayer} and an \link[ggplot2]{autoplot} method are defined.
#' 
#' @seealso \link[stats]{predict.glm}
#' 
#' @example inst/extexample/Chapter11.R 
#' 
#' @name Chapter11
#' @importFrom stats plogis predict.glm setNames
#' @export
predict_glm_binomial <- function(object, newx, level = .95, ...) {
  if (!inherits(object, 'glm') || (object$family$family != 'binomial')) stop('input must be a simple logistic regression')
  xvar <- attr(object$terms, which = 'term.labels', exact = TRUE)
  if (length(xvar) != 1L) stop('input must contains one and only one predictor')
  if (attr(object$terms, which = 'dataClasses', exact = TRUE)[2L] != 'numeric') stop('predictor must be numeric')
  xval <- object$model[[xvar]]
  xseq <- seq.int(from = min(xval), to = max(xval), length.out = 1001L)
  newdata <- setNames(data.frame(xseq), nm = xvar)
  # this is 'prediction interval'
  pred_link <- predict.glm(object, newdata = newdata, type = 'link', se.fit = TRUE)
  pred <- plogis(with(pred_link, fit + qnorm(level + (1-level)/2) * se.fit %*% t(c(lwr = -1, fit = 0, upr = 1))))
  ret <- list(glm = object, xseq = xseq, pred = pred, level = level)
  if (!missing(newx)) {
    # check `newx`, omitted for now
    newX <- setNames(data.frame(newx), nm = xvar)
    ret[['pred_newx']] <- data.frame(x = newx, fit = predict.glm(object, newdata = newX, type = 'response', se.fit = FALSE))
  }
  class(ret) <- 'predict_glm_binomial'
  return(ret)
}  


#' @export
print.predict_glm_binomial <- function(x, ...) print(autoplot.predict_glm_binomial(x, ...))

#' @importFrom ggplot2 autoplot ggplot scale_y_continuous theme_bw
#' @importFrom scales percent
#' @export
autoplot.predict_glm_binomial <- function(object, ...) {
  ggplot() + autolayer.predict_glm_binomial(object, ...) + 
    scale_y_continuous(labels = percent) +
    theme_bw()
}


# dont use autolayer.glm; dont want to break other packages
#' @importFrom ggplot2 geom_jitter geom_smooth labs
autolayer_glm <- function(object, xlab = deparse1(trms[[3L]]), ylab = deparse1(trms[[2L]]), ...) {
  # skip the check: must be single-numeric-predictor, simple logistic regression
  dat <- object[['model']]
  trms <- object$terms
  yval <- as.numeric(eval(trms[[2L]], envir = dat)) # TRUE/FALSE wont work, must be 1/0
  xval <- eval(trms[[3L]], envir = dat)
  list(
    geom_jitter(mapping = aes(x = xval, y = yval), height = .05),
    geom_smooth(mapping = aes(x = xval, y = yval), method = 'glm', method.args = list(family = 'binomial'), se = FALSE, formula = y ~ x),
    labs(x = xlab, y = ylab)
  )
}


#' @importFrom ggplot2 autolayer geom_ribbon geom_point
#' @importFrom ggrepel geom_label_repel
#' @export
autolayer.predict_glm_binomial <- function(object, ...) {
  xseq <- object[['xseq']]
  pred <- object[['pred']]
  newx <- object[['pred_newx']]
  c(autolayer_glm(object[['glm']], ...), list(
    geom_ribbon(mapping = aes(x = xseq, ymin = pred[,1L], ymax = pred[,3L]), fill = 'grey70', alpha = .2),
    if (length(newx)) geom_point(data = newx, mapping = eval(quote(aes(x = x, y = fit))), colour = 'blue', size = 2L),
    if (length(newx)) geom_label_repel(data = newx, mapping = eval(quote(aes(x = x, y = fit, label = sprintf(fmt = '%.1f%%', 1e2*fit)))), colour = 'blue', size = 3.5)
  ))
}