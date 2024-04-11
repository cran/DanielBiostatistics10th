

#' @title Chapter 4: Probability Distributions
#' 
#' @description 
#' 
#' Functions for Chapter 4, \emph{Probability Distributions}.
#' 
#' @param size non-negative \link[base]{integer} scalar, number of trials for binomial distribution
#' 
#' @param prob \link[base]{numeric} scalar between 0 and 1, probability of success on each trial for binomial distribution
#' 
#' @param lambda positive \link[base]{numeric} scalar, mean of Poisson distribution
#' 
#' @param xlim length-two \link[base]{numeric} vector, horizontal limit of the figure
#' 
# @param type \link[base]{character} scalar, whether a bar plot of relative frequency (`'density'`, default)
# or cumulative relative frequency (`'distribution'`) should be plotted 
#' 
#' @param title \link[base]{character} scalar, title of the figure
#' 
#' @details 
#' 
#' Functions [binomBar] and [poisBar] generate bar plots of binomial and Poisson distributions.
#' 
#' @return 
#' 
#' Functions [binomBar] and [poisBar] returns a `'discreteDistBar'` object, for which 
#' a \link[base]{print} method, an \link[ggplot2]{autolayer} and an \link[ggplot2]{autoplot} method are defined.
#' 
#' 
#' @seealso \link[stats]{dbinom} \link[stats]{dpois}
#' 
#' 
#' @examples 
#' binomBar(size = 25L, prob = .1)
#' poisBar(lambda = 12, xlim = 30L)
#' 
#' @example inst/extexample/Chapter4.R  
#' 
#' @name Chapter04
#' @importFrom stats dbinom setNames
#' @export
binomBar <- function(size, prob, xlim = size, title) {
  if (!is.integer(size) || length(size) != 1L) stop('size must be len-1 integer')
  if (!is.numeric(prob) || length(prob) != 1L || is.na(prob) || prob < 0 || prob > 1) stop('prob must be between 0,1')
  if (!is.integer(xlim)) stop('xlim must be integer')
  nl <- length(xlim)
  xl <- if (nl == 1L) 0:xlim else if (nl == 2L) xlim[1L]:xlim[2L] else stop('illegal xlim')
  pr <- dbinom(setNames(nm = xl), size = size, prob = prob)
  if (missing(title)) title <- sprintf(fmt = 'Binom(n = %d, p = %.0f%%)', size, 100*prob)
  ret <- list(x = xl, pr = pr, title = title)
  class(ret) <- 'discreteDistBar'
  return(ret)
}



#' @rdname Chapter04
#' @importFrom stats dpois setNames
#' @export
poisBar <- function(lambda, xlim, title) {
  if (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda) || lambda < 0) stop('lambda must be len-1 positive numeric')
  if (!is.integer(xlim)) stop('xlim must be integer')
  nl <- length(xlim)
  xl <- if (nl == 1L) 0:xlim else if (nl == 2L) xlim[1L]:xlim[2L] else stop('illegal xlim')
  pr <- dpois(setNames(nm = xl), lambda = lambda)
  if (missing(title)) title <- sprintf(fmt = 'Poisson($lambda$ = %.1f)', lambda)
  ret <- list(x = xl, pr = pr, title = title)
  class(ret) <- 'discreteDistBar'
  return(ret)
}



#' @export
print.discreteDistBar <- function(x, ...) print(autoplot.discreteDistBar(x, ...))


#' @importFrom ggplot2 autoplot ggplot scale_y_continuous theme_bw
#' @importFrom scales percent
#' @export
autoplot.discreteDistBar <- function(object, ...) {
  ggplot() + autolayer.discreteDistBar(object, ...) + scale_y_continuous(labels = percent) + theme_bw()
}

#' @importFrom ggplot2 autolayer geom_bar labs
#' @importFrom latex2exp TeX
#' @export
autolayer.discreteDistBar <- function(object, type = c('density', 'distribution'), ...) {
  x <- object[['x']]
  pr <- object[['pr']]
  type <- match.arg(type)
  y <- switch(type, density = pr, distribution = cumsum(pr))
  ylab <- switch(type, density = 'Density', distribution = 'Cumulative Distribution')
  list(
    geom_bar(mapping = aes(x = x, y = y), stat = 'identity'),
    labs(x = NULL, y = ylab, title = TeX(object[['title']]))
  )
}



#' @title Binomial Approaching Poisson
#' 
#' @description Binomial Approaching Poisson
#' 
#' @param x \link[base]{integer} scalar, observed number of responses
#' 
#' @param lambda positive \link[base]{numeric} scalar, parameter \eqn{\lambda} of Poisson distribution
#' 
#' @param size \link[base]{integer} vector, parameter \eqn{n} of binomial distribution
#' 
#' @details 
#' Function [binom2pois] shows how binomial density approaches Poisson density when
#' \eqn{n\rightarrow\infty} and \eqn{p\rightarrow 0}, while holding a constant product \eqn{np=\lambda}.
#' 
#' @return 
#' 
#' Function [binom2pois] returns a `'binom2pois'` object, for which 
#' a \link[base]{print} method, an \link[ggplot2]{autolayer} and an \link[ggplot2]{autoplot} method are defined.
#' 
#' @seealso \link[stats]{dbinom} \link[stats]{dpois}
#' 
#' @examples 
#' binom2pois(x = 4L, lambda = 6, size = seq.int(10L, 50L, by = 10L))
#' 
#' @keywords internal
#' @export
binom2pois <- function(x, lambda, size = c(10L, 100L)) {
  if (!is.integer(size) || length(size <- unique.default(size)) < 2L || anyNA(size) || any(size < 0L)) stop('illegal `size`')
  if (!is.integer(x) || length(x) != 1L || is.na(x) || x < 0L) stop('illegal `x`')
  if (!is.numeric(lambda) || length(lambda) != 1L || is.na(lambda) || lambda <= 0) stop('illegal `lambda`')
  ret <- list(
    x = x,
    lambda = lambda, 
    size = size
  )
  class(ret) <- 'binom2pois'
  return(ret)
}

#' @importFrom stats dbinom dpois
#' @export
print.binom2pois <- function(x, ...) {
  object <- x; x <- NULL
  lambda <- object[['lambda']]
  size <- object[['size']]
  prob <- lambda / size
  x <- object[['x']]
  ret <- noquote(matrix(
    sprintf(fmt = '%.2f%%', 100*c(dbinom(x, size = size, prob = prob), dpois(x, lambda = lambda))),
    dimnames = list(
      c(sprintf(fmt = 'Binomial(n=%d, p=%.0f%%)', size, 100*prob),
        sprintf(fmt = 'Poisson($lambda$=%.1f)', lambda)),
      sprintf(fmt = 'Prob(X = %d)', x)
    )), right = TRUE)
  print.noquote(ret)
  print(autoplot.binom2pois(object))
  return(invisible(ret))
}

#' @importFrom ggplot2 autolayer geom_path geom_hline geom_point
#' @importFrom ggrepel geom_label_repel
#' @importFrom stats dbinom dpois
#' @export
autolayer.binom2pois <- function(object, ...) {
  lambda <- object[['lambda']]
  size <- object[['size']]
  x <- object[['x']]
  dbinoms <- dbinom(x, size = size, prob = lambda/size)
  seq_size <- min(size):max(size)
  list(
    geom_path(mapping = aes(x = seq_size, y = dbinom(x, size = seq_size, prob = lambda/seq_size))),
    geom_hline(yintercept = dpois(x, lambda = lambda), colour = 'red'),
    geom_point(mapping = aes(x = size, y = dbinoms), size = 2L),
    geom_label_repel(mapping = aes(x = size, y = dbinoms, label = sprintf(fmt = '%.1f%%', 1e2*dbinoms)), size = 3.5)
  )
}

#' @importFrom ggplot2 autoplot ggplot scale_y_continuous labs theme_bw
#' @importFrom latex2exp TeX
#' @importFrom scales percent
#' @export
autoplot.binom2pois <- function(object, ...) {
  ggplot() + autolayer.binom2pois(object, ...) +
    scale_y_continuous(labels = percent) +
    labs(x = 'Number of Bernoulli Trials', y = sprintf(fmt = 'Prob(X = %d)', object$x),
         title = TeX(sprintf('Bin($n$, $p$) $\\rightarrow$ Poisson($lambda=np=%.1f$) while $n\\rightarrow\\infty$ and $p\\rightarrow 0$', object$lambda))) + 
    theme_bw()
}
