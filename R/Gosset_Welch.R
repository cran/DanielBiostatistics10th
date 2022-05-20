
# Do NOT edit DanielBiostatistics10th/R/Gosset_Welch.R  
# Edit tzh/R/Gosset_Welch.R

# https://en.wikipedia.org/wiki/Student%27s_t-test
# https://en.wikipedia.org/wiki/Welch%27s_t-test
# https://en.wikipedia.org/wiki/Welch–Satterthwaite_equation


#' @title Two-Sample Student's \eqn{t} and Welch–Satterthwaite Equation
#' 
#' @description
#' To determine the degree of freedom, as well as the standard error,
#' of two-sample \eqn{t}-test, with or without the equal-variance assumption.
#' 
#' @param s1,s2 (optional) \link[base]{double} vectors, sample standard deviations of the two samples
#' 
#' @param v1,v2 \link[base]{double} vectors, sample variances of the two samples, 
#' default \eqn{v_1=s_1^2}, \eqn{v_2=s_2^2}.
#' 
#' @param n1,n2 \link[base]{integer} vectors, sample sizes of the two samples
#' 
#' @param var.equal \link[base]{logical} scalar, whether to treat the two variances as being equal 
#' when calculating the degree of freedom and the standard error of the mean-difference.
#' If \code{FALSE} (default), Welch–Satterthwaite equation is used.
#' If \code{TRUE}, the original two-sample t-test from William Sealy Gosset is used.
#' 
#' @return 
#' 
#' \link{Gosset_Welch} returns a \link[base]{numeric} scalar of the degree of freedom, 
#' with a \link[base]{numeric} scalar attribute \code{'stderr'} of the standard error of the mean-difference.
#' 
#' @references 
#' Student's \eqn{t}-test, \doi{10.1093/biomet/6.1.1}.
#' 
#' Welch–Satterthwaite equation, \doi{10.2307/3002019} and \doi{10.1093/biomet/34.1-2.28}.
#' 
#' @seealso \link[stats]{t.test}
#' 
#' @examples 
#' x = rnorm(32L, sd = 1.6); y = rnorm(57L, sd = 2.1)
#' vx = var(x); vy = var(y); nx = length(x); ny = length(y)
#' t.test(x, y, var.equal = FALSE)[c('parameter', 'stderr')]
#' Gosset_Welch(v1 = vx, v2 = vy, n1 = nx, n2 = ny, var.equal = FALSE)
#' t.test(x, y, var.equal = TRUE)[c('parameter', 'stderr')]
#' Gosset_Welch(v1 = vx, v2 = vy, n1 = nx, n2 = ny, var.equal = TRUE)
#' 
#' @export
Gosset_Welch <- function(s1, s2, v1 = s1^2, v2 = s2^2, n1, n2, var.equal = FALSE) {
  if (anyNA(v1) || anyNA(v2) || anyNA(n1) || anyNA(n2)) stop('do not allow missing')
  if (!is.logical(var.equal) || length(var.equal) != 1L || is.na(var.equal)) stop('`var.equal` must be len-1 logical')
  
  if (var.equal) {
    ret <- n1 + n2 - 2L
    attr(ret, which = 'stderr') <- sqrt(((n1-1L)*v1+(n2-1L)*v2) / (n1+n2-2L) * (1/n1 + 1/n2))
  } else {
    .v1 <- v1/n1
    .v2 <- v2/n2
    vp <- .v1 + .v2
    ret <- vp^2 / (.v1^2/(n1-1L) + .v2^2/(n2-1L))
    attr(ret, which = 'stderr') <- sqrt(vp)
  }
  
  return(ret)
}



