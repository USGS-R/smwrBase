#'Fourier Series Components
#'
#'Compute sine and cosine terms for describing annual or daily variations.
#'
#'The argument \code{x} can be expressed as decimal time, either annual or
#'diel; or it can be an object of class "Date," "POSIXct," or "POSIXlt" in
#'which case it will be converted to annual decimal time using the
#'\code{dectime} function.
#'
#' @param x a numeric vector where one unit specifies the period. See
#'\bold{Details}. Missing values are permitted.
#' @param k.max the maximum number of paired sine and cosine terms specifying
#'the order of the Fourier series.
#' @return A matrix of the sine and cosine terms corresponding to the
#'value---two terms are computed for each value of k from 1 to \code{k.max}: sine(k 2
#'pi \code{x}) and cosine(k 2 pi \code{x}). The value of \code{k.max} is included as an attribute.
#' @note Water-quality data commonly follow a sinusoidal variation throughout a
#'yearly cycle.  A Fourier series of order one to three is generally enough to
#'adequately describe that variation for many constituents.
#' @seealso \code{\link{dectime}}
#' @keywords manip
#' @export
#' @examples
#'
#'# compute the sine and cosine terms for quarters of 2002
#'fourier(2002 + (0:3)/4)
#'#           sin(k=1)      cos(k=1) 
#'# [1,]  3.54692e-014  1.00000e+000
#'# [2,]  1.00000e+000  7.08886e-013
#'# [3,] -3.65749e-013 -1.00000e+000
#'# [4,] -1.00000e+000 -3.78606e-013
#'# attr(, "k.max"):
#'# [1] 1
#'# Compare to 2 cycles per year:
#'fourier(2002 + (0:3)/4, 2)
#'#            sin(k=1)      cos(k=1)     sin(k=2) cos(k=2)
#'#[ 1,]  3.546924e-14  1.000000e+00 7.093848e-14        1
#'# [2,]  1.000000e+00  7.088855e-13 1.417771e-12       -1
#'# [3,] -3.657492e-13 -1.000000e+00 7.314983e-13        1
#'# [4,] -1.000000e+00 -3.786056e-13 7.572112e-13       -1
#'# attr(,"k.max")
#'# [1] 2
#'
fourier <- function(x, k.max=1) {
  ## Coding history:
  ##    2004Nov16 DLLorenz Original
  ##    2011Apr27 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  ## Convert x to dectime if it is dateLike
  if(isDateLike(x))
    x <- dectime(x)
  ## Compute Fourier Series predictors
  retval <- matrix(0.0, nrow=length(x), ncol=k.max * 2)
  Names <- character(k.max*2)
  for(i in seq(k.max)) {
    retval[, 2L * i - 1L] <- sin(2*i*pi*x)
    Names[2L * i - 1L] <- paste("sin(k=",i,")", sep="")
    retval[, 2L * i] <- cos(2*i*pi*x)
    Names[2L * i] <- paste("cos(k=",i,")", sep="")
  }
  dimnames(retval) <- list(NULL, Names)
  attr(retval, "k.max") <- k.max
  return(retval)
}
