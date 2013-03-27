#'Fourier Series Components
#'
#'Compute sine and cosine terms for describing annual or daily variations.
#'
#'The argument \code{x} can be expressed as decimal time, either annual or
#'diel; or it can be an object of class "Date," "POSIXct," or "POSIXlt" in
#'which case it will be converted to anual decimal time using the
#'\code{dectime} function.
#'
#'@usage fourier(x, k.max = 1)
#'@param x a numeric vector where one unit specifies the period. See
#'\bold{Details}. Missing values are permitted.
#'@param k.max the maximum number of paired sine and cosine terms specifying
#'the order of the Fourier series.
#'@return A matrix of the sine and cosine terms corresponding to the
#'value---two terms are computed for each value of k from 1 to k.max: sine(k 2
#'pi x) and cosine(k 2 pi x). The value of k.max is included as an attribute.
#'@note Water-quality data commonly follow a sinusoidal variation throughout a
#'yearly cycle.  A Fourier series of order one to three is generally enough to
#'adequately describe that variation.
#'@seealso \code{\link{dectime}},
#'@keywords manip
#'@examples
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
