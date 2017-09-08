#' @title Moving Averages
#'
#' @description Implements the Savitzky-Golay (Savitzky and Golay, 1964) filter 
#'on a regular series of data to compute a moving average.
#'
#' @param x the data to be averaged or differenced. Missing values are permitted
#'but result in missing values in the output.
#' @param span the length of the data to be averaged.
#' @param order the polynomial order for averaging. Must be less than
#'\code{span}.
#' @param pos how to position the output data relative to the value returned;
#'"center" means that the value represents the average of the
#'most central value relative to the \code{span}, "end" or "trailing" means the
#'the value is the average of the preceding \code{span} values,
#'and "begin" or "leading" means the value is the average of the
#'following \code{span} values.
#' @return A vector of the same length as \code{x} containing the averages.
#' @note For odd values of \code{span} and \code{pos} equal to "center,"
#'\code{order} equal to 0 or 1 gives the same result.\cr 
#'
#'In general, there is no
#'reason to use polynomial orders greater than 2, and \code{pos} should always
#'be set to "center" for polynomial orders greater than 1 to avoid strange
#'behavior due to end effects.\cr
#'
#'The weights for the averages are computed based on linear model theory 
#'(Savitzky and Golay, 1964; Wood and Hockens, 1970). Wood and Hockens (1970) 
#'also discuss some artifacts resulting from smoothing.
#' @seealso 
#Flip for production/manual
#'\code{\link[stats]{filter}}, \code{\link[base]{diff}}, \code{\link{movingDiff}}
#\code{filter} (in stats package), \code{diff} (in base package), \code{\link{movingDiff}}
#' @references 
#'Savitzky, A., and Golay, M.J.E., 1964, Smoothing and differentiation of data 
#'by simplified least squares procedures: Analytical Chemistry, v. 36, no. 8, p. 
#'1627--1639.\cr
#'
#'Wood, L.C., and Hockens, S.N., 1970, Least squares smoothing
#'operators: Geophysics, v. 35, no. 6, p. 1005--1019.
#' @keywords manip
#' @export
#' @examples
#'
#'## Construct a simple valley
#'movingData <- abs(seq(-5, 5))
#'movingAve(movingData, span=5)
#'movingAve(movingData, span=5, order=2)
movingAve <- function(x, span=3, order=0, pos="center") {
  ## Coding history:
  ##    2009Aug17 DLLorenz Original Coding
  ##    2012May24 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb03 DLLorenz Prep for gitHub
  ##
  ## get the correct position
  pos <- match.arg(pos, c("center", "begin", "end", "leading", "trailing"))
  if(pos == "leading")
    pos <- "begin"
  else if(pos == "trailing")
    pos <- "end"
  if(order >= span)
    stop("the value for order must be less than the value for span")
  ## Construct the filter matrix
  ## Note that for order greater than 0, the construction of the matrix is
  ##  based on linear model theory
  if(order > 0) {
    X <- cbind(1, poly(seq(span), order))
    filMat <- X %*% solve(crossprod(X)) %*% t(X)
  }
  else
    filMat <- matrix(1/span, ncol=span, nrow=span)
  if(span > length(x)) # need to protect against failure in filter
    retval <- rep(NA_real_, length(x))
  else if(pos == "center")
    retval <- stats::filter(x, filMat[span + 1 - trunc((span + 1)/2),])
  else if(pos == "begin")
    retval <- rev(stats::filter(rev(x), filMat[1L,], sides=1))
  else # Must be end
    retval <- stats::filter(x, filMat[1L,], sides=1)
  return(as.vector(retval))
}
