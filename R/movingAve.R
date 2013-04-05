#'Moving Averages
#'
#'Filter a regular series of data to compute a moving average.
#'
#'
#'@aliases movingAve
#'@usage movingAve(x, span = 3, order = 0, pos = "center") 
#'@param x the data to be averaged or differenced. Missing values are permitted
#'but result in missing values in the output.
#'@param span the length of the data to be averaged.
#'@param order the polynomial order for averaging. Must be less than
#'\code{span}.
#'@param pos how to position the output data relative to the value returned;
#'"center" means that the value represents the average or difference of the
#'most central value realtive to the \code{span}, "end" or "trailing" means the
#'the value is the average or difference or the preceding \code{span} values,
#'and "begin" or "leading" means the value is the average or difference or the
#'following \code{span} values.
#'@return A vector of the same legnth as \code{x} containing the averages.
#'@note For odd values of \code{span} and \code{pos} equal to "center",
#'\code{order} equal 0 or 1 give the same result.\cr In general, there is no
#'reason to use polynomial orders greater than 2 and \code{pos} should always
#'be set to "center" for polynomial orders greater than 1 to avoid strange
#'behavior due to end effects.\cr
#'
#'The weights for the averages are computed based on linear model theory (Wood
#'and Hockens, 1970). They also discuss some artifacts resulting from
#'smoothing.
#'@seealso \code{\link{filter}}, \code{\link{diff}}, \code{\link{movingDiff}}
#'@references Wood, L.C. and Hockens, S.N., 1970, Least squares smoothing
#'operators: Geophysics v. 35, no. 6, p. 1005-1019.
#'@keywords manip
#'@export
#'@examples
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
  if(pos == "center")
    retval <- filter(x, filMat[span + 1 - trunc((span + 1)/2),])
  else if(pos == "begin")
    retval <- rev(filter(rev(x), filMat[1L,], sides=1))
  else # Must be end
    retval <- filter(x, filMat[1L,], sides=1)
  return(as.vector(retval))
}
