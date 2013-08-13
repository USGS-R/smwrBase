#'Moving Differences
#'
#'Filter a regular series of data to compute a moving difference.
#'
#'
#' @param x the data to be averaged or differenced. Missing values are permitted
#'but result in missing values in the output.
#' @param span  the span of the differences.
#' @param pos how to position the output data relative to the value returned;
#'"center" means that the value represents the average or difference of the
#'most central value realtive to the \code{span}, "end" or "trailing" means the
#'the value is the average or difference or the preceding \code{span} values,
#'and "begin" or "leading" means the value is the average or difference or the
#'following \code{span} values.
#' @return A vector of the same legnth as \code{x} containing the differences.
#' @note For odd values of \code{span} and \code{pos} equal to "center",
#'\code{order} equal 0 or 1 give the same result.\cr 
#' @seealso \code{\link{filter}}, \code{\link{diff}}, \code{\link{movingAve}}
#' @keywords manip
#' @export
#' @examples
#'
#'## Construct a simple valley
#'movingData <- abs(seq(-5, 5))
#'movingDiff(movingData, span=1)
movingDiff <- function(x, span=1, pos="end") {
  ## Coding history:
  ##   2009Aug17 DLLorenz Original Coding
  ##   2012May24 DLLorenz Conversion to R
  ##   2013Feb03 DLLorenz Prep for gitHub.
  ##
  ## get the correct position
  pos <- match.arg(pos, c("center", "begin", "end", "leading", "trailing"))
  if(pos == "leading")
    pos <- "begin"
  else if(pos == "trailing")
    pos <- "end"
  offset <- span
  retval <- diff(x, lag=span)
  ## set up positions for returns and offsets
  leadfil <- endfil <- NA
  if(pos == "center") 
    retval <- c(rep(leadfil, offset %/% 2), retval,rep(endfil, offset - offset %/% 2))
  else if(pos == "begin")
    retval <- c(retval, rep(endfil, offset))
  else # Must be end
    retval <- c(rep(leadfil, offset), retval)
  retval
}
