#' Scale Data
#'
#' Transforms numeric data to a specified range.
#'
#' The function \code{scaleRng} maps the minimum of \code{x.range} to \code{Min}
#'and the maximum of \code{x.range} to \code{Max} and uses linear interpolation 
#'for other values in \code{x}.
#'
#' @aliases scaleRng IscaleRng
#' @param x any numeric vector. Missing values are permitted and
#'result in missing values in the corresponding output.
#' @param Min the minimum of the output range.
#' @param Max the maximum of the output range.
#' @param x.range the input range to map to the output range. The default range is 
#'computed from the range of \code{x} after removing missing values.
#' @return A numeric vector scaled to the specified range.
#' @note Some applications suggest or require data scaled to a consistent range.
#'The function \code{scaleRng} will do that and can be used to back-transform
#'the data.
#' @keywords manip
#' @examples
#'
#'## simple case with back-transform
#'x.tmp <- print(scaleRng(c(1.2, 2.3, 3.4, 5.6)))
#'IscaleRng(x.tmp)
#'## now set the expected ranges
#'x.tmp <- print(scaleRng(c(1.2, 2.3, 3.4, 5.6), x.range=c(1, 6)))
#'IscaleRng(x.tmp)
#'
#' @export scaleRng
scaleRng <- function(x, Min=0, Max=1, x.range=range(x, na.rm=TRUE)) { 
  ## Coding history:
  ##    2011Jun21 DLLorenz Original Coding
  ##    2011Nov07 DLLorenz Modifed to retrun if x is contstant
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb11 DLLorenz Prep for gitHub
  ##    2013Aug26 DLLorenz Modifed for missing values
  ##    2014Jul24 DLLorenz Added attributes
  ##
  if(x.range[1L] == x.range[2L]) {
    return((0 * x) + (Min + Max)/2)
  } else {
    m <- (Max - Min)/(x.range[2L] - x.range[1L])
    retval <- (x - x.range[1L]) * m + Min
  }
  attr(retval, "Min") <- Min
  attr(retval, "Max") <- Max
  attr(retval, "x.range") <- x.range
  class(retval) <- "scaleRng"
  return(retval) 
}

#' @rdname scaleRng
#' @export IscaleRng
IscaleRng <- function(x, Min, Max, x.range) { 
  ## Coding history:
  ##    2011Jun21 DLLorenz Original Coding
  ##    2011Nov07 DLLorenz Modifed to retrun if x is contstant
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb11 DLLorenz Prep for gitHub
  ##    2013Aug26 DLLorenz Modifed for missing values
  ##    2014Jul24 DLLorenz Added attributes
  ##
  if(missing(Min))
    Min <- attr(x, "Min")
  if(missing(Max))
    Max <- attr(x, "Max")
  if(missing(x.range))
    x.range <- attr(x, "x.range")
  retval <- scaleRng(x, Min=x.range[1L], Max=x.range[2L], x.range=c(Min, Max))
  # strip attributes on return
  return(as.vector(retval) )
}
