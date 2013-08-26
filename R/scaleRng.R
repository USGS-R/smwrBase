#'Scale Data
#'
#'Transforms numeric data to a specified range
#'
#' @param x any numeric vector. Missing values are permitted and
#'result in missing values in the corresponding output.
#' @param Min the minimum of the output range.
#' @param Max the maximum of the output range.
#' @param x.range the input range to map to the output range.
#' @return A numeric vector scaled to the specified range.
#' @note Some applications suggest or require data scaled to a consistent range.
#'The function \code{scaleRng} will do that and can be used to back-transform
#'the data.
#' @keywords manip
#' @export
#' @examples
#'
#'## simple case with back-transform
#'scaleRng(c(1.2, 2.3, 3.4, 5.6))
#'scaleRng(c(0.00, 0.25, 0.50, 1.00), Min=1.2, Max=5.6)
#'## now set the expected ranges
#'scaleRng(c(1.2, 2.3, 3.4, 5.6), x.range=c(1, 6))
#'scaleRng(c(0.04, 0.26, 0.48, 0.92), Min=1, Max=6, x.range=c(0,1))
scaleRng <- function(x, Min=0, Max=1, x.range=range(x, na.rm=TRUE)) { 
  ## Coding history:
  ##    2011Jun21 DLLorenz Original Coding
  ##    2011Nov07 DLLorenz Modifed to retrun if x is contstant
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb11 DLLorenz Prep for gitHub
  ##    2013Aug26 DLLorenz Modifed for missing values
  ##
  ## missing values are not allowed in x--the approx function will fail 
  if(x.range[1L] == x.range[2L])
    return((0 * x) + (Min + Max)/2)
  m <- (Max - Min)/(x.range[2L] - x.range[1L])
  return((x - x.range[1L]) * m + Min) 
 }
