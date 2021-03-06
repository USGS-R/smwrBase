#'Compute Hysteresis
#'
#'Compute a basis for estimating hysteresis effects in some variable related to
#'the argument \code{x}.
#'
#' @param x the sequence of observations. Missing values are permitted and will
#'be copied in the output.
#' @param step the number of previous observations to use to compute the local
#'mean. See \bold{Note}.
#' @return A numeric vector that approximates the local trend in \code{x}.
#' @note The basis for estimating hysteresis is the current value \code{x} minus
#'the mean of the previous \code{step} values. The first \code{step} values in
#'the output will be missing, and each missing value will result in \code{step}
#'plus 1 missing values. This approximates the trend in \code{x}; if \code{x} is
#'increasing in value over the previous \code{step} values, then the output will be positive
#'and the greater the relative increase, the larger the output.
#' @seealso \code{\link{anomalies}}
#' @references 
#' The use of hysteresis to help model the relations between stream water chemistry
#'and flow are described in:\cr
#'
#' Garrett, J.D., 2012, Concentrations, loads, and yields of select constituents 
#'from major tributaries of the Mississippi and Missouri Rivers in Iowa, water years 2004-2008:
#'U.S. Geological Survey Scientific Investigations Report 2012--5240, 61 p.
#'
#'Wang, P., and Linker, L.C., 2008, Improvement of regression simulation in fluvial sediment 
#'loads: Journal of Hydraulic Engineering, v. 134, no. 10, p. 1,527--1,531.
#' @keywords manip
#' @examples
#'\dontrun{
#'library(smwrData)
#'data(Q05078770)
#'# Plot flow and hysteresis to show looping 
#'with(Q05078770, plot(log(FLOW), hysteresis(log(FLOW), 3), type="l"))
#'}
#' @export
hysteresis <- function(x, step=3) {
  ## Coding history:
  ##    2013Jan10 DLLorenz Original
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  xclass <- class(x)
  if(xclass == "ts")
    x <- as.double(x)
  else if(xclass != "numeric")
    stop(paste("class of", xclass, "cannot be used in function hysteris",
               sep=" "))
  ## Construct the trailing series
  filter <- rep(1./step, step)
  trails <- filter(x, filter, sides=1)
  ## Subtract obs at i from the previous step average
  retval <- x - shiftData(trails)
  return(retval)
}
