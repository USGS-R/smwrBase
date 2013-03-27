#'Compute Hysteresis
#'
#'Compute a basis for estimating hysteresis in some variable rlated to
#'\code{x}.
#'
#'@usage hysteresis(x, step=3)
#'@param x the sequence of observations. Missing values are permitted and will
#'be copied in the output.
#'@param step the number of previous observations to use to compute the local
#'mean. See \bold{Note}.
#'@return The observations in \code{x} with missing values replaced by
#'interpolation.
#'@note The basis for estimating hysterisis is the current value \code{x} minus
#'the mena of the previous \code{step} values. The first \code{step} values in
#'the output will be missing, and each missing value will result in \code{step}
#'plus 1 missing values.
#'@seealso \code{\link{anomalies}}
#'@references Garrett, J.D., 2012, Concentrations, loads, and yields of select constituents 
#'from major tributaries of the Mississippi and Missouri Rivers in Iowa, water years 2004-2008:
#'U.S. Geological Survey Scientific Investigations Report 2012--5240, 61 9.
#'Wang, P., and Linker, L.C., 2008, Improvement of regression simulation in fluvial sediment 
#'loads: Journal of Hydraulic Engineering, v. 134, no. 10, p. 1,527--1,531.
#'@keywords manip
#'@examples
#'
#'library(USGSwsData)
#'data(Q05078770)
#'# Plot flow and hysteresis to show looping 
#'with(Q05078770, plot(log(FLOW), hystersis(log(FLOW), 3), type='l'))
#'

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
