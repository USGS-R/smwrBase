#'Anomalies
#'
#'Decompose a series of observations into deviations (anomalies) from the mean
#'for selected periods and the remainder (HFV or high frequency variation)
#'using the method described in Appendix A of Vecchia(2000).
#'
#'The intent of computing anomalies is to give flexibility in fitting the
#'relation between flux, or concentration, and flow for time periods longer
#'than a couple of years. Taking a very simple regression model:\cr
#'
#'C = B0 + B1 * Q + e,\cr
#'
#'where C is the concentration, B0 and B1 are the regression coefficients, Q is
#'the flow, and e is the error. this can be re-expressed in terms of flow
#'anomalies:\cr
#'
#'C = B0 + B1 * Qbar + B1 * A5 + B1 * A1 + B1 * HVF + e,\cr
#'
#'where C, B0, B1, and e are the same as the simple regression, and Qbar is the
#'mean flow, A5 is the 5-year anomaly, A1 is the 1-year anomaly, and HVF is the
#'high-frequency variation. The simple regression model assumes that the
#'regression coefficient (B1) is constant for all anomalies. Computing
#'anomalies removes that constraint and is represented by this model:\cr
#'
#'C = B0 + B1 * A5 + B2 * A1 + B3 * HVF + e,\cr
#'
#'where C, A5, A1, HVF, and e are the same as the re-expressed model, and B0,
#'B1, B2, and B3 are regression coefficients (numerically different from the
#'simple coefficients). Qbar is a constant and is not needed for the
#'regression.\cr
#'
#'Anomalies are computed sequentially. First the mean of x is computed and
#'subtracted from the data. Then for each anomaly, the running mean of the
#'specified period is computed (the anomaly) and is subtracted from the data.
#'The remainder is the HFV. This procedure ensures that the sum of the
#'anomalies plus the mean is equal to the original data.
#'
#'@usage anomalies(x, ...)
#'@param x a time series (ts) or a vector of observations that represents a
#'daily series. Missing values (NAs) are allowed only at the beginning and end
#'of the series.
#'@param \dots named anomalies and the length of the selected periods,
#'generally in days. The anomalies must be specified in order of decreasing
#'length.
#'@return A matrix of the specified anomalies and HFV. The mean of x is
#'included as an attribute.
#'@note The output matrix contains missing values in the beginning,
#'corresponding to the length of the longest anomaly.\cr
#'
#'A long time-frame anomaly that users may be interested, include the 5-year
#'anomaly, which is 1826 days.
#'@references Vecchia, A.V., 2000, Water-quality trend analysis and sampling
#'design for the Souris River, Saskatchewan, North Dakota, and Manitoba: U.S.
#'Geological Survey Water-Reources Investigations Report 00-4019, 77 p.
#'@keywords manip
#'@examples
#'
#'library(USGSwsData)
#'data(Q05078770)
#'anomalies(log(Q05078770$FLOW), A3mo=90)
#'

anomalies <- function(x, ...) {
  ## Coding history:
  ##    2004Nov16 DLLorenz Original
  ##    2004Nov23 DLLorenz Clean up code
  ##    2005Jul14 DLLorenz Date fix
  ##    2011Apr26 DLLorenz Conversion to R
  ##    2011Sep07 DLLorenz Change algorithm from sequential to differential
  ##    2012Feb08 DLLorenz Bug Fixes.
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  ## Break down a time-series into components
  xclass <- class(x)
  if(xclass == 'ts')
    x <- as.double(x)
  else if(xclass != 'numeric')
    stop(paste("class of", xclass, "cannot be used in function anomalies", sep=" "))
  dots <- list(...)
  xmean <- mean(x, na.rm=TRUE) # missing values allowed at ends
  x <- x - xmean
  n <- length(x)
  nano <- length(dots)
  iano <- unlist(dots)
  if(nano > 1L && any(diff(iano) >= 0))
    stop("The anomalies must be strictly decreasing in the order specified")
  retval <- matrix(0.0, nrow=n, ncol=nano+1L)
  ## Compute the individual running averages
  for(i in seq(nano)) {
    nf <- iano[i]
    if(nf > n)
      stop("filter is longer than data series")
    filter <- rep(1./nf, nf)
    retval[, i] <- filter(x, filter, sides=1)
  }
  retval[,nano+1] <- x
  ## Now back out the differential residuals
  for(i in seq(nano + 1, 2L, -1L))
    retval[, i] <- retval[, i] - retval[, i - 1L]
  dimnames(retval) <- list(NULL, c(names(dots), "HFV"))
  attr(retval, "mean") <- xmean
  return(retval)
}
