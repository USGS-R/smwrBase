#'Regular Series
#'
#'Some time-series analyses require data that are uniformly spaced in time.
#'Theis function will construct a regular series from randomly spaced data
#'using any of several user-definable methods.
#'
#'For \code{regularSeries}, if there is no observation during a period, then that
#'value is set to \code{NA}. If there is one observation, then the value is set
#'to the value of that single observation. The value of which controls how
#'periods with multiple observations are handled. Three character strings are
#'recognized for selecting a single value: "earliest" selects the earliest
#'observation in the period, "middle" selects the observation closest to the
#'middle of the period, and "latest" selects the latest observation in the
#'period. If which is not one of these, then it should be the name of a
#'function such as mean or median.\cr
#'
#' @param x a vector of observations that represents a series.
#' @param times a date like vector corresponding to data.
#' @param period character string that is valid input to the the POSIXct method
#'for seq is OK, specifying the spacing between successive periods. For example
#'"year," "month," or "day."
#' @param which a character string indicating the method to use, or the name of
#'a function. See \bold{Details} for options.
#' @param begin the beginning date as POSIXt or as character.
#' @param end the end date as POSIXt or as character.
#' @param k.period the number of units of \code{period} in each period of the
#'output series.
#' @return The function \code{regularSeries} returns a data frame with the
#'following columns:
#'\item{Season}{the season number.}
#'\item{SeasonStartDate}{the starting data of the corresponding season
#'number.}
#'\item{SeasonEndDate}{the end data of the corresponding season number.}
#'\item{Value}{the value from \code{x} for the corresponding season number.}
#'\item{ValueDate}{the date from \code{times} for the corresponding season
#'number if \code{which} was one of "earliest," "middle," or "latest,"
#'otherwise missing.}
#' @seealso Refer to the documentation for \code{seaken} in the USGSwsStats
#'package if it is installed.
#' @keywords manip
#' @export
#' @examples
#'
#'library(USGSwsData)
#'data(QW05078470)
#'with(QW05078470, regularSeries(P00665, DATES))
#'# there should be no values for season numbers 2, 5, or 10
regularSeries <- function(x, times, period="month", which = "middle",
                          begin, end, k.period=1) {
  ## Coding history:
  ##    2005Jul14 DLLorenz Initial dated version
  ##    2008Dec05 DLLorenz Bug fix and added to USGS library
  ##    2011May27 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb02 DLLorenz USe POSIXlt rather than ct
  ## 
  ## Force times to POSIXlt format
  times <- as.POSIXlt(times)
  if(missing(begin))
    begin <- floor_date(min(times, na.rm=TRUE), unit=period)
  if(missing(end)) {
    end <- floor_date(max(times, na.rm=TRUE), unit=period)
    ## This construct is needed because there is no way to represent a difftime in units
    ## of a month or a year (differing number of seconds in those periods).
    end <- seq(end, by=period, length=k.period+1)[k.period+1]
  }
  ## Remove the missing values in x from consideration
  drop <- is.na(x)
  if(any(drop)) {
    x <- x[!drop]
    times <- times[!drop]
  }
  ## Create the sequence of 'seasons' and assign each x to one
  cutdates <- seq(begin, end, by = period)
  if(k.period > 1) # trim those not equal to the k.period multiple
    cutdates <- cutdates[(seq(0, by=1, length=length(cutdates)) %% k.period) == 0L]
  seasons <- cut(times, cutdates, start.on.monday = FALSE, labels=FALSE)
  ## Assign a value or NA to each element in the regular series
  nobs <- length(cutdates) - 1L
  ## Some seasons may have more than a single observation, so process as 
  ## instructed by which
  ## Modify the seasons vector so that there is a unique observation in
  ## each season
  if(is.character(which)) {
    ## Select which observation to use
    seasons.dupes <- unique(seasons[duplicated(seasons, incomparables=NA)])
    if(length(seasons.dupes) > 0L) {
      for(i in seasons.dupes) {
        test.obs <- which(seasons == i)
        check.data <- as.double(times[test.obs])
        test.julian <- switch(which,
                              earliest = as.double(cutdates[i]),
                              middle = (as.double(cutdates[i]) + as.double(cutdates[i + 1]))/2. + 0.01,
                              latest = as.double(cutdates[i + 1]))
        ## Here's an easy way to get the minimum of the vector
        obs.midrange <- mahalanobis(as.matrix(check.data), as.matrix(test.julian), as.matrix(1))
        obs.midrange <- obs.midrange == min(obs.midrange)
        ## OK, replace the data
        seasons[test.obs] <- as.double(obs.midrange) * i
      }
    }
    ## Replace NAs with 0
    seasons[is.na(seasons)] <- 0
    ret.data <- x[match(1:nobs, seasons)]
    ret.times <- times[match(1:nobs, seasons)]
  }
  else {
    ## Must be a function like mean
    temp <- tapply(x, seasons, FUN = which)
    ## Protect against NAs in seasons
    temp.ndx <- as.numeric(names(temp))
    temp.sel <- !is.na(temp.ndx)
    ret.data <- rep(NA, nobs)
    ret.data[temp.ndx[temp.sel]] <- temp[temp.sel]
    ret.times <- rep(as.POSIXlt(NA), nobs)
  }
  return(data.frame(Season=seq(along=ret.data), SeasonStartDate=cutdates[-(nobs+1)],
                    SeasonEndDate=cutdates[-1], Value=ret.data, ValueDate=ret.times))
}
