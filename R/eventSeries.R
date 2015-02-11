#'Regular Series
#'
#'Some time-series analyses require data that are uniformly spaced in time.
#'This function will construct a regular series from randomly spaced events.
#'
#'If there is no observation during a period, then that value
#'is set to 0 if \code{which} is "sum" and the value previous period if
#'\code{which} is "cumsum." The initial value of the series is always 0.
#'
#' @param times a date-like vector corresponding to data.
#' @param period character string that is valid input to the the POSIXct method
#'for the \code{seq} function is accepable, specifying the spacing between 
#'successive periods. For example "year," "month," or "day."
#' @param which a character string indicating the method to use. 
#'See \bold{Details} for options.
#' @param begin the beginning date as POSIXt or as character.
#' @param end the end date as POSIXt or as character.
#' @param k.period the number of units of \code{period} in each period of the
#'output series.
#' @return The function \code{eventSeries} returns a data frame with two columns:
#'\item{DateTime}{the date and time.}
#'\item{Sum}{the sum of the number of events in the period if \code{which}
#'is "sum" or}
#'\item{CumSum}{the cumulative sum of the number of events up to and
#'including the period if \code{which} is "cumsum."}
#' @seealso Refer to the documentation for \code{seaken} in the USGSstats
#'package if it is installed.
#' @keywords manip
#' @examples
#'\dontrun{
#'library(smwrData)
#'data(QW05078470)
#'# Count the number of samples per month
#'with(QW05078470, eventSeries(DATES, "month", which="sum"))
#'}
#' @export
eventSeries <- function(times, period="hour", which = "cumsum",
                          begin, end, k.period=1) {
  ## Coding history:
  ##    2011Dec02 DLLorenz Original Coding as script
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  ## Internal vectors
  seq.by <- c("sec", "min", "hour", "day", "week", "month", "year" )
  seq.sec <- c(sec=1, min=60, hour=3600) # number  of seconds in each
  Column <- c(sum="Sum", cumsum="CumSum")
  ##
  period <- match.arg(period, seq.by)
  if(k.period > 1)
    by <- paste(k.period, period, sep=' ')
  else
    by <- period
  ## Force times to POSIXct format
  times <- as.POSIXct(times)
  if(missing(begin)) {
    if(k.period == 1)
      begin <- ceiling_date(min(times, na.rm=TRUE), unit=period)
    else if( period %in% c("year", "month", "week", "day")) {
      ## Align periods to full first period
      temp1 <- floor_date(min(times, na.rm=TRUE), unit=period)
      begin <- seq(temp1, by=by, length.out=2)[2L]
    }
    else { # Must be hour, min, or sec
      ## Align to next upper time period, days for hours
      temp1 <- min(times, na.rm=TRUE)
      temp2 <- floor_date(temp1, unit=period)
      nsec <- k.period * seq.sec[period]
      begin <- temp2 + (as.double(temp1 - temp2 + nsec) %/% nsec) * nsec
    }
  } # end of missing begin
  if(missing(end)) {
    if(k.period == 1)
      end <- ceiling_date(max(times, na.rm=TRUE), unit=period)
    else if( period %in% c("year", "month", "week", "day")) {
      temp1 <- ceiling_date(min(times, na.rm=TRUE), unit=period)
      end <-  seq(temp1, by=by, length.out=2)[2L]
    }
    else { # Must be hour, min, or sec
      ## Must capture the end time to record all
      temp1 <- max(times, na.rm=TRUE)
      temp2 <- floor_date(temp1, unit=period)
      nsec <- k.period * seq.sec[period]
      end <- temp2 + (as.double(temp1 - temp2 + nsec) %/% nsec) * nsec
    }
  }
  ## Create the series and process according to which (SLOW!)
  retval <- data.frame(DateTime=seq(begin, end, by=by))
  Count <- apply(retval, 1L, function(x) sum(times < as.POSIXct(x)))
  which <- match.arg(which, c("sum", "cumsum"))
  if(which == "sum")
    retval[[Column[which]]] <- diff(c(0L, Count))
  else
    retval[[Column[which]]] <- Count
  return(retval)
}
