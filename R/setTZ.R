#'Set Time Zone
#'
#'Set the time zone information for dates and times.
#'
#'The time zone information should be a standard name like those described in
#'\url{"http://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones"}. For the
#'convenience of users in the United States, correct conversion is provided for
#'the time zone codes of 
#'EST, EDT, CST, CDT, MST, MDT, PST, PDT, AKST, AKDT, HAST, and HST. However,
#'time zones data in states like Arizona, where savings time is never used
#'would use time zone information specified like "America/Phoenix" to avoid
#'the possibility of setting savings time when it is not appropriate.
#'
#' @param x the date-time data, generally class "POSIXct."
#' @param TZ time zone code or time zone name, see \bold{Details}.
#' @param force.stz force standard time specified in \code{TZ}. Useful for Arizona
#'times, where daylight savings is not used, or in other cases where all times are
#'recorded as standard time. Also useful when the dates and times are recorded over
#'the transition from daylight savings time to standard time. Valid only in the US.
#' @return Data like \code{x}, but with times adjusted by the time zone information.
#' @note The timezone information is a characterisitic of the data and not of
#'each individual value. If the data in \code{x} come from different time
#'zones, then a time zone is selected from the data and used as the base---the
#'dates in \code{x} are correctly converted to the selected time zone and a
#'warning is issued.
#' @seealso \code{\link{as.POSIXct}}
#' @keywords chron manip
#' @export
#' @examples
#'
#'TestDts <- as.POSIXct(c("2010-05-28 09:50:00", "2010-11-29 15:20:00"))
#'setTZ(TestDts, c("PDT", "PST"))
#'# Try setting to different Time zones
#'setTZ(TestDts, c("PDT", "CST"))
setTZ <- function(x, TZ, force.stz=FALSE) {
  ## Coding History
  ##    2012Aug21 DLLorenz Original Coding
  ##    2014Jun10 DLLorenz Final setting of force.stz?
  ##
  ##  TZ timezone information:
  ##   if any standard US format, convert to proper
  ##   otherwise assume standard time zone, see list at
  ##   http://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones
  ## Note for Arizona or any other place that does not use
  ## DST, you must specify the standard timezone name, like "America/Phoenix"
  x <- format(x, usetz=FALSE)
  if(force.stz) {
    TZ <- pick(TZ,
               EST="America/Jamaica",
               EDT="America/New_York",
               CST="America/Managua",
               CDT="America/Chicago",
               MST="America/Phoenix",
               MDT="America/Denver",
               PST="America/Metlakatla",
               PDT="America/Los_Angeles",
               AKST="America/Anchorage",
               AKDT="America/Anchorage",
               HAST="America/Honolulu",
               HST="America/Honolulu")
  }
  else {
    TZ <- pick(TZ,
               EST="America/New_York",
               EDT="America/New_York",
               CST="America/Chicago",
               CDT="America/Chicago",
               MST="America/Denver",
               MDT="America/Denver",
               PST="America/Los_Angeles",
               PDT="America/Los_Angeles",
               AKST="America/Anchorage",
               AKDT="America/Anchorage",
               HAST="America/Honolulu",
               HST="America/Honolulu")
  }
  TZx <- unique(TZ)
  if(length(TZx) == 1L) 
    return(as.POSIXct(as.character(x), tz=TZx)) # Conversion to character is required
  ## Otherwise, must do one by one--really slow, and
  ## generates an warning because time zone is an attribute of the
  ## data not of the value.
  N <- length(x)
  retval <- rep(as.POSIXct(x[1L], tz=TZx[1L]), N)
  for(i in seq(2L, N))
    retval[i] <- as.POSIXct(x[i], tz=TZ[i])
  warning("Mixed time zone information, time corrected and all set to ", TZx[1L])
  return(retval)
}
