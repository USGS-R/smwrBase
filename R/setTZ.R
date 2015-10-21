#'Set Time Zone
#'
#'Set the time-zone information for dates and times.
#'
#'The time-zone information should be a standard name like those described in
#'\url{http://en.wikipedia.org/wiki/List_of_zoneinfo_time_zones}. For the
#'convenience of users in the United States, correct conversion is provided for
#'the time-zone codes of 
#'EST, EDT, CST, CDT, MST, MDT, PST, PDT, AKST, AST, AKDT, ADT, HAST,
#'and HST. However, time data in States like Arizona, where savings 
#'time is never used, would use time-zone information specified like "
#'America/Phoenix" to avoid the possibility of setting savings time 
#'when it is not appropriate.
#'
#' @param x the date-time data, generally class "POSIXct."
#' @param TZ time-zone code or time-zone name, see \bold{Details}.
#' @param force.stz force standard time specified in \code{TZ}. Useful for Arizona
#'times, where daylight savings is not used, or in other cases where all times are
#'recorded as standard time. Also useful when the dates and times are recorded over
#'the transition from daylight savings time to standard time. Valid only in the 
#'United States. Used only when retrieving data from a single time zone.
#' @return Data like \code{x}, but with times adjusted by the time-zone information.
#' @note The time-zone information is a characterisitic of the data and not of
#'each individual value. If the data in \code{x} come from different time
#'zones, then a time zone is selected from the data and used as the base---the
#'dates in \code{x} are correctly converted to the selected time zone and a
#'warning is issued.
#' @seealso 
#Flip for production/manual
#'\code{\link[base]{as.POSIXct}}
#\code{as.POSIXct} (in base package)
#' @keywords chron manip
#' @export
#' @examples
#'
#'TestDts <- as.POSIXct(c("2010-05-28 09:50:00", "2010-11-29 15:20:00"))
#'setTZ(TestDts, c("PDT", "PST"))
#'# Try setting to different time zones
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
    TZx <- pick(TZ,
                EST="America/Jamaica",
                EDT="America/New_York",
                CST="America/Managua",
                CDT="America/Chicago",
                MST="America/Phoenix",
                MDT="America/Denver",
                PST="America/Metlakatla",
                PDT="America/Los_Angeles",
                AKST="America/Anchorage",
                AST="America/Anchorage",
                AKDT="America/Anchorage",
                ADT="America/Anchorage",
                HAST="America/Honolulu",
                HST="America/Honolulu")
  }
  else {
    TZx <- pick(TZ,
                EST="America/New_York",
                EDT="America/New_York",
                CST="America/Chicago",
                CDT="America/Chicago",
                MST="America/Denver",
                MDT="America/Denver",
                PST="America/Los_Angeles",
                PDT="America/Los_Angeles",
                AKST="America/Anchorage",
                AST="America/Anchorage",
                AKDT="America/Anchorage",
                ADT="America/Anchorage",
                HAST="America/Honolulu",
                HST="America/Honolulu")
  }
  TZx <- unique(TZx)
  if(length(TZx) == 1L) 
    return(as.POSIXct(as.character(x), tz=TZx)) # Conversion to character is required
  ## Otherwise, must do one by one--really slow, and
  ## generates an warning because time zone is an attribute of the
  ## data not of the value.
  retval <- as.POSIXct(as.character(x), tz="UTC")
  hourdiff <- c(EST=5, EDT=4, CST=6, CDT=5, MST=7, MDT=6, PST=8, PDT=7,
                AKST=9, AST=9, AKDT=8, ADT=8, HAST=10, HST=10)
  retval <- retval + hourdiff[TZ] * 3600
  names(retval) <- NULL
  warning("Mixed time zone information, time corrected and all set to UTC")
  return(retval)
}
