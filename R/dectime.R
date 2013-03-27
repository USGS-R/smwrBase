#'Decimal Time
#'
#'Convert time data to be expressed as year and fractional part of year. This
#'can be useful for plotting or representing time in a regression model.
#'
#'The format for \code{times} must be one of "hm," "hms," or "ms." Note that
#'this is actually a conversion function, see \bold{Seealso}.
#'
#'@usage dectime(dates, times, time.format, date.format)
#'@param dates a vector of a valid date object, or character representation of
#'dates. Missing values are permitted and produce corresponding missing
#'values in the output.
#'@param times a character representation of times. Missing values are
#'permitted and produce corresponding missing values in the output.
#'@param time.format format to convert \code{times}. See \bold{Details}.
#'@param date.format format to convert \code{dates} if character.
#'@return A vector representation of the data in decimal format--year and
#'decimal fraction.
#'@seealso \code{\link{hm}}, \code{\link{strptime}}
#'@keywords manip
#'@examples
#'
#'dectime("11/11/1918", date.format="%m/%d/%Y")
#'dectime(1988:1990)
#'

dectime <- function(dates, times, time.format, date.format) {
  ## Coding history:
  ##    2004Nov16 DLLorenz Original
  ##    2011May29 DLLorenz Conversion to R
  ##    2011Jul01 DLLorenz decimal_date does not handle missing values
  ##    2012Jun04 DLLorenz Added option to just pass dates if numeric
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  if(is.numeric(dates))
    return(dates)
  if(is.character(dates))
    dates <- as.Date(dates, format=date.format)
  if(!missing(times)) {
    ConvTime <- get(time.format)
    times <- ConvTime(times)
    dates <- dates + times
  }
  if(any(is.na(dates)))
    dates <- na2miss(dates, "0000-01-01") # Force to 0
  retval <- decimal_date(dates)
  retval[retval == 0] <- NA
  return(retval)
}
