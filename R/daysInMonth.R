#'Days in a Month
#'
#'Computes the number of days in a month or the total number of days in the
#'year to the end of the month.
#'
#'
#' @usage daysInMonth(month, year, cum = FALSE)
#' @param month the month number, must range in value from 1 to 12. Missing
#'values are permitted.
#' @param year the calendar year, replicated in length to match \code{month}.
#'Missing values are permitted.
#' @param cum a logical value to indicate whether the cumulative days in the
#'year \code{cum=TRUE} is returned or the number of days in the month.
#' @return A vector matching \code{month} of the requested number of days.
#'Missing values are returned wherever either \code{month} or \code{year} is
#'missing.
#' @keywords manip
#' @export
#' @examples
#'
#'## Check February on a leap year and regular year.
#'daysInMonth(c(2,2), c(2000, 2001))
daysInMonth <- function(month, year, cum=FALSE) {
  ## Coding history:
  ##    2001Sep07 DLLorenz Initial coding
  ##    2006Apr05 DLLorenz Added option for summing
  ##    2011May25 DLLorenz Conversion to R and vectorized
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2012Aug17 DLLorenz Fixed for missings in year
  ##    2013Feb02 DLLorenz Prep for gitHub
  year <- as.integer(round(year, 9)) 
  year <- rep(year, length.out=length(month))
  ## Determine which are leap years--every 4 years except centuries, with
  ##  an exception made every 400 years (2000 was a leap year).
  ## No need to require lubridate for this--would require converting to
  ##  a recognized date format.
  leap <- year %% 4L == 0L & (year %% 100L != 0L | year %% 400L == 0L)
  retval <- apply(cbind(month, leap), 1L, function(x, cum) {
    month <- x[1L]
    if(is.na(x[2L]))
      return(NA)
    if(as.logical(x[2L])) {
      if(cum)
        return(sum(c(31,29,31,30,31,30,31,31,30,31,30,31)[seq(month)]))
      else
        return(c(31,29,31,30,31,30,31,31,30,31,30,31)[month])
    }
    else {
      if(cum)
        return(sum(c(31,28,31,30,31,30,31,31,30,31,30,31)[1:month]))
      else
        return(c(31,28,31,30,31,30,31,31,30,31,30,31)[month])
    }
  }, cum=cum)
  return(retval)
}

                   
