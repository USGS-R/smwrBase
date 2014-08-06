#' @title Base Day
#'
#' @description Computes the 'base' day of the year, a reference value that can be 
#'used to group days for the computation of summary statistics.
#'
#' @details The 'base' day is computed so that all dates have the same reference value
#'regardless of whether the year is a leap year or not. If \code{year} is 
#'"calendar," then the factor levels begin on January 1; if \code{year} is
#'"water," then the factor levels begin on October 1; and if \code{year} is
#'"climate," then the factor levels begin on April 1.
#'
#' @param x a vector of class POSIXt, Dates, or character that represents a
#'date. Missing values are permitted.
#' @param numeric a logical value; \code{TRUE} means return the numeric value
#'of the day, \code{FALSE} means return a factor.
#' @param year a character string indicating the basis of the fator levels. See
#'\bold{Details}.
#' @return An integer value representing the 'base' day number if \code{numeric}
#'is \code{TRUE}. Otherwise a factor with levels for every day of the year.
#' @import digest lubridate memoise
#' @keywords manip
#' @export
#' @examples
#'
#'# The default numeric result
#'baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"))
#'# The result as a factor
#'baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"), numeric=FALSE)
baseDay <- function(x, numeric=TRUE, year=c("calendar", "water", "climate")) {
  ## Coding history:
  ##    2010Feb22 DLLorenz Initial coding
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2012Dec05 DLLorenz Added option to retrun factor instead of integer
  ##    2013Feb02 DLLorenz Prep for gitHub  
  ##    2014Apr14 DLLorenz added year argument
  ##
  x <- as.POSIXlt(x)
  if(numeric) {
    retval <- x$yday + 1L # Correct to counting days
    ## Adjust non leap year data to the base defeined by leap years
    retval <- ifelse(retval > 59L & !leap_year(x), retval + 1L, retval)
  }
  else {
    retval <- format(x, format="%b %d")
    year=match.arg(year)
    if(year == "calendar") {
      levels <- format(seq(as.Date("2000-01-01"), by=1, length.out=366),
                       format="%b %d")
    } else if(year == "water") {
      levels <- format(seq(as.Date("1999-10-01"), by=1, length.out=366),
                       format="%b %d")
    } else
      levels <- format(seq(as.Date("1999-04-01"), by=1, length.out=366),
                       format="%b %d")
    retval <- factor(retval, levels=levels)
  }
  return(retval)
}
