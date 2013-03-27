#'Base Day
#'
#'Computes the 'base' day of the year, a reference value that can be used to
#'group days for the computation of summary statistics.
#'
#'The 'base' day is computed so that all dates have the same reference value
#'regardless of whether the year is a leap year or not.
#'
#'@usage baseDay(x, numeric = TRUE)
#'@param x a vector of class POSIXt, Dates, or character that represents a
#'date. Missing values are permitted.
#'@param numeric a vector of class POSIXt, Dates, or character that represents
#'a date. Missing values are permitted.
#'@return An integer value representing the 'base' day number if \code{numeric}
#'is \code{TRUE}. Otherwise a factor with levels for every day of the year.
#'@keywords manip
#'@examples
#'
#'# The default numeric result
#'baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"))
#'# The result as a factor
#'baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"), numeric=FALSE)
#'

# Compute the julian day, aligning leap and non-leap year dates
#

#

baseDay <- function(x, numeric=TRUE) {
  ## Coding history:
  ##    2010Feb22 DLLorenz Initial coding
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2012Dec05 DLLorenz Added option to retrun factor instead of integer
  ##    2013Feb02 DLLorenz Prep for gitHub  
  ##
  x <- as.POSIXlt(x)
  if(numeric) {
    retval <- x$yday + 1L # Correct to counting days
    ## Adjust non leap year data to the base defeined by leap years
    retval <- ifelse(retval > 59L & !leap_year(x), retval + 1L, retval)
  }
  else {
    retval <- format(x, format="%b %d")
    levels <- format(seq(as.Date("2000-01-01"), by=1, length.out=366),
                     format="%b %d")
    retval <- factor(retval, levels=levels)
  }
  return(retval)
}
