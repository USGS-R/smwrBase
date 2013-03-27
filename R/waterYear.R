#'Water Year
#'
#'Create an ordered factor or numeric values from a vector of dates based on
#'the water year.
#'
#'
#'@usage waterYear(x, numeric = FALSE)
#'@param x an object of class "Date" or "POSIXt." Missing values are permittied and
#'reesult in corresponding missing values in the output.
#'@param numeric a logical value that indicates whether the returned values
#'should be numeric \code{TRUE} or an ordered factor \code{FALSE}. The default
#'value is \code{FALSE}.
#'@return An ordered factor or numeric vector corresponding to the water year.
#'@note The water year is defined as the period from October 1 to September 30.
#'The water year is designated by the calendar year in which it ends. Thus, the
#'year ending September 30, 1999, is the "1999 water year."
#'@seealso \code{\link{year}}
#'@keywords manip
#'@examples
#'
#'library(USGSwsData)
#'data(QW05078470)
#'## Return an ordered factor
#'waterYear(QW05078470$DATES)
#'

waterYear <- function(x, numeric=FALSE) {
  ## Coding history:
  ##    2005Jul14 DLLorenz Initial dated verion
  ##    2010Feb17 DLLorenz Added option to return numerics
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  x <- as.POSIXlt(x)
  yr <- x$year + 1900L
  mn <- x$mon + 1L
  ## adjust for water year
  yr <- yr + ifelse(mn < 10L, 0L, 1L)
  if(numeric)
    return(yr)
  ordered(yr)
}
