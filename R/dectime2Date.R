#'Date Conversion
#'
#'Convert time data expressed as year and fractional part of year to class "Date."
#'
#'
#'@usage dectime2Date(x)
#'@param x the decimal date to convert.
#'@return A vector of class "Date" cooresponding to each value in \code{x}.
#'@note A small value, representing about 1 minute, is added to each value in \code{x}
#'to prevent truncation errors in the conversion. This can cause some errors if
#'the data were converted from date and time data.
#'@seealso \code{\link{dectime}}, \code{\link{as.Date}}
#'@keywords manip
#'@examples
#'
#'dectime("02/07/2013", date.format="%m/%d/%Y")
#'# Convert back the printed result:
#'dectime2Date(2013.101)
#'# Convert a more precise value:
#'dectime2Date(2013.1013699)
#'

dectime2Date <- function(x) {
  ## Add about 1 minute to the data to prevent truncation errors
  x <- x + 2.e-6
  xtrun <- trunc(x)
  xfrac <- x - xtrun
  as.Date(xfrac * (365 + leap_year(xtrun)),
          origin=ISOdate(xtrun,1,1))
}
