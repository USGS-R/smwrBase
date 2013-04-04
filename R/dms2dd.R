#'Decimal Degrees
#'
#'Convert data in degrees, minutes, and seconds format to decimal degrees.
#'
#'
#'@usage dms2dd(x, minutes = NULL, seconds = 0, split = "")
#'@param x a character or numeric vector coded as degrees, minutes, and seconds
#'or a numeric vector of degrees.  The character string may contain a leading
#'zero for values less than 100. Missing values are permitted and result in
#'missing values in the output.
#'@param minutes a vector of minutes. If supplied, then \code{x} is assumed to
#'be a numeric vector of degrees.  Missing values are permitted.
#'@param seconds a vector of seconds. Assumed to be 0 if not supplied. Missing
#'values are permitted.
#'@param split the delimiter for \code{x} if \code{x} is character.
#'@return A numeric vector of decimal degrees the same length as \code{x}.
#'Missing values are returned whereever either \code{x}, \code{minutes}, or
#'\code{seconds} is missing.
#'@keywords manip
#'@export
#'@examples
#'
#'dms2dd(983206) # using a numeric value
#'# should be [1] 98.535
#'dms2dd("0983206") # using a character value
#'# should be [1] 98.535
#'dms2dd(98, 32, 6) # using numeric values for degrees, minutes and seconds
#'# should be [1] 98.535
#'dms2dd("98:32", split=":") # Note missing seconds
#'# should be [1] 98.53333
dms2dd <- function(x, minutes=NULL, seconds=0, split="") {
  ## Coding history:
  ##    2006Dec12 DLLorenz Original coding
  ##    2007Feb12 DLLorenz Entered into USGS library and added minutes and seconds
  ##    2007Jul12 DLLorenz Modifications to allow character input
  ##    2011Apr26 DLLorenz Conversion to R
  ##    2011Jul06 DLLorenz Prep for package
  ##    2012Feb10 DLLorenz Added sep argument and logic
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  ## Split x if needed
  if(split != "") {
    x <- strsplit(x, split=split)
    seconds <- na2miss(as.double(sapply(x, function(s) s[3L])), 0)
    minutes <- as.double(sapply(x, function(s) s[2L]))
    x <- as.double(sapply(x, function(s) s[1L]))
  }
  x <- as.double(x)
  signx <- x > 0
  if(is.null(minutes)) {
    x <- abs(x)
    x <- x / 10000
    d <- floor(x) # degrees
    x <- (x - d) * 100
    m <- floor(x) # minutes
    x <- (x - m) * 100 # seconds
    retval <- d + m / 60. + x / 3600.
  }
  else {
    minutes <- as.double(minutes)
    seconds <- as.double(seconds)
    retval <- abs(x) + minutes / 60. + seconds / 3600.
  }
  ifelse(signx, retval, -retval)
  return(retval)
}
