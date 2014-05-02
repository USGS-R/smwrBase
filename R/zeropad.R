#'Pad a Number
#'
#'Formats a character representation of a number and pads it with leading zeros
#'to a specified number of characters.
#'
#' @param x a vector of numbers, to be formatted. Missing value are permitted and result
#'in corresponding missing values in the output.
#' @param LEN the minimum number of characters in the output. If LEN is less
#'than or equal to 1, then no padding is done. The default is 0.
#' @return An optionally padded character vector representing the data in x.
#' @note Although this function is specifically designed for formatting integer
#'numbers, \code{x} can be a vector of any type.\cr
#'
#'By default, no padding is done; only a character representation of the input
#'is returned.
#' @seealso \code{\link{format}},
#' @keywords manip
#' @export
#' @examples
#'
#'zeropad(10^(-3:6))
#'# [1] "0.001"   "0.01"    "0.1"     "1"       "10"      "100"    
#'# [7] "1000"    "10000"   "100000"  "1000000"
#'zeropad(10^(-3:6),5)
#'# [1] "0.001"   "00.01"   "000.1"   "00001"   "00010"   "00100"  
#'# [7] "01000"   "10000"   "100000"  "1000000"
#'zeropad(c(""," ","a","bb","ccc","dddd","eeeee","ffffff"),5)
#'# [1] "00000"  "0000 "  "0000a"  "000bb"  "00ccc"  "0dddd"  "eeeee" 
#'# [8] "ffffff"
zeropad <- function(x, LEN=0) {
  ## Coding history:
  ##    2002Dec04 DLLorenz Initial coding.
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  x <- as.character(x)
  if(LEN <= 1) return(x)
  pattern <- paste(rep(0,LEN), collapse="")
  sapply(as.list(x), FUN=function(x, p, LEN) {
    nx <- nchar(x) - 1
    if(nx >= LEN) # Protect against x longer than LEN
      return(x)
    substring(p, LEN - nx, LEN) <- x
    return(p)}, p = pattern, LEN=LEN)
}
