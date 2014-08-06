#'Encode in a Common Format
#'
#'Format an object of class "timeDay."
#'
#'
#' @param x the object to be formatted to type "character."
#' @param format the format to use for output. See \code{\link{strptime}} for
#'supported format information.
#' @param \dots not used, required for other methods.
#' @return A vector of character strings representing the time of day values in \code{x}.
#' @seealso \code{\link{strptime}}
#' @keywords manip
#' @method format timeDay
#' @export
format.timeDay <- function(x, format, ...) {
  if(missing(format))
    format <- x@format
  ## Easy way out--just use the functionality of POSIXt, origin is irrelevant
  return(format(as.POSIXct(x@time, origin="2000-01-01", tz="GMT"), format=format))
}
