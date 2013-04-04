#'Encode in a Common Format
#'
#'Format an object of class "timeDay."
#'
#'
#'@usage \method{format}{timeDay}(x, format, ...)
#'@param x the object to be formatted to type "character."
#'@param format the format to use for output. See \code{\link{strptime}} for
#'supported format information.
#'@param \dots further arguments to be passed from or to other methods.
#'@return A cahracter string representing the time of day.
#'@seealso \code{\link{strptime}}
#'@keywords manip
format.timeDay <- function(x, format, ...) {
  if(missing(format))
    format <- x@format
  ## Easy way out--just use the functionality of POSIXt, origin is irrelevant
  return(format(as.POSIXct(x@time, origin="2000-01-01"), format=format))
}
