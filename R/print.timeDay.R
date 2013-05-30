#'Print an Object
#'
#'Print objects defined in the USGSbase package.
#'
#'
#'@param x an object of class "timeDay."
#'@param \dots  not used, required for other methods.
#'@return The object \code{x} is returned invisibly
#'@section Side Effect: The object \code{x} is printed
#'@seealso \code{\link{timeDay-class}}
#'@keywords print
#'@method print timeDay
#'@S3method print timeDay
print.timeDay <- function(x, ...)
  print(format(x), quote=FALSE)
