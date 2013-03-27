#'Print an Object
#'
#'Print objects defined in the USGSbase package.
#'
#'
#'@aliases print.timeDay
#'@usage \method{print}{timeDay}(x, ...)
#'@param x an object of class "timeDay."
#'@param ...  not used, required for other methods.
#'@return The object \code{x} is returned invisibly
#'@section Side Effect: The object \code{x} is printed
#'@seealso \code{\link{timeDay-class}}
#'@keywords print

print.timeDay <- function(x, ...)
  print(format(x), quote=FALSE)
