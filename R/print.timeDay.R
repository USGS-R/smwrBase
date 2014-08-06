#'Print an Object
#'
#'Print an object of class "timeDay."
#'
#' @param x an object of class "timeDay."
#' @param \dots  not used, required for other methods.
#' @return The object \code{x} is returned invisibly.
#' @note The object is printed using the format that created the object
#'in \code{as.timeDay}.
#' @section Side Effect: The object \code{x} is printed.
#' @seealso \code{\link{timeDay-class}}, \code{\link{as.timeDay}}
#' @keywords print
#' @method print timeDay
#' @export
print.timeDay <- function(x, ...)
  print(format(x), quote=FALSE)
