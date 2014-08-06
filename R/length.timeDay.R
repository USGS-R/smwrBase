#'Length of an Object
#'
#'Get the length of a time-of-day object.
#'
#'
#' @param x a time-of-day object.
#' @return An integer of length 1 indicating the number of elements in \code{x}.
#' @keywords attribute
#' @examples
#'
#'length(as.timeDay(c("10:30", "11:00")))
#' @method length timeDay
#' @export
length.timeDay <- function(x)
  length(x@time)
