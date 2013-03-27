#'Length of an Object
#'
#'Get the length of time-of-day objects.
#'
#'
#'@usage \method{length}{timeDay}(x)
#'@param x a time-of-day object.
#'@return An integer of length 1 indicating the number of elements in \code{x}.
#'@keywords attribute
#'@examples
#'
#'length(as.timeDay(c("10:30", "11:00")))
#'

length.timeDay <- function(x)
  length(x@time)
