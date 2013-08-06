#'Length of an Object
#'
#'Get the length of time-of-day objects.
#'
#'
#'@param x a time-of-day object.
#'@return An integer of length 1 indicating the number of elements in \code{x}.
#'@keywords attribute
#'@examples
#'
#'length(as.timeDay(c("10:30", "11:00")))
#'@method length timeDay
#'@S3method length timeDay
length.timeDay <- function(x)
  length(x@time)
