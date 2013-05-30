#'Missing Values
#'
#'Indicate which elemnts are missing.
#'
#'
#'@param x the object to be tested.
#'@return A logical vector of the same length as its argument \code{x},
#'containing TRUE for those elements marked NA and FALSE otherwise.
#'@keywords manip
#'@examples
#'is.na(as.timeDay(c("10:30", "11:00")))
#'@method is.na timeDay
#'@S3method is.na timeDay
is.na.timeDay <- function(x)
  return(is.na(x@time))
