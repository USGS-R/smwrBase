#'Missing Values
#'
#'Indicate which elemnts are missing.
#'
#'
#'@usage \method{is.na}{timeDay}(x)
#'@param x the object to be tested.
#'@return A logical vector of the same length as its argument \code{x},
#'containing TRUE for those elements marked NA and FALSE otherwise.
#'@keywords manip
#'@export
#'@examples
#'is.na.timeDay(as.timeDay(c("10:30", "11:00")))
is.na.timeDay <- function(x)
  return(is.na(x@time))
