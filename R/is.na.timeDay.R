#'Missing Values
#'
#'Indicate which elemnts are missing.
#'
#'
#' @param x the object to be tested.
#' @return A logical vector of the same length as its argument \code{x},
#'containing \code{TRUE} for those elements marked \code{NA} and code{FALSE} otherwise.
#' @keywords manip
#' @examples
#'is.na(as.timeDay(c("10:30", "11:00")))
#' @method is.na timeDay
#' @export
is.na.timeDay <- function(x)
  return(is.na(x@time))
