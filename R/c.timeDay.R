#'Concatenate Data
#'
#'Combine time-of-day data into a single vector.
#'
#'
#' @param \dots any number of objects that can be converted to class "timeDay."
#'The first must be a time-of-day object.
#' @param recursive not used, required for other methods.
#' @return A single vector of class "timeDay."
#' @keywords manip
#' @examples
#'c(as.timeDay("10:00"), as.timeDay("3 PM", format="%I %p"))
#' @method c timeDay
#' @export
c.timeDay <- function (..., recursive=FALSE) {
  all.tod <- lapply(list(...), as.timeDay)
  times <- unlist(lapply(all.tod, function(x) x@time))
  fmts <- unlist(lapply(all.tod, function(x) x@format))
  fmtl <- nchar(fmts)
  ## Pick the longest format
  format <- fmts[which(max(fmtl) == fmtl)[1L]]
  return(new("timeDay", time=times, format=format))
}
