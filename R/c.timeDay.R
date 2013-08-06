#'Concatenate Data
#'
#'Combine data into a single vector.
#'
#'
#'@param \dots any number of objects that can be converted to class "timeDay."
#'@param recursive required for other methods.
#'@return A single vector of class "timeDay."
#'@keywords manip
#'@method c timeDay
#'@S3method c timeDay
c.timeDay <- function (..., recursive=FALSE) {
  all.tod <- lapply(list(...), as.timeDay)
  times <- unlist(lapply(all.tod, function(x) x@time))
  fmts <- unlist(lapply(all.tod, function(x) x@format))
  fmtl <- nchar(fmts)
  ## Pick the longest format
  format <- fmts[which(max(fmtl) == fmtl)[1L]]
  return(new("timeDay", time=times, format=format))
}
