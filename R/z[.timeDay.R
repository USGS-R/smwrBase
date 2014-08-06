#'Extract Parts of an Object
#'
#'Extract elements of a time-of-day object.
#'
#' @rdname zsubset.timeDay
#' @param x the object.
#' @param i an index specifying elements to extract. See \code{\link{Extract}}
#'for details.
#' @return The subset of \code{x} indicated by \code{i}.
#' @seealso \code{\link{Extract}}
#' @keywords manip
#' @method [ timeDay
#' @export
"[.timeDay" <- function(x, i) {
  x@time <- x@time[i]
  return(x)
}
