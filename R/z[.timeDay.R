#'Extract Parts of an Object
#'
#'Extract elements of a time-of-day object.
#'
#' @rdname zsubset.timeDay
#' @param x the object.
#' @param i an index specifying elements to extract. See \code{\link{Extract}}
#'for details.
#' @return The subset of \code{x} indicated by \code{i}.
#' @seealso 
#Flip for production/manual
#'\code{\link[base]{Extract}}
#\code{Extract} (in base package)
#' @keywords manip
#' @method [ timeDay
#' @export
"[.timeDay" <- function(x, i) {
  x@time <- x@time[i]
  return(x)
}
