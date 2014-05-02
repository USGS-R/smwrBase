#'Data Frames
#'
#'Convert an object to class "data.frame."
#'
#'
#' @param x the time-of-day object to be converted.
#' @param row.names \code{NULL} or a character vector giving the row names for
#'the data frame. Missing values are not allowed.
#' @param optional logical. If \code{TRUE}, setting row names and converting
#'column names to syntactic names is optional.
#' @param \dots not used, required for other methods.
#' @param nm the column name to create for \code{x}.
#' @return A data frame is created containing the data in \code{x}.
#' @seealso \code{\link{as.data.frame}}
#' @keywords manip
#' @method as.data.frame timeDay
#' @S3method as.data.frame timeDay
as.data.frame.timeDay <- function(x, row.names = NULL, optional = FALSE,
                                  ..., nm = deparse(substitute(x))) {
  force(nm)
  nrows <- length(x)
  if(is.null(row.names)) {
    if(nrows == 0L) 
      row.names <- character(0L)
    else
      row.names <- .set_row_names(nrows)
  }
  retval <- list(x)
  if(!optional) 
    names(retval) <- make.names(nm)
  attr(retval, "row.names") <- row.names
  class(retval) <- "data.frame"
  retval
}
