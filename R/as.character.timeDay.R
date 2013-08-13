#'Character Vector
#'
#'Convert the time-of-day object to a character string.
#'
#'
#' @param x the object ot be converted.
#' @param \dots further arguments passed to or from other methods.
#' @return The values in \code{x} converted to a character representation.
#' @keywords character
#' @method as.character timeDay
#' @S3method as.character timeDay
as.character.timeDay <- function(x, ...)
  format(x) # Easy!
