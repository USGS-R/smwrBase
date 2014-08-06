#'Character Vector
#'
#'Convert the time-of-day object to a character string.
#'
#'
#' @param x the time-of-day object to be converted.
#' @param \dots not used, required for other methods.
#' @return The values in \code{x} converted to a character representation.
#' @keywords character
#' @method as.character timeDay
#' @export
as.character.timeDay <- function(x, ...)
  format(x) # Easy!
