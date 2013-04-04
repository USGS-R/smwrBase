#'Character Vector
#'
#'Convert to object of type "character."
#'
#'
#'@usage \method{as.character}{timeDay}(x, ...)
#'@param x the object ot be converted.
#'@param \dots further arguments passed to or from other methods.
#'@return The values in \code{x} converted to a character representation.
#'@keywords character
#' @export
as.character.timeDay <- function(x, ...)
  format(x) # Easy!
