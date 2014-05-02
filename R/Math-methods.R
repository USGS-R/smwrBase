#'Mathematical Functions for \code{timeDay} objects
#'
#'No mathematic functions, such as \code{log} or \code{exp} are allowed on time-of-day data.
#'An error results when trying to use any mathematic function on objects of class "timeDay."
#'
#' @name Math-methods
#' @aliases Math-methods Math,timeDay-method
#' @docType methods
#' @section Methods: \describe{
#'
#'\item{signature(x = "timeDay")}{ Any call to a mathematical
#'function will generate an error. } }
#' @keywords methods
#' @exportMethod Math
setMethod("Math", "timeDay", function(x)
          stop(gettextf("'%s' not defined for timeDay objects", .Generic),
               domain=NA)
          )
