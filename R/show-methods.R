#'Methods for Function \code{show} for time-of-day objects
#'
#'Display the time of day.
#'
#'
#'@name show-methods
#'@aliases show-methods show,timeDay-method
#'@docType methods
#'@param object the object to be printed.
#'@return The object is printed and returned invisibly.
#'@keywords methods
#'@exportMethod show
setMethod("show",  "timeDay", function(object)
          print(object)
          )
