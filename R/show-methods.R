#'Methods for Function \code{show} for time-of-day objects
#'
#'Display the time of day.
#'
#'
#'@name show-methods
#'@aliases show-methods show,timeDay-method
#'@docType methods
#'@section Methods: \describe{
#'
#'\item{list("signature(object = \"timeDay\")")}{ Display to time of day in the
#'default format. } }
#'@keywords methods ~~ other possible keyword(s) ~~
#'@method show
setMethod("show",  "timeDay", function(object)
          print(object)
          )
