#'Special Data
#'
#'Support function to supply data for functions within smwrBase.
#'
#'
#' @aliases conc.meq
#' @usage conc.meq()
#' @return A list containing necessary information for the function
#'\code{conc2meq}.
#' @note The user may choose to make local copies of the data with the 
#'same name (conc.meq) to be able to change or add to the list.
#' @seealso \code{\link{conc2meq}}
#' @keywords list
#' @export
conc.meq <- function(){
  ## Coding history:
  ##    2011Jul06 DLLorenz Original in prep for package
  ##    2012Aug23 DLLorenz Convert to function
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  list(constituent = c("aluminum", "ammonia as n", "ammonium as nh4",
         "bicarbonate", "bromide", "calcium", "carbonate", "chloride",
         "fluoride","iron", "magnesium", "manganese", "nitrate as n",
         "nitrite as n", "phosphorus as p", "potassium", "sodium",
         "sulfate", "sulfide"),
       conversion = c(.11119, .07139, .05544, .01639, .01252, .04990,
         .03333, .02821, .05264, .03581, .08229, .03640, .07139,
         .07139, .06457, .02558, .04350, .02082, .06238))
}
