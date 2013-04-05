#'Special Data
#'
#'Function to supply data for functions within USGSwsBase
#'
#'
#'@aliases conc.meq
#'@usage conc.meq()
#'@return A list containing necessary information for the function
#'\code{con2meq}.
#'@note The user may choose to make local copies of data with the same name to
#'change or add to each list.
#'@seealso \code{\link{conc2meq}}
#'@keywords list
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
