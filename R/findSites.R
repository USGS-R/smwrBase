#' Site Inventory
#'
#' Get USGS streamgages, observation wells or other data collection sites
#'given some search criteria.
#'
#' There are many arguments to the \code{findSites} function. In general, only a few need
#'to be used to limit the search to a reasonable group.
#' 
#' @param type the type of site desired. Must by one of ...
#' @param name the first few words of the name of the site. Most often used to search for 
#'streamgages along a specific river. See \bold{Examples}.
#' @param state the state name. May also be the postal abbreviation.
#' @param county the county name.
#' @return A data frame containing the columns .
#' @seealso \code{\link{readNWIS}}
#' @references Information about current water conditions in the United States
#'and historical hydrologic data can be obtained from 
#'\code{\link{http://waterdata.usgs.gov}}.
#' @keywords DataIO
#' @examples
#'\dontrun{
#'# Get the Streamgages on the Red River of the North
#'findSites(type="SW")
#'}
#' @export
findSites <- function(type=c("SW", "GW"), name="", state="", county="") {
  ## Coding history:
  ##    2014Apr24 DLLorenz original Coding
  ##
  type <- match.arg(type)
  ## What was specified on the call? A list of argument names with the values
  todo <- as.list(match.call())[-1L] # Drop function name
  return(todo)
  myurl <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&sites=",
                 gage, "&outputDataTypeCd=iv", sep="")
  retval <- importRDB(myurl)
  # Keep only the good stuff
  return(retval)
}
