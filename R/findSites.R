#' Site Inventory
#'
#' Get USGS streamgages, observation wells or other data collection sites
#'given some search criteria.
#'
#' There are several arguments to the \code{findSites} function. Only one of
#'\code{state}, \code{county}, \code{huc} or \code{box} can be used to 
#'define the major filter. 
#'In general, only a few need
#'to be used to limit the search to a reasonable group.
#' 
#' @param type the type of site desired. Must by one of "SW" or "ST" for 
#'stream, "GW" for groundwater well, "WE" for wetland,
#'"SP" for spring, "LK" for lake, "ES" for esturary, or "OC" for ocean.
#' @param state the 2-character postal abbreviation for a state
#' @param county a character vector of county 5-digit FIPS ids. No
#'more than 20 can be selected. See \bold{References} to find county codes.
#' @param huc either a single 2-, 4-, or 6-digit hydrologic region, subregion,
#'or accounting unit, or from 1 to a maximum of 10 8-digit hydrologic
#'cataloging units.
#' @param box a list containing the components north, south, east, and west
#'that indicate the latitude and longitude extremes of the bounding box 
#'for the retrieval. The components may be abbreviated n, s, e, and w.
#'The east and west values are assumed to be in terms of western 
#'longitude, so may be either positive or negative values.
#' @param name the first few words of the name of the site. Most often used to search for 
#'streamgages along a specific river. See \bold{Examples}.
#' @param data.type the type of data for which to search. Must be one of
#'"any" for any type of data, "iv" for instantaneous values, "dv" for daily
#'values, "gw" for ground-water levels, or "qw" for water-quality data.
#' @return A data frame containing the columns.
#'#'\tabular{ll}{ agency_cd \tab The agency code\cr 
#'site_no \tab The site identifier\cr 
#'station_nm \tab The site name\cr 
#'site_tp_cd \tab The site type code\cr 
#'dec_lat_va \tab The decimal latitute\cr 
#'dec_long_va \tab The decimal longitude\cr
#'coord_acy_cd \tab Latitude-longitude accuracy\cr
#'dec_coord_datum_cd \tab Decimal Latitude-longitude datum\cr
#'alt_va \tab Altitude of Gage/land surface\cr
#'alt_acy_va \tab Altitude accuracy\cr
#'alt_datum_cd \tab Altitude datum\cr
#'huc_cd \tab Hydrologic unit code\cr }
#' @seealso \code{\link{readNWIS}}
#' @references Information about current water conditions in the United States
#'and historical hydrologic data can be obtained from 
#'\url{http://waterdata.usgs.gov}.\cr
#'County FIPS codes can be obtained from 
#'\url{http://www.epa.gov/enviro/html/codes/state.html}.\cr
#'
#' @keywords DataIO
#' @examples
#'\dontrun{
#'# Find Streamgages on the Red River of the North
#'findSites(huc="0902", name="Red River", data.type="dv")
#'# Find wells in Greeley county Nebraska that have water-quality samples
#'findSites(type="GW", county="31077", data.type="qw")
#'}
#' @export
#' @importFrom dataRetrieval importRDB1
findSites <- function(type=c("SW", "ST", "GW", "WE", "SP", "LK", "ES", "OC"),
                      state="", county="", huc="", box=list(),
                      name="", data.type=c("any", "iv", "dv", "gw", "qw")) {

  
  
  #   ## Coding history:
#   ##    2014Apr24 DLLorenz original Coding
#   ##    2014May02 DLLorenz finish initial working version
  warning("findSites is deprecated in USGSwsBase and will be moved, and possibly renamed, to dataRetrieval.")
  type <- match.arg(type)
  if(type == "SW")
    type <- "ST" # fix for those who rely on SW rather than the code used
  ## What was specified on the call? A list of argument names with the values
  todo <- names(as.list(match.call())[-1L]) # Drop function name
  CK <- todo %in% c("state", "county", "huc", "box")
  if(length(CK) == 0L || !any(CK))
    stop("At least one of state, county, huc, or box must be specified")
  if(sum(CK) > 2L)
    stop("No more than one of state, county, huc, or box can be specified")
  ## Process major request
  hucfilter <- ""
  if("state" %in% todo) {
    myurl <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&stateCd=",
                   state, sep="")
  } else if("county" %in% todo) {
    county <- paste(county, collapse=",")
    myurl <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&countyCd=",
                   county, sep="")
  } else if("huc" %in% todo) {
    hucnchar <- nchar(huc)
    if(length(hucnchar) > 1L && min(hucnchar) < 8L)
      stop("Only a single 2-, 4-, or 6- digit huc can be requested.")
    if(length(hucnchar) > 1L) {
      huc <- paste(huc, collapse=",")
    } else if(hucnchar < 8L && hucnchar > 2L) {
      hucfilter <- huc
      huc <- substring(huc, 1L, 2L)
    }
    myurl <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&huc=",
                   huc, sep="")
  } else { # Must be bounding box
    if(is.null(north <- box$n))
      stop("north latitude not found")
    if(is.null(south <- box$s))
      stop("south latitude not found")
    if(is.null(east <- box$e))
      stop("east longitude not found")
    if(is.null(west <- box$w))
      stop("west longitude not found")
    ## fix oopses
    if(west > 0)
      west <- -west
    if(east > 0)
      east <- -east
    sn <- range(north, south)
    we <- range(east, west)
    llbox <- paste(we[1L], sn[1L], we[2L], sn[2L], sep=",")
    myurl <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&bBox=",
                   llbox, sep="")
  }
  ## add minor limits
  myurl <- paste(myurl, "&siteType=", type, sep="")
  data.type <- match.arg(data.type)
  if(data.type != "any")
    myurl <- paste(myurl, "&hasDataTypeCd=", data.type, sep="")
  retval <- dataRetrieval::importRDB1(myurl)
#   # Keep only the good stuff
  if(name != "") {
    name <- paste("^", name, sep="")
    picks <- grep(name, retval$station_nm, ignore.case=TRUE)
    if(length(picks) == 0L) {
      warning("No data retrieved, check arguments for validity")
      return(data.frame())
    }
    retval <- retval[picks,]
  }
  if(hucfilter != ""){
    picks <- grep(hucfilter, retval$huc_cd)
    if(length(picks) == 0L) {
      warning("No data retrieved, check arguments for validity")
      return(data.frame())
    }
    retval <- retval[picks,]
  }
  return(retval)
}
