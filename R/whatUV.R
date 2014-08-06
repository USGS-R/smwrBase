#'Data Inventory
#'
#'Gets a description of unit-value data available for a USGS station and the
#'beginning and ending dates of each parameter.
#'
#' @param gage a single USGS station identifier as a character string.
#' @return A data frame containing the columns site_no, the USGS station identifier;
#'parm_cd, the parameter code; description, a desciption of the parameter code;
#'begin_date, the earliest date available for the parameter code data;
#'end_date, the latest date available for the parameter code data; and
#'count_nu, the number of values available for the parameter code.
#',EndDate, pCode, and name.
#' @seealso \code{\link{readNWIS}}
#' @references Information about current water conditions in the United States
#'and information about unit values can be obtained from 
#'\url{http://waterdata.usgs.gov/usa/nwis/rt}.
#' @keywords DataIO
#' @examples
#'\dontrun{
#'UVavailable <- whatUV("04027000")
#'print(UVavailable)
#'}
#' @export
whatUV <- function(gage) {
  ## Coding history:
  ##    2012Dec20 DLLorenz original Coding
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##    2014Apr15 DLLorenz Total rewrite to use waterservices
  ##
  if(missing(gage))
    stop("gage is required")
  myurl <- paste("http://waterservices.usgs.gov/nwis/site/?format=rdb&sites=",
                 gage, "&outputDataTypeCd=iv", sep="")
  retval <- importRDB(myurl)
  # Keep only the good stuff
  retval <- retval[, c("site_no", "parm_cd", "loc_web_ds", "begin_date", 
                       "end_date", "count_nu")]
  names(retval)[3L] <- "description" # make it nicer
  # Fill in some missing descriptions
  Descs <- c("00010" = "Water temperature", "00060" = "Streamflow",
             "00065" = "Gage height", "00095" = "Specific conductance",
             "00300" = "Dissolved oxygen", "00400" = "pH",
             "63680" = "Turbidity", "95202" = "Cyanobacteria",
             "99133" = "Nitrate plus Nitrite", "32295" = "CDOM",
             "32318" =  "Chlorophyll")
  tofill <- retval$description == ""
  if(any(tofill))
    retval$description[tofill] <- Descs[retval$parm_cd[tofill]]
  return(retval)
}
