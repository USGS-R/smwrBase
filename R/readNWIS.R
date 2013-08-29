#'Import Data
#'
#'Reads surface-water or groundwater data from NWISweb.
#'
#'The value for \code{param} must match a valid 5-digit USGS parameter code.
#'Required only for \code{dtype} "swdv," "gwdv," and "uv." The default value
#'for \code{dtype} = "swdv" is "00060" (daily streamflow) and for \code{dtype}
#'= "gwdv" is "72019" (water level below land surface). If blank and
#'\code{gage} has fewer than 15 characters, then \code{param} is set to
#'"00060," otherwise "72019."\cr
#'In some cases, daily values data can be tagged with additional nonnumeric
#'flags. In those cases, the data would be converted to \code{NA}, but setting
#'\code{convert.type} to \code{FALSE} will preserve all data as character and
#'the all data can be converted manually by the user.
#'
#' @param gage a single USGS station identifier as a character string.
#' @param dtype the type of data, must be "swdv" for surface-water daily values,
#'"peak" for annual peaks, "measurements" for discharge measurements, "gwdv"
#'for groundwater daily values,  "gwlevels" for groundwater level
#'measurements, "uv" for unit values, "gage" for streamgage information, or
#'"well" for well information. Only the first letter is required,
#'except to distinguish between "gwdv," "gwlevels," and "gage."
#' @param begin.date the data to use for the earliest value. Not used for
#'\code{dtype} = "peak." If, "", then retrieve beginning with the first record
#'in the database for "swdv" and "gwdv". If, "" for "uv", one week of data is returned.
#' @param end.date the data to use for the latest value. Not used for
#'\code{dtype} = "peak." If "", then retrieve the most recent values in the
#'database.
#' @param param the parameter code to retrieve. See \bold{Details}.
#' @param convert.type Convert data to types indicated by the column type in the
#'data or as indicated in \bold{Note}? See \bold{Details}.
#' @return A data frame of the appropriate data. See
#'\url{http://waterdata.usgs.gov/usa/nwis/sw} for details about surface water
#'or \url{http://waterdata.usgs.gov/usa/nwis/gw} for details about groundwater.
#' @note Column names ending in "_va" are always forced to be numeric even if
#'the column header information indicates otherwise. Other columns may need to
#'be converted depending on user needs.\cr Peak flow data do not always have
#'complete date information due to uncertainity in the exact day of the peak.
#'The column \code{peak_dt} is always type "character" but can be converted by
#'the user.
#' @author Original coding by Tim Cohn \email{tacohn@@usgs.gov} additional
#'capabilities added by Dave Lorenz \email{lorenz@@usgs.gov}.
#' @seealso \code{\link{importRDB}}
#' @keywords manip IO
#' @export
#' @examples
#'\dontrun{
#'# Get the first 5 days in 2010 for USGS station identifier
#'# 01578310 SUSQUEHANNA RIVER AT CONOWINGO, MD
#'readNWIS("01578310", begin.date="2010-01-01", end.date="2010-01-05")
#'# Get the most recent groundwater levels, in feet below land surface,
#'# from a well south of Bend, Ore.
#'readNWIS("434400121275801", "gwl", begin.date="2010-01-01")
#'}
readNWIS <- function(gage, dtype="swdv", begin.date="", end.date="",
                     param=NULL, convert.type=TRUE) {
  ## Coding history:
  ##    2005Oct25 TimCohn  Original
  ##    2009Dec30 TimCohn  Revisions
  ##    2011Jul12 DLLorenz Changed output to data.frame
  ##    2012Feb07 DLLorenz Tweaks for USGS package
  ##                        basically set up to use importRDB
  ##    2012Feb08 DLLorenz Added begin and end dates for dv and measurements
  ##    2012Feb14 DLLorenz Added gw options
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2012Sep14 DLLorenz Force all columns ending in _va to numeric
  ##    2012Oct27 DLLorenz Add uv and suppress date conversion on peak
  ##    2012Nov08 DLLorenz Add "gage" and "well" dtypes
  ##    2012Dec03 DLLorenz Revision for date range selection for dv data
  ##                        and allow retrieval of all parameters for dv data
  ##    2012Dec04 DLLorenz Forced column ending in _nu to integer
  ##    2012Dec07 DLLorenz Bug fix for gage retrievals to get partial records
  ##    2012Dec20 DLLorenz Tweaks for unit values
  ##    2013Jan30 DLLorenz Added convert.type option to supress all type conversions
  ##    2013Jan30 DLLorenz Prep for gitHub
  ##
  ## Make sure dtype is valid
  dtype <- match.arg(dtype, c("swdv", "gwdv", "measurements", "peak",
                              "gwlevels", "uv", "gage", "well"))
  if(dtype == "swdv") {
    if(is.null(param))
      param <- "&index_pmcode_00060=1"
    else if(param == "all")
      param <- NULL # Go figure the logic on this one! (force action to get all)
    else
      param <- paste("&index_pmcode_", param, "=1", sep="")
    if(begin.date == "") # Fix needed to set earliest date
      begin.date <- "1860-01-01"
  }
  else if(dtype == "gwdv") {
    if(is.null(param))
      param <- "&index_pmcode_72019=1"
    else if(param == "all")
      param <- NULL
    else
      param <- paste("&index_pmcode_", param, "=1", sep="")
    if(begin.date == "") # Fix needed to set earliest date
      begin.date <- "1860-01-01"
  }
  else if(is.null(param) && dtype == "uv") {
      stop("the param argument is required for dtype \"uv\"")
  }
  typeadd <- switch(dtype,
                    swdv=paste(param, "&begin_date=", begin.date,
                      "&end_date=", end.date, "&referred_module=sw", sep=""),
                    gwdv=paste(param, "&begin_date=", begin.date,
                      "&end_date=", end.date, "&referred_module=gw", sep=""),
                    measurements=paste("&begin_date=", begin.date,"&end_date=",
                      end.date, sep=""),
                    peak=NULL,
                    gwlevels=paste("&begin_date=", begin.date,"&end_date=",
                      end.date, sep=""),
                    uv=paste("?format=rdb,1.0&sites=", gage,"&startDT=",begin.date, 
                      "&endDT=", end.date, "&parameterCd=", param, sep=""),
                    gage=paste("&search_site_no=", gage,
                      "&search_site_no_match_type=exact&sort_key=site_no&group_key=NONE&format=sitefile_output&sitefile_output_format=rdb&column_name=agency_cd&column_name=site_no&column_name=station_nm&column_name=site_tp_cd&column_name=lat_va&column_name=long_va&column_name=dec_lat_va&column_name=dec_long_va&column_name=coord_meth_cd&column_name=coord_acy_cd&column_name=coord_datum_cd&column_name=dec_coord_datum_cd&column_name=district_cd&column_name=state_cd&column_name=county_cd&column_name=country_cd&column_name=land_net_ds&column_name=map_nm&column_name=map_scale_fc&column_name=alt_va&column_name=alt_meth_cd&column_name=alt_acy_va&column_name=alt_datum_cd&column_name=huc_cd&column_name=basin_cd&column_name=topo_cd&column_name=data_types_cd&column_name=instruments_cd&column_name=construction_dt&column_name=inventory_dt&column_name=drain_area_va&column_name=contrib_drain_area_va&column_name=tz_cd&column_name=local_time_fg&column_name=reliability_cd&column_name=project_no&column_name=rt_bol&column_name=peak_begin_date&column_name=peak_end_date&column_name=peak_count_nu&column_name=qw_begin_date&column_name=qw_end_date&column_name=qw_count_nu&column_name=sv_begin_date&column_name=sv_end_date&column_name=sv_count_nu&range_selection=days&period=365&date_format=YYYY-MM-DD&rdb_compression=file&list_of_search_criteria=search_site_no",
                      sep=""),
                    well=paste("&search_site_no=", gage,
                      "&search_site_no_match_type=exact&sort_key=site_no&group_key=NONE&format=sitefile_output&sitefile_output_format=rdb&column_name=agency_cd&column_name=site_no&column_name=station_nm&column_name=site_tp_cd&column_name=lat_va&column_name=long_va&column_name=dec_lat_va&column_name=dec_long_va&column_name=coord_meth_cd&column_name=coord_acy_cd&column_name=coord_datum_cd&column_name=dec_coord_datum_cd&column_name=district_cd&column_name=state_cd&column_name=county_cd&column_name=country_cd&column_name=land_net_ds&column_name=map_nm&column_name=map_scale_fc&column_name=alt_va&column_name=alt_meth_cd&column_name=alt_acy_va&column_name=alt_datum_cd&column_name=huc_cd&column_name=basin_cd&column_name=topo_cd&column_name=data_types_cd&column_name=instruments_cd&column_name=construction_dt&column_name=inventory_dt&column_name=tz_cd&column_name=local_time_fg&column_name=reliability_cd&column_name=gw_file_cd&column_name=nat_aqfr_cd&column_name=aqfr_cd&column_name=aqfr_type_cd&column_name=well_depth_va&column_name=hole_depth_va&column_name=depth_src_cd&column_name=project_no&column_name=rt_bol&column_name=qw_begin_date&column_name=qw_end_date&column_name=qw_count_nu&column_name=gw_begin_date&column_name=gw_end_date&column_name=gw_count_nu&column_name=sv_begin_date&column_name=sv_end_date&column_name=sv_count_nu&range_selection=days&period=365&date_format=YYYY-MM-DD&rdb_compression=file&list_of_search_criteria=search_site_no",
                      sep=""))
  ## Monthly, annual and others may require different url formats
  if(dtype %in% c("swdv", "gwdv"))
    dtype <- substring(dtype, 3L, 4L)
  if(dtype == "uv")
    myurl <- url(paste("http://waterservices.usgs.gov/nwis/iv/", typeadd, sep=""))
  else if(dtype == "gage")
    myurl <- url(paste("http://waterdata.usgs.gov/nwis/measurements?",
                       typeadd, sep=""))
  else if(dtype == "well")
    myurl <- url(paste("http://waterdata.usgs.gov/nwis/dv?referred_module=gw",
                       typeadd, sep=""))
  else if(dtype == "peak")
    myurl <- url(paste("http://nwis.waterdata.usgs.gov/usa/nwis/",
                       dtype,
                       "/?site_no=",
                       gage,
                       "&range_selection=date_range&format=rdb",
                       typeadd,
                       sep=""))
  else
    myurl <- url(paste("http://waterdata.usgs.gov/nwis/",
                       dtype,
                       "?site_no=",
                       gage,
                       "&range_selection=date_range&format=rdb",
                       typeadd,
                       sep=""))
  ## Use the date.format option according to the dtype
  URL <- summary(myurl)$description # keep for debugging below
  if(dtype == "uv")
    retval <- importRDB(myurl, date.format="%Y-%m-%d %H:%M", convert.type=convert.type)
  else if(dtype == "peak")
    retval <- importRDB(myurl, date.format="none", convert.type=convert.type) # not all are valid
  else
    retval <- importRDB(myurl, convert.type=convert.type)
  close(myurl)
  if(convert.type) { #Do not force conversion if requested not to
    ## In some cases, columns ending in _va are not always numeric, but should be
    for(i in grep("_va$", names(retval), value=TRUE))
      retval[[i]] <- as.numeric(retval[[i]])
    ## In some cases, columns ending in _nu are not always numeric, but should be
    ##  forced to integer
    for(i in grep("_nu$", names(retval), value=TRUE))
      retval[[i]] <- as.integer(retval[[i]])
  }
  if(nrow(retval) == 0L)
    warning("No data retrieved, check arguments for validity")
  if(ncol(retval) == 1L) {
    ## No retrieval should get only a single column, must not be valid
    ## The one-column retrieval means that an HTML page was returned instead
    warning("Invalid data retrieved, check for validity of the request, URL:\n",
            URL)
    retval <- data.frame()
  }
  return(retval)
}
