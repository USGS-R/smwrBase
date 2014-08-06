#'Rename Columns
#'
#'Renames columns in a dataset of daily or unit values retrieved from NWIS web. Daily and unit value columns
#'have names derived from their data descriptor, parameter, and statistic codes. This
#'function reads information from the header and the arguments in the call to 
#'\code{renCol} to rename those columns.
#'
#' @param data the daily- or unit-values datset retrieved from NWISweb.
#' @param p00010 the base name for parameter code 00010.
#' @param p00045 the base name for parameter code 00045.
#' @param p00060 the base name for parameter code 00060.
#' @param p00065 the base name for parameter code 00065.
#' @param p00095 the base name for parameter code 00095.
#' @param p00300 the base name for parameter code 00300.
#' @param p00400 the base name for parameter code 00400.
#' @param p62611 the base name for parameter code 62611.
#' @param p63680 the base name for parameter code 63680.
#' @param p72019 the base name for parameter code 72019.
#' @param \dots named arguments for the base name for any other parameter code. The
#'form of the name must be like pXXXXX, where XXXXX is the parameter code.
#' @return A dataset like \code{data} with selected columns renamed.
#' @note The following statistics codes are converted by renCol. See 
#'url{http://help.waterdata.usgs.gov/stat_cd_nm} for information about USGS statistics codes.
#'\describe{
#'\item{00001}{Maximum value, suffix: Max}
#'\item{00002}{Minimum value, suffix: Min}
#'\item{00003}{Mean value, no suffix}
#'\item{00006}{Sum of values, suffix: Sum}
#'\item{00007}{Modal value, suffix: Mode}
#'\item{00008}{Median value, suffix: Median}
#'\item{00011}{Instantaneous Value, suffix: Inst}
#'\item{00012}{Equivalent mean value, suffix: EqMean}
#'\item{00021}{Tidal high-high value, suffix: HiHiTide}
#'\item{00022}{Tidal low-high value, suffix: LoHiTide}
#'\item{00023}{Tidal high-low value, suffix: HiLoTide}
#'\item{00024}{Tidal low-low value, suffix: LoLoTide}
#'}
#' @seealso \code{\link{readNWIS}}
#' @keywords manip IO
#' @export
renCol <- function(data, p00010="Wtemp", p00045="Precip",
                   p00060="Flow", p00065="GH", p00095="SpecCond", p00300="DO",
                   p00400="pH", p62611="GWL", p63680="Turb", p72019="WLBLS",
                   ...) {
  ## Coding history:
  ##    2012Dec21 DLLorenz Original Coding, world did not end!
  ##    2013Feb11 DLLorenz Prep for gitHub
  ##
  warning("renCol is deprecated in USGSwsBase and will be moved, and possibly renamed, to USGSwsDataRetrieval.")
  ## Create list of conversions
  Conv <- list(...)
  Conv$p00010 <- p00010
  Conv$p00060 <- p00060
  Conv$p00045 <- p00045
  Conv$p00065 <- p00065
  Conv$p00095 <- p00095
  Conv$p00300 <- p00300
  Conv$p00400 <- p00400
  Conv$p62611 <- p62611
  Conv$p63680 <- p63680
  Conv$p72019 <- p72019
  Pconv <- substring(names(Conv), 2) # The codes
  Nconv <- make.names(unlist(Conv)) # The names
  ## Extract the header info for dd, pcodes, etc.
  header <- comment(data)
  Dstart <- grep("DD param", header)
  if(length(Dstart) == 0L)
    stop("invalid data")
  ## Subtract 2 becuase Dstart = line 1 and Count is extra line
  Dend <- grep("^#$", header[Dstart:length(header)])[1L] + Dstart - 2L
  header <- gsub("   *", ";", header[Dstart:Dend])
  header[1] <- gsub(" ", ";", header[1])
  header <- read.csv2(text=header, header=TRUE, comment.char="!",
                      colClasses="character")[, -1L] # First col is #
  ## Get the default col names--Xdd_pcode_stat (_stat dv only)
  Cnames <- names(data)
  for(i in seq(along=Pconv)) {
    ## Any to change?
    picks <- grep(Pconv[i], substring(Cnames, 5L, 9L))
    DDs <- unique(substring(Cnames[picks], 2L, 3L))
    if(length(DDs) == 1L) # value and code, easy!
      Cnames[picks] <- sub(paste("X.._", Pconv[i], sep=""), Nconv[i],
                           Cnames[picks])
    else { # need DD info
      Cnames[picks] <- sub(Pconv[i], Nconv[i],
                           Cnames[picks])
      DDnm <- sapply(DDs, function(d, h) {
        x <- h$Description[h$DD == d][1L]
        x <- strsplit(x, split=", ")[[1]]
        return(make.names(x[length(x)]))
      }, h=header)
      for(j in seq(along=DDs))
        Cnames[picks] <- sub(paste("X", DDs[j], sep=""), DDnm[j],
                             Cnames[picks])
    }
    ## Otherwise, nothing to do!
  }
  ## Convert the trailing parts of dv data
  Cnames <- sub("_00001", "_Max", Cnames)
  Cnames <- sub("_00002", "_Min", Cnames)
  Cnames <- sub("_00003", "", Cnames) # Leave mean blank
  Cnames <- sub("_00006", "_Sum", Cnames)
  Cnames <- sub("_00007", "_Mode", Cnames)
  Cnames <- sub("_00008", "_Median", Cnames)
  Cnames <- sub("_00011", "_Inst", Cnames) # Why is this in dv?
  Cnames <- sub("_00012", "_EqMean", Cnames)
  Cnames <- sub("_00021", "_HiHiTide", Cnames)
  Cnames <- sub("_00022", "_LoHiTide", Cnames)
  Cnames <- sub("_00023", "_HiLoTide", Cnames)
  Cnames <- sub("_00024", "_LoLoTide", Cnames)
  names(data) <- Cnames
  return(data)
}
