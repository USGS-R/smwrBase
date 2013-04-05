#'Screen Data for Completeness
#'
#'Screens data for the completeness of record by calendar or water year.
#'
#'
#'@usage screenData(dates, values, type = "DV", year = "calendar", printit =
#'TRUE)
#'@param dates the sequence of dates for each value in \code{values}.
#'@param values the sequence of observations.
#'@param type the frequency of \code{values}. Only daily values ("DV") are
#'accepted in this version.
#'@param year the type of year "calendar" or "water," which begins on October 1
#'of the previous calendar year and ends on September 30.
#'@param printit print the results in a pretty format?
#'@return For \code{type} = "DV," a matrix of the counts of missing values,
#'either coded as \code{NA} or not in the data set, for each month and each
#'year within the range of \code{dates}.
#'@references Rutledge, A.T., 2007, Program user Guide for Recess:
#'\url{http://water.usgs.gov/ogw/recess/UserManualRECESS.pdf}.
#'@keywords missing
#'@export
#'@examples
#'library(USGSwsData)
#'data(Q05078770)
#'# this should indicate no missing values.
#'with(Q05078770, screenData(DATES, FLOW))
#'# There should be missing values shown for the two water years.
#'with(Q05078770, screenData(DATES, FLOW, year="w"))
screenData <- function(dates, values, type="DV", year="calendar", printit=TRUE) {
  ## Coding history:
  ##    2005Sep29 DLLorenz Initial coding.
  ##    2006May18 DLLorenz Entered into GW package
  ##    2006Aug02 DLLorenz Modified arguments
  ##    2006Aug28 DLLorenz Standardized arguments
  ##    2012May31 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb11 DLLorenz Prep for gitHub
  ##
  if(length(dates) != length(values))
    stop("The lengths of Dates and Values must be the same.")
  if(type != "DV")
    stop('Only type = "DV" is currently supported.')
  ## Set year
  year <- match.arg(year, c("calendar", "water"))
  ## Remove NAs in arguments
  dates <- dates[!is.na(values)]
  if(year == "calendar") {
    Year <- as.factor(year(dates))
    Month.number <- factor(month(dates), levels=seq(12))
    strt <- "-01-01"
    end <- "-12-31"
  }
  else {
    Year <- waterYear(dates)
    Month.number <- factor(month(dates), levels=c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9))
    strt <- "-10-01"
    end <- "-09-30"
  }
  actual <- table(Year, Month.number)
  Byrange <- range(dimnames(actual)[[1L]])
  if(year == "water") # need to subtract one year
    Byrange[1L] <- as.character(as.integer(Byrange[1]) - 1L)
  Start <- as.Date(paste(Byrange[1L], strt, sep=""))
  End <- as.Date(paste(Byrange[2L], end, sep=""))
  potential <- seq(from=Start, to=End, by="days")
  if(year == "calendar") {
    Year <- year(potential)
    Month.number <- month(potential)
  }
  else {
    Year <- waterYear(potential)
    Month.number <- factor(month(potential),
                    levels=c(10, 11, 12, 1, 2, 3, 4, 5, 6, 7, 8, 9))
  }
  potential <- table(Year, Month.number)
  if(dim(potential)[1L] != dim(actual)[1L]) { # missing a complete By
    temp <- potential * 0L
    temp[dimnames(actual)[[1L]], dimnames(actual)[[2L]]] <- actual
    actual <- temp
  }
  retval <- potential - actual
  if(printit) {
    if(all(retval == 0L))
      cat("\nNo missing data between", as.character(Start),
          "and", as.character(End), "\n\n")
    else {
      cat("\nTable of incomplete ", type, "s:\n", sep='')
      prret <- gsub("^0$", " ", as.character(retval))
      prret <- matrix(prret, ncol=12, dimnames=dimnames(retval))
      print(prret, quote=FALSE)
      cat("Date of first value: ", as.character(min(dates)),
          "\nDate of last value: ", as.character(max(dates)), "\n", sep='')
      if(any(is.na((values))))
        cat("\n", sum(is.na(values)), "missing values in the dataset.\n", sep=' ')
    }
  }
  invisible(retval)
}

## Supplemental Notes
## For UVs, assume that any day that has at least one UV is OK.
## For IVs (intermittent values, like those stored in QWDATA or GWSI)
## and UVs, it might be better to show a table of values present--by
## month for IVs and by day for UVs.
