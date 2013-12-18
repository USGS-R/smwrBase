#'Import Files
#'
#'Imports a comma separated variable file to a data frame.
#'
#'All of the dates in a date column must have the same format as the first
#'non-blank date in the column. Any date with a format different from that of
#'the first non-blank date in the column will be imported as \code{NA} (missing
#'value). Dates imported as class "Date" using a 4-digit year,
#'2-digit month, and 2-digit day with the period (.), hyphen (-), slash (/), or
#'no separator. Time and date data are imported as class "POSIXct" and
#'assumes the standard POSIX format for date and time.\cr
#'
#' @param file.name a character string specifying the name of the RDB file
#'containing the data to be imported. This should be changed to file.name
#' @param tz a character string indicating the time zone information for data
#'imported as "POSIXct." The default is to use the local setting.
#' @return A data frame with one column for each data column in the CSV
#'file.
#' @note A NULL data frame is created if there are no data in the file.\cr
#' @seealso \code{\link{read.csv}}, \code{\link{scan}},
#'\code{\link{read.table}}, \code{\link{as.Date}}, \code{\link{as.POSIXct}},
#' @keywords manip IO
#' @export
#' @examples
#'
#'## These datasets are available in USGSwsData as text files
#'TestDir <- system.file("misc", package="USGSwsData")
#'TestPart <- importCSV(file.path(TestDir, "TestPart.csv"))
importCSV <- function(file.name="", tz="") {
  ## Coding history:
  ##    2011Feb25 DLLorenz Origial Coding
  ##    2011Jun02 DLLorenz Fixed dated conversion
  ##    2011Jul26 DLLorenz Add single quote for leading 0 data
  ##    2012Mar03 DLLorenz Bug fixes: dates, scan
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2012Nov03 DLLorenz Factor fix
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  ## This function needed to replace default because Excel changes the format
  ## of dates and does not have POSIX as an option (except as Sweden). 
  ##
  Date2character <- function (x, format = "") {
    ## Arguments:
    ##  x (character vector) the data to convert
    ##  format(character scalar) the format (not used in call to this function
    ##
    charToDate <- function(x) {
      ## Argument:
      ##  x (character vector) the data to convert
      ##
      ## Find first possible valid date
      xx <- x[1]
      N <- length(x)
      j <- 1
      while ((is.na(xx) || nchar(xx) < 10L) && (j <- j + 1L) <= N) 
        xx <- x[j]
      if(j <= N) {
        ## Find format
        ## Is sep '-' ?
        seps <- regexpr('-', xx, fixed=TRUE)
        if(seps > 0L) {
          if(seps <= 3L)
            f <- "%m-%d-%Y"
          else
            f<- "%Y-%m-%d"
        }
        else { # Sep must be '/'
          seps <- regexpr('/', xx, fixed=TRUE)
          if(seps > 0L) {
            if(seps <= 3)
              f <- "%m/%d/%Y"
            else
              f<- "%Y/%m/%d"
          }
        }
        return(strptime(x, f))
      }
      else {
        warning("character string is not in a standard unambiguous format")
        return(rep(NA, N))
      }
    }
   res <- if (missing(format)) 
      charToDate(x)
    else strptime(x, format, tz = "GMT")
    as.Date(res)
  }
  ## Start execution
  MetaName <- setFileType(file.name, "meta", replace=TRUE)
  if(file.access(MetaName) < 0L) # No meta information
    return(read.csv(file.name))
  ## Get the meta info and process the file
  MetaData <- scan(MetaName, what='', sep='\n', quiet=TRUE)
  ## Using scan allows quoates to be processed, unlike strsplit
  MetaData <- lapply(MetaData, function(txt)
                     scan(text=txt, quote="'\"", quiet=TRUE, what=""))
  MetaColNames <- sapply(MetaData, function(x) x[1L])
  MetaColTypes <- sapply(MetaData, function(x) x[2L])
  Data <- read.csv(file.name, colClasses = 'character')
  DataColNames <- names(Data)
  for(i in DataColNames) { # loop through all columns, setting type
    Ndx <- which(MetaColNames %in% i)
    if(length(Ndx) > 0) {
      ## Check for single quote for leading 0 data
      if((tmp.loc <- regexpr("-quote$", MetaColTypes[Ndx])) > 0L) {
        ## Strip -quote from column type
        MetaColTypes[Ndx] <- substring(MetaColTypes[Ndx], 1L, tmp.loc-1L)
        ## Strip ' from data--leading and trailing
        Data[[i]] <- sub("^'", "", Data[[i]])
        Data[[i]] <- sub("'$", "", Data[[i]])
      }
      if(MetaColTypes[Ndx] == 'factor') {
        Data[[i]] <- as.factor(Data[[i]])
        Levs <- levels(Data[[i]])
        OldLevs <- MetaData[[Ndx]][-seq(2L)]
        ### Need to check differences
        if(length(OldLevs) > 0L) { # ignore if length is 0
          ck=TRUE # check flag for order in factors
          CkLevs <- setdiff(Levs, OldLevs)
          if(length(CkLevs) > 0L) {
            cat("New factor levels in", i, ":", CkLevs, '\n', sep=' ')
            ck=FALSE
          }
          CkLevs <- setdiff(OldLevs, Levs)
          if(length(CkLevs) > 0L) {
            cat("Old factor levels not in", i, ":", CkLevs, '\n', sep=' ')
            ck=FALSE
          }
          if(ck) { # There is no reason to check order if there were other diffs
            if(any(!(OldLevs == Levs)))
              cat("Sequence of factor leves changed for", i, '\n', sep=' ')
          }
        } # end of checking levels
      } # end of factor processing
      else if(MetaColTypes[Ndx] == 'ordered') {
        OldLevs <- MetaData[[Ndx]][-seq(2L)]
        ### Need to maintain order
        if(length(OldLevs) == 0L) { # Oops
          cat("Warning: No levels specified for ordered factor:", i,
              ', forced to unordered factor\n', sep=' ')
          Data[[i]] <- as.factor(Data[[i]])
        }
        else # OK
          Data[[i]] <- factor(Data[[i]], levels=OldLevs, ordered=TRUE)
      }
      else { # Do not forget QW types
        Data[[i]] <- switch(MetaColTypes[Ndx],
                            numeric=as.numeric(Data[[i]]),
                            integer=as.integer(Data[[i]]),
                            character=Data[[i]],
                            Date=Date2character(Data[[i]]),
                            logical=as.logical(Data[[i]]),
                            POSIXct=as.POSIXct(Data[[i]], tz=tz),
                            type.convert(Data[[i]])) # Unkown
      }
    }
    else
      Data[[i]] <- type.convert(Data[[i]]) # Make the best guess
  }
  return(Data)
}
