#'Merge Datasets
#'
#'Merge two dataset based on the the nearest date between each observation.
#'
#'The format for \code{max.diff} should be a numeric value followed by a
#'description of the time span. The time span must be one of "secs," "mins,"
#'"hours," "days," or "weeks" for seconds, minutes, hours, days, or weeks,
#'respectively.
#'
#' @param left the left-hand dataset.
#' @param dates.left the name of the column of dates in the left-hand dataset.
#' @param all.left logical (\code{TRUE} or \code{FALSE}), include all rows from the 
#'left-hand dataset regardless of
#'whether there is a matching row in the right-hand dataset?
#' @param suffix.left the suffix to apply to common column names in the
#'left-hand dataset.
#' @param right the right-hand dataset.
#' @param dates.right the name of the column of dates in the right-hand dataset.
#' @param suffix.right the suffix to apply to common column names in the
#'right-hand dataset.
#' @param Date.noon logical (\code{TRUE} or \code{FALSE}), adjust columns of 
#'class "Date" to represent a noon observation rather than 12 AM?
#' @param max.diff the maximum allowable difference in time for a match. See
#'\bold{Details}.
#' @return A data.frame of the merged data with common column names renamed by
#'the \code{suffix} arguments to avoid conflict.
#' @note Water-quality data taken at a specific time frequently need to be
#'merged with daily flow data or merged with other water-quality data such as
#'replicate samples or samples of a different medium taken at about the same
#'time, but having a different time stamp.
#' @seealso \code{\link{mergeQ}}
#' @keywords manip
#' @export
#' @examples
#'
#'library(smwrData)
#'data(Q05078470)
#'data(QW05078470)
#'# Set the actual time of sampling in QW05078470
#'QW05078470 <- transform(QW05078470, DATES=DATES + as.timeDay(TIMES))
#'mergeNearest(QW05078470, right=Q05078470)
#'# Notice the difference in selected dates
#'mergeNearest(QW05078470, right=Q05078470, Date.noon=FALSE)
mergeNearest <- function(left, dates.left="DATES", all.left=FALSE,
                         suffix.left="left",
                         right, dates.right="DATES",
                         suffix.right="right",
                         Date.noon=TRUE, max.diff="7 days") {
  ## Coding history:
  ##    2012Jun05 DLLorenz Initial verion
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb03 DLLorenz Prep for gitHub
  ##    2013Jun05 DLLorenz Modify for data containing "qw" columns
  ##
  ## The matching function, find the value in x closest to targ subject to
  ##  the distance being less that maxd:
  pickNear <- function(targ, x, maxd) {
    if(is.na(targ))
      return(0)
    distx <- abs(targ - x)
    distm <- min(distx, na.rm=TRUE)
    if(distm > maxd)
      return(0)
    return(which(distm == distx)[1])
  }
  ## Create dummy rbindQW if necessary
  if(!exists("rbindQW"))
    rbindQW <- rbind
  ## Check to verify that the dates are in POSIXt:
  ##  the converstion to character forces the date to be in local time
  if(inherits(left[[dates.left]], "Date"))
    left[[dates.left]] <- as.POSIXct(as.character(left[[dates.left]])) +
      43200 * Date.noon
  if(inherits(right[[dates.right]], "Date"))
    right[[dates.right]] <- as.POSIXct(as.character(right[[dates.right]])) +
      43200 * Date.noon
  ## Convert max.diff to numeric seconds
  max.diff <- strsplit(max.diff, "  *")
  max.diff <- as.difftime(as.numeric(max.diff[[1]][1]), units=max.diff[[1]][2])
  max.diff <- as.numeric(max.diff, units="secs")
  if(is.na(max.diff))
    stop("cannot convert max.diff")
  ## Create unique column names
  left.names <- names(left)
  right.names <- names(right)
  l.names <- intersect(left.names, right.names)
  if(length(l.names) > 0L) {
    r.names <- intersect(right.names, left.names) # preserves order for right
    names(left)[left.names %in% l.names] <-
      paste(l.names, suffix.left, sep='.')
    names(right)[right.names %in% r.names] <-
      paste(r.names, suffix.right, sep='.')
    ## Update data column names if necessary
    if(dates.left %in% l.names)
      dates.left <- paste(dates.left, suffix.left, sep='.')
    if(dates.right %in% r.names)
      dates.right <- paste(dates.right, suffix.right, sep='.')
  }
  pck <- sapply(as.numeric(left[[dates.left]]), pickNear,
                x=as.numeric(right[[dates.right]]), maxd=max.diff)
  if(all.left) {
    right <- rbindQW(right, NA) # add a row of missings
    pck <- recode(pck, 0, nrow(right))
  }
  else {
    ## remove unmatched values
    left <- left[pck > 0,]
    pck <- pck[pck > 0]
  }
  retval <- cbind(left, right[pck, ], stringsAsFactors = FALSE)
  return(retval)
}
