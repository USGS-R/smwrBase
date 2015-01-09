#'Seasonal Categories
#'
#'Create categories for any definitions of seasons by month and day.
#'
#'The default names for the seasons are of the form "Season Ending ...," where
#'... is derived from \code{breaks}.
#'
#' @param x any vector of valid dates or date-time data of class "Date" or "POSIXt."
#' @param breaks either month names of the end of the seasons or specific days
#'in the form of "mm/dd," where mm is the 2-digit month and dd is the 2-digit
#'day. Breaks in the form of "mm/dd" indicate the last day of each season.
##Breaks must be in calendar order.
#' @param Names optional names for the seasons.
#' @return A factor of seasonal categories.
#' @seealso \code{\link{month}}
#' @keywords chron manip category
#' @export
#' @examples
#'
#'## Just two seasons
#'seasons(as.Date(c("2001-03-31", "2001-06-30", "2001-09-30")), breaks=c("June", "December"))
#'## The equivalent using mm/dd format
#'seasons(as.Date(c("2001-03-31", "2001-06-30", "2001-09-30")), breaks=c("06/30", "12/31"))
#'\dontrun{
#'# Apply to a real dataset
#'library(smwrData)
#'data(QW05078470)
#'transform(QW05078470, Seas=seasons(DATES, breaks=c("June", "December")))
#'}
seasons <- function(x, breaks, Names=paste("Season Ending ", breaks, sep="")) {
  ## Coding history:
  ##    2006Apr05 DLLorenz Initial coding
  ##    2006May23 DLLorenz Slight change
  ##    2007Feb08 DLLorenz Added the days.in.month function from hydrograph()
  ##    2009Jul10 DLLorenz Bug fix for missing periods and fixed ends
  ##    2011May25 DLLorenz Begin Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb11 DLLorenz Prep for gitHub
  ##    2013Nov13 DLLorenz Bug fix for mm/dd 
  ##
  ## First, extract Julian days in the year and adjust for leap years
  Jdays <- baseDay(x)
  Names <- Names
  Nseas <- length(Names)
  ## Break breaks into Julian Days
  match1 = pmatch(breaks, month.name)
  if(any(is.na(match1))) { # Alternanate format should be mm/dd
    breaks1 <- paste(breaks, "1972", sep="/") # 1972 allows for leap year
    breaks1 <- as.integer(as.Date(breaks1, format="%m/%d/%Y")) - 729L
  }
  else # Format is month name
    breaks1 <- apply(as.matrix(match1), 1L, function(j) daysInMonth(j, 1960, TRUE))
  ## Pre/Append values to capture the entire year
  breaks1 <- c(-12, breaks1, 400)
  ## Cut into seasons
  Jdays <- cut(Jdays, breaks1, labels=FALSE)
  Jdays[Jdays > Nseas] <- 1L # replace excess over the end with season 1
  ## Levels required for case where at least one season is missing
  return(factor(Jdays, labels=Names, levels=seq(Nseas)))
}
