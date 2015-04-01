#'Insert Missing Data
#'
#'Inserts rows of missing values into a data frame for gaps in a sequence.
#'
#' @param x the data frame.
#' @param col the name of the column that defines the sequence.
#' @param fill logical (\code{TRUE} or \code{FALSE}), fill with many rows of data to 
#'match the sequence?  If \code{FALSE}, insert only a single row. See \bold{Note}.
#' @return A data frame like \code{x}, but with rows of missing values where
#'there is a break in the sequence in column \code{col}.
#' @note Setting \code{fill} to \code{TRUE} is useful for setting up datasets
#'for \code{fillMissing} if there are gaps in the retrieved data.  Setting 
#'\code{fill} to \code{FALSE}, the default, is useful for creating a break 
#'in the sequence for plotting the data.
#' @seealso \code{\link{fillMissing}}, \code{\link{screenData}}
#' @keywords manip
#' @examples
#'\dontrun{
#'library(smwrData)
#'data(Q05078470)
#'# Plot the original data
#'with(Q05078470[100:120, ], plot(DATES, FLOW, type="l"))
#'# Remove 3 rows from the data set
#'Q05078470 <- Q05078470[-(109:111), ]
#'# Plot the data--line drawn throught the missing record
#'with(Q05078470[100:117, ], lines(DATES, FLOW, col="green"))
#'# Insert a missing record
#'Q05078470 <- insertMissing(Q05078470, "DATES")
#'# Now plot to show gap in line
#'with(Q05078470[100:118, ], lines(DATES, FLOW, col="blue"))
#'}
#' @export
insertMissing <- function(x, col, fill=FALSE) {
  ## Coding history:
  ##    2014Jan24 DLLorenz Original Coding
  ##
  ## Get the differences in the column
  coldifs <- diff(x[[col]])
  if(any(coldifs < 0)) {
    stop("the data set is not sorted by col.")
  } else if(any(coldifs == 0))
    stop("the minimum difference in col is zero, cannot establish a sequence")
  diftbl <- table(coldifs)
  if(length(diftbl) == 1L) {
    warning("no gaps in sequence in col")
    return(x)
  } else if(diftbl[1L] < length(coldifs)/3) {
    warning("col does not appear to a sequence or has too many gaps, returning x")
    return(x)
  }
  ## OK insert missings
  misrow <- as.data.frame(lapply(x[1L,], function(dd) NA))
  unit <- coldifs[which(coldifs == names(diftbl)[1L])[1L]] # should retain time unit if nec.
  retval <- NULL
  lastrow <- 1L
  for(i in which(coldifs != unit)) {
    if(fill) {
      from <- x[i, col] + unit
      to <- x[i+1, col] - unit
      seqcol <- seq(from, to, by=unit)
      N <- length(seqcol)
      fillrow <- as.data.frame(lapply(misrow, function(mr) rep(mr, length=N)))
      fillrow[[col]] <- seqcol
    } else {
      # Create an entry near the middle of the gap
      halfway <- as.integer((coldifs[i]/as.double(unit) +0.001)/2) * unit
      fillrow <- misrow
      fillrow[[col]] <- x[i, col] + halfway
    }
    ## Copy fill to bottom
    retval <- rbind(retval, x[seq(lastrow, i), ], fillrow)
    lastrow <- i+1
  }
  ## copy to last row of x
  retval <- rbind(retval, x[seq(lastrow, nrow(x)), ])
  return(retval)
}
