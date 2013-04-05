#'Identify Rows and Columns
#'
#'Identifies the row and column numbers (indexes) of \code{TRUE} values in a
#'matrix.
#'
#'
#'@usage whichRowCol(x, which = "both")
#'@param x a logical matrix. Missing values are treated as \code{FALSE}
#'@param which a character string indicating what should be returned.
#'@return A matrix of the row and column number if \code{which} is "both."
#'Otherwise a named vector of the row number, if \code{which} is "row," or
#'column number, if \code{which} is "col."  Only the first character is needed.
#'@note Some comparisons, \code{\%in\%} for example, will return a vector rather
#'than a matrix and cause whichRowCol to fail.
#'
#'@seealso \code{\link{row}}, \code{\link{col}}, \code{\link{which}}
#'@keywords manip
#'@export
#'@examples
#'
#'# Simple case to find a single value
#'whichRowCol(matrix(1:20, ncol=4) == 16)
#'# Where are the missing values in a data set?
#'library(USGSwsData)
#'data(MenomineeMajorIons)
#'whichRowCol(sapply(MenomineeMajorIons, is.na))
whichRowCol <- function(x, which="both") {
  ## Coding history:
  ##    2003Oct23 DLLorenz Initial coding
  ##    2006Jun06 DLLorenz Added option to specify rows or cols
  ##    2007Aug29 DLLorenz Added to USGS library
  ##    2010Feb17 DLLorenz Fixed typo in comments
  ##    2011Jun06 DLLorenz Begin Conversion to R 
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  which <- pmatch(which, c("both", "row", "column"), nomatch=0)
  retval <- cbind(Row=row(x)[x], Col=col(x)[x])
  retval <- na.omit(retval)
  attr(retval, "na.action") <- NULL # remove this unneeded info
  if(which == 1L)
    return(retval)
  if(which == 2L)
    return(retval[order(retval[,2L]),1L])
  if(which == 3L)
    return(retval[order(retval[,1L]),2L])
  # invalid option
  return()
}
