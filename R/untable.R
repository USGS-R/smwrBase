#'Contingency Table
#'
#'Constructs a data frame from the count data in a contingency table, using the
#'column and row names as classes.
#'
#'
#'@usage untable(x, rows = "Rows", cols = "Columns", counts = FALSE)
#'@param x a contingency table. Missing values are not permitted.
#'@param rows a character string indicating the name of the column containing
#'the data for the rows. The default column name is "Rows."
#'@param cols a character string indicating the name of the column containing
#'the data for the columns. The default column name is "Columns."
#'@param counts a logical value indicating whether there should be one row in
#'the result for each observation, the default \code{counts} = \code{FALSE}, or
#'whether there should be a column that contains the number of counts for each
#'row and column class, \code{counts} = \code{TRUE}.
#'@return A data frame containing two columns named from rows and cols and an
#'optional column named "Counts" if \code{counts} is set to \code{TRUE}.
#'@note The output for this function can be used for input to contingency table
#'analysis functions that require a data frame rather than a contingency table.
#'To convert a column from factor to ordered use the ordered function.
#'@seealso \code{\link{ordered}}
#'@keywords manip
#'@examples
#'
#'## Create a small synthetic data matrix
#'mdat <- matrix(seq(6), nrow = 2, ncol=3,
#'    dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3")))
#'untable(mdat)
#'

untable <- function(x, rows="Rows", cols="Columns", counts=FALSE) {
  ## Coding history:
  ##    2008Jan04 DLLorenz Original Coding
  ##    2008Jan11 DLLorenz Moved to USGS library
  ##    2011Jun06 DLLorenz Begin Conversion to R 
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  if(is.null(dimnames(x)))
    stop("x is not a valid table: no dimnames")
  if(length(dim(x)) != 2L)
    stop("x is not a valid table: not 2-D")
  x.df <- expand.grid(dimnames(x))
  names(x.df) <- make.names(c(rows, cols))
  if(counts)
    x.df <- cbind(x.df, Counts=as.vector(x))
  else
    x.df <- lapply(x.df, function(x, reps) rep(x, times=reps), reps=as.vector(x))
  return(as.data.frame(x.df))
}
