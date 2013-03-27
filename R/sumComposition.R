#'Percentage Composition
#'
#'Compute percentage, or proportional, composition
#'
#'Missing values are permitted in \code{x} or \code{\dots} and result in missing 
#'values in the output.
#'
#'@usage sumComposition(x, ..., Range = 100)
#'@param x any numeric vector, matrix, or data frame containing only numeric
#'columns.
#'@param \dots any additional vectors or matrices.
#'@param Range the output range, generally 100 for percentages or 1 for
#'proportions.
#'@return A matrix with columns matching all of the data in \code{x} and
#'\code{\dots{}} with rows summing to \code{Range}.
#'@keywords manip array
#'@examples
#'
#'# Create tiny dataset
#'TinyCations <- data.frame(Ca=c(32, 47, 28), Mg=c(10,12,15), Na=c(7, 5, 7))
#'sumComposition(TinyCations)
#'

sumComposition <- function(x, ..., Range=100) {
  ## Coding history:
  ##    2008Oct27 DLLorenz Original Code
  ##    2011Jan22 DLLorenz Conversion to R
  ##    2013Feb15 DLLorenz Prep for gitHub
  ##
  if(!missing(..1)) {
    mat <- cbind(x, ...)
    mat <- as.matrix(mat)
  }
  else
    mat <- as.matrix(x)
  rsum <- rowSums(mat)
  mat <- mat / rsum * Range
  return(mat)
}
