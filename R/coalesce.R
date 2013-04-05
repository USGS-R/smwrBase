#'Replace missing values
#'
#'Construct a vector with as few missing values as possible from a selected
#'sequence of vectors.
#'
#'
#'@aliases coalesce index.coalesce
#'@usage coalesce(mat, ...) 
#'
#'index.coalesce(mat, ...)
#'@param mat a vector or matrix.
#'@param \dots additional vectors or matrices, must have the same number of
#'rows as mat. The last argument can be a constant that would substitute for
#'all remaining missing values.
#'@return For coalesce, a vector in which each element is determined by
#'selecting the first non-missing value in the order in which they are
#'specified in the argument list. The first step is to construct a matrix from
#'all arguments. The output is initially set to column 1, for any missing value
#'in column, the data from column 2 is used and so on until all columns have
#'been searched or all missing values replaced.\cr
#'
#'For index.coalesce, an integer vector indicating which column from mat or
#'vector or constant produced the result in coalesce.
#'@note This function is most useful for creating a column in a dataset from
#'related columns that represent different methods. For example, a single
#'column of alkalinity may be desired when there are multiple columns of
#'alkalinity determined by various methods.
#'@keywords manip
#' @export
#'@examples
#'
#'coalesce(c(1,NA,NA,3), c(2,2,NA,2))
#'# should be: [1]  1  2 NA  3
#'coalesce(c(1,NA,NA,3), c(2,2,NA,2), 0)
#'# should be: [1] 1 2 0 3
coalesce <- function(mat, ...) {
  ## Coding history:
  ##    2003Jun06 DLLorenz Original version.
  ##    2003Oct30 DLLorenz Added to new Library
  ##    2011Apr26 DLLorenz Conversion to R
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  if(!missing(..1))
    mat <- cbind(mat,...)
  mat <- as.matrix(mat)
  nc <- ncol(mat)
  ret.val <- mat[,nc]
  for(i in (nc-1):1)
    ret.val <- ifelse(is.na(mat[,i]), ret.val, mat[,i])
  return(ret.val)
}

index.coalesce <- function(mat, ...) {
  ##
  if(!missing(..1))
    mat <- cbind(mat,...)
  mat <- as.matrix(mat)
  mat <- (!is.na(mat))*rep(1:ncol(mat),each=nrow(mat))+0*mat
  return(coalesce(mat))
}
