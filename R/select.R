#'Conditional Element Selection
#'
#'Return the value associated with \code{test} from the supplied vectors.
#'
#'If \code{test} is logical, then if test is \code{TRUE}, return the first
#'argument in \dots{}, otherwise return the second argument.\cr If \code{test}
#'is numeric, then return that value in the list defined by \code{\dots{}}.\cr
#'If \code{test} is character, then return that value in the list defined by
#'\code{\dots{}}, which must be named in the call.\cr If \code{test} is
#'\code{NA}, then return the value specified by \code{na}.
#'
#' @param test a logical, numeric or character vector that indicates which value
#'to select from the data supplied in \code{\dots{}}. See \bold{Details}.
#' @param \dots the values to be selected.
#' @param .pass the value to return for any element of \code{test} that does not
#'match an argument name in \dots{}. Useful only when the class of \code{test}
#'is "character" or "factor."
#' @param na the value to return for any element of \code{test} is \code{NA}.
#' @return A vector of the same length as \code{test} and data values from the
#'values list defined by \code{\dots{}}. The mode of the result will be coerced
#'from the values list defined by \code{\dots{}}.
#' @note This function is designed to replace nested \code{ifelse} expressions.
#'See \bold{Examples}. It is different from \code{switch} in that the value
#'selected from the possible alternatives is selected by the values in
#'\code{test} rather than by a single value.\cr
#'There is a function in the \code{MASS} package that is also called select. If 
#'\code{MASS} is loaded after \code{USGSwsBase}, then this version of \code{select}
#'will not be available to the user.
#' @seealso \code{\link{ifelse}}, \code{\link{switch}}
#' @keywords manip
#' @export
#' @examples
#'
#'## create the test vector
#'testselect <- c(1,2,3,1)
#'## Nested ifelse
#'ifelse(testselect == 1, 1,
#'  ifelse(testselect == 2, 3,
#'    ifelse(testselect == 3, 5, NA)))
#'## Results by select:
#'select(testselect, 1, 3, 5)
select <- function(test,..., .pass=test, na=NA) {
  ## Coding history:
  ##    2006Jun06 DLLorenz Initial coding
  ##    2008Nov10 DLLorenz vectorized version
  ##    2012May11 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2012Aug21 DLLorenz Added .pass for character or factor
  ##    2013Jun20 DLLorenz Bug fix and changed to note MASS issue in doc
  ##
  ## Force to length and Convert dots to data.frame
  test <- as.vector(test)
  dots <- list(...)
  dots <- lapply(dots, function(x, N) rep(x, length.out=N), N=length(test))
  dots <- as.data.frame(dots, stringsAsFactors=FALSE)
  ## make test an index to the column in dots
  if(is.factor(test))
    test <- as.character(test)
  if(is.logical(test))
    test <- 2L - test
  else if(is.character(test)) {
    dots$.pass <- .pass
    test <- match(test, names(dots), nomatch=ncol(dots))
  }
  testr <- seq(along=test)
  ## get the data
  retval <- sapply(testr, function(i) dots[i, test[i]])
  retval[is.na(test)] <- na
  retval
}
