#'Find Local Maxima
#'
#'Find the local maxima in a vector.
#'
#'Possible values for \code{ties} are "none" treat sequential tied values as
#'individual values; all other values can be thought of as collapsing
#'sequential tied values--"first," "middle," or "last" identify the first,
#'middle, or last of a sequence of ties as the peak if appropriate.
#'
#'@usage peaks(x, span = 3, ties = "first", ends = TRUE)
#'@param x any numeric vector. Missing values are permitted, but supress
#'identifying peaks within \code{span}.
#'@param span The window width, the default value is 3, meaning compare each
#'value to both of its neighbors. The value for \code{span} must be odd and if
#'set to an even value, then it is increased to the next largest odd value.
#'@param ties a character indicating how to handle ties. See \bold{Details}.
#'@param ends a logical value indicating whether or not to include either the
#'first or last observations in the sequence if it is a local maximum.
#'@return A vector matching \code{x} of logical values indicating wether the
#'corresponding element is a local maxiumum or not.
#'@note A peak is defined as an element in a sequence which is strctly greater
#'than all other elements within a window of width \code{span} centered at that
#'element. As such, setting \code{ties} to "none" has the effect of not
#'identifying peaks with sequential tied values.
#'@seealso \code{\link{max}}
#'@keywords manip
#'@examples
#'
#'# Note the effect of missing values
#'peaks(c(1:6,5,4,NA,4,6,9,NA))
#'peaks(c(1:6,NA,5,4,NA,4,6,9,NA))
#'# Note the effect of ties
#'peaks(c(1:6,6,6,5,4,3,4,6,9))
#'peaks(c(1:6,6,6,5,4,3,4,6,9), ties="none")
#'

peaks <- function(x, span=3, ties='first', ends=TRUE) {
  ## Coding history:
  ##    2010Dec14 DLLorenz Original Coding
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb03 DLLorenz Prep for gitHub
  ##
  ## How to handle ties:
  ##  if first, then take the first of a sequence of ties that is a peak
  ##  if last, then take the last in the sequence
  ##  if middle, then take the rounded average of the first and last sequence
  ##     numbers
  ##  if none, then identify no ties, same as strict=T in S+.
  ##
  ties <- match.arg(ties, c('first', 'last', 'middle', 'none'))
  ## Use rle to collapse ties if necessary. Compute index value in lengths.
  if(ties == 'none')
    xrle <- list(lengths=seq(along=x), values=x)
  else if(ties == 'first') {
    xrle <- rle(x)
    xrle$lengths <- cumsum(c(1, xrle$lengths[-length(xrle$lengths)]))
  }
  else if(ties == 'last') {
    xrle <- rle(x)
    xrle$lengths <- cumsum(xrle$lengths)
  }
  else { # must be middle
    xrle <- rle(x)
    xrle$lengths <- round((cumsum(xrle$lengths) + 
                          cumsum(c(1, xrle$lengths[-length(xrle$lengths)])))/2, 0)
  }
  ## Check span and make offsets
  span <- as.integer(span)
  if(span %% 2L != 1L) {
    span <- span + 1L
    warning("span increased to next odd value: ", span, "\n")
  }
  offset <- (span - 1L)/2L
  xtest1 <- xrle$values[1L]
  Ntest <- length(xrle$values)
  xtestL <- xrle$values[Ntest]
  xtest <- c(rep(xtest1 - ends, offset), xrle$values,
             rep(xtestL - ends, offset))
  ## The test must compare all values to those surrounding, but not to itself
  peaks <- sapply(seq(Ntest), function(ndx, xx, off)
                  all(xx[ndx + off] > xx[seq(ndx, ndx+2*off)][-(off+1)]),
                  xx=xtest, off=offset)
  retval <- rep(FALSE, length(x))
  retval[xrle$lengths[peaks]] <- TRUE
  return(retval)
}
