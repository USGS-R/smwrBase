#'Collapse a Sequence
#'
#'Collapse a numeric sequence into a compact form that represents continuous
#'ranges and discontinuous values.
#'
#' @param x an integer vector, missing values and repeated values are permitted
#'and removed before collapsing.
#' @param sequential the separator for sequential values.
#' @param skips the separator for gaps in the sequence
#' @return A character string that represents that data in \code{x} in a compact
#'form. If \code{x} is empty, then "" is returned.
#' @note This function is commonly used to express years in a compact form.
#' @seealso \code{\link{paste}}
#' @keywords manip
#' @export
#' @examples
#'
#'# A single value
#'seqCollapse(1968)
#'# A singe continuous range of values
#'seqCollapse(1968:1992)
#'# A collection of continuous and individual values
#'seqCollapse(c(1968:1992, 1998, 2002, 2006:2012))
seqCollapse <- function(x, sequential="-", skips=",") {
  ## Coding history:
  ##    2007May03 DLLorenz Original Coding
  ##    2012Jun20 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb11 DLLorenz Prep for gitHub
  ##
  x <- as.integer(x)
  x <- x[!is.na(x)] # remove missings
  if(length(x) == 0L)
    return("")
  x <- unique(x) # remove repeated values, this assumes that the data are sorted
  ## This sequence protects against sequential differences that are the same length!
  xdiff <- diff(x)
  xdgt1 <- xdiff > 1L
  xdiff[xdgt1] <- xdiff[xdgt1] + runif(sum(xdgt1))
  x.rle <- rle(xdiff)
  x.out <- x[cumsum(c(1, x.rle$lengths))]
  x.sep <- c(ifelse(x.rle$values == 1, sequential, skips), "")
  x.out <- paste(x.out, x.sep, sep="", collapse="")
  return(x.out)
}

  
