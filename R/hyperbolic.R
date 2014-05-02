#' Hyperbolic transform
#'
#' Functions for transforming and back-transforming data using a hyperbolic
#'function.
#'
#' If \code{x} contains missing values, then \code{scale} is computed after
#'omitting the missing values and the output vector has a missing value
#'wherever \code{x} has a missing value.\cr
#'
#'The basic equation for the hyberbolic transform is 1/(1 + (10^factor * x)/
#'scale). It is adjusted to produce fairly consistent values for small changes
#'in \code{factor}.\cr
#'
#'The function \code{hyperbolic} computes the forward transform and the
#'function \code{Ihyperbolic} computes the inverse [hyperbolic] transform, or back-transform.
#'
#' @aliases hyperbolic Ihyperbolic
#' @param x a numeric vector to be transformed by \code{hyperbolic} or
#'back-trasnformed by \code{Ihyperbolic}. Must be strictly positive. Missing
#'values are allowed. See \bold{Details}.
#' @param factor the hyperbolic adjustment term in the hyperbolic equation.
#' @param scale the scaling factor for the data.
#' @return A numeric vector of the transformed or back-transformed values in
#'\code{x} with an attribute "scale" of the values used for \code{scale}.
#' @note The original hyperbolic transform used a linear factor. The version in
#'these functions uses the common log of the factor to make the factors easier
#'to use.\cr
#'
#'When used with the default value for \code{scale}, \code{factor} values
#'outside the range of +/- 3 have very little effect on the transform.
#' @seealso \code{\link{boxCox}}
#' @references The use of a variable hyperbolic transform to help model the
#'relations between stream water chemistry and flow was first described in:
#'
#'Johnson, N.M., Likens, G.E., Borman, F.H., Fisher, D.W., and Pierce, R.S.,
#'1969, A working model for the variation in stream water chemistry at the
#'Hubbard Brook Experimental Forest, New Hampshire: Water Resources Research,
#'v. 5, no. 6, p. 1353-1363.
#' @keywords manip
#' @examples
#'
#'hyperbolic(1:3) # accept the defaults
#'## Should return
#'# [1] 0.3333333 0.5000000 0.6000000
#'# attr(,"scale")
#'# [1] 2
#' @export
hyperbolic <- function(x, factor = 0, scale = mean(x, na.rm=TRUE)) {
  ## Coding history:
  ##    2010Mar24 DLLorenz First dated version
  ##    2012Aug17 DLLorenz Allow NAs
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##    2013Jun12 DLLorenz Added class, necessary for safe predictions
  ##
  ## Use common logs for factor--easier to understand
  retval <- 1/(1 + (10^factor * x)/scale)
  ## scale to'nice' range
  if(factor < 0)
    retval <-  1 - (1 - retval)*10^-factor
  else if(factor < 1)
    retval <- retval*10^(factor/2)
  else
    retval <- retval*10^(factor-.5)
  ## reverse sense so that increase in x is increase in retval
  retval <- 1 - retval
  attr(retval, "scale") <- scale
  class(retval) <- "hyperbolic"
  return(retval)
}

#' @rdname hyperbolic
#' @export
Ihyperbolic <- function(x, factor = 0, scale) {
  if(missing(scale)) # get the attribute if scale is missing
    scale <- attr(x, "scale")
  ## rescale from 'nice' range
  x <- 1 - x
  if(factor < 0)
    x <- 1 - (1 - x)*10^factor
  else if(factor < 1)
    x <- x*10^(-.5*factor)
  else
    x <- x*10^(0.5 - factor)
  factor <- 10^factor # use common logs so that factor is interpretable
  return(((1/x - 1) * scale)/factor)
}
