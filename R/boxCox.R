#' Box-Cox Power Transform
#'
#' Functions for transforming and back-transforming data using the Box-Cox power
#'transform, with options to preserve the measurement units.
#'
#' If \code{x} contains missing values, then \code{GM} is computed after
#'omitting the missing values and the output vector has a missing value
#'wherever \code{x} has a missing value.\cr
#'
#'The function \code{boxCox} computes the forward transform and the function
#'\code{IboxCox} computes the inverse [boxCox] transform, or back-transform.
#'
#' @rdname boxCox
#' @param x a numeric vector to be transformed by \code{boxCox} or
#'back-transformed by \code{IboxCox}. Must be strictly positive for the
#'forward trasnformation---the argument
#'\code{alpha} can be used to force positive values. Missing values are
#'allowed and result in corresponding missing values in the output. See \bold{Details}.
#' @param lambda the power term in the Box-Cox transformation. The value of 1 is
#'a linear transform, the value of 0 results in a natural log transform.
#' @param GM the value to use for the geometric mean of \code{x}. If not
#'supplied, then compute the geometric mean (\code{boxCox}) or extract from
#'the attributes of \code{x} (\code{IboxCox}).
#' @param alpha an offset value for \code{x}.
#' @return A numeric vector of the transformed or back-transformed values in
#'\code{x} with an attribute "GM" of the geometric mean.
#' @note The original power transform described by Box and Cox (1964) is
#'adjusted by a power transform of the geometric mean to retain the correct
#'dimensional units of the original data as described in section 13.2 by 
#'Draper and Smith (1998).
#' @seealso \code{\link{hyperbolic}}
#' @references Box, G.E.P., and Cox, D.R., 1964, An analysis of transformations:
#'Journal of the Royal Statistical Society, v. 26, Series B, p. 211--243.\cr
#'Draper, N.R., and Smith, H., 1998, Applied regression analysis: New York,
#'John Wiley and Sons, 706 p.
#' @keywords manip
#' @examples
#'X.test <- c(1,4,9,16,25,36,49)
#'boxCox(X.test)
#'boxCox(X.test, lambda=0)
#' @export
boxCox <- function(x, lambda=1, GM, alpha=0) {
  ## Coding history:
  ##    2008Apr28 DLLorenz Original Coding
  ##    2008Jul03 DLLorenz Tweaks
  ##    2009Apr20 DLLorenz Converted to a single function and
  ##                       divide by GM(x)^(lambda-1) to preserve units
  ##    2009Apr21 DLLorenz Added GM argument so that conversion could be preserved
  ##                       among different data
  ##    2010Dec03 DLLorenz Conversion to R
  ##    2012Aug17 DLLorenx Allow NAs
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##    2013Jun12 DLLorenz Added class, necessary for safe predictions
  ##
  xtest <- min(x + alpha, na.rm=TRUE)
  if(xtest < 0)
    return(x * NA)
  else if(xtest == 0 && lambda <= 0)
    return(x * NA) 
  else if(xtest == 0 && missing(GM))
    return(x * NA)
  if(missing(GM)) # compute the geoemtric mean if GM is missing
    GMx <- exp(mean(log(x + alpha), na.rm=TRUE))
  else
    GMx <- GM
  if(lambda == 0)
    retval <- GMx * log(x + alpha)
  else
    retval <- ((x + alpha)^lambda - 1) / lambda / GMx^(lambda-1)
  attr(retval, "GM") <- GMx
  class(retval) <- "boxCox"
  return(retval)
}

#' @rdname boxCox
#' @export
#' @examples
#'IboxCox(boxCox(1:3, lambda=0), lambda=0) # verify the back-transform
#'## Should return
#'# [1] 1 2 3
#'# attr(,"GM")
#'# [1] 1.817121
IboxCox <- function(x, lambda=1, GM, alpha=0) {
  if(missing(GM)) # get the attribute if GM is missing
    GM <- attr(x, "GM")
  if(lambda == 0)
    xTest <- exp(x / GM)
  else
    xTest <- (lambda * GM^(lambda-1) * x + 1)^(1 / lambda)
  return(xTest)
}

