#' S-Curve Transform
#'
#' Functions for transforming and back-transforming data using an s-shaped
#'curve.
#'
#' The basic equation for the s-curve is \emph{z}/(1 + abs(\emph{z})^\code{shape})^(1/\code{shape}),
#'where \emph{z} is \code{scale}*(\code{x}-\code{location}).\cr
#'
#'The function \code{sCurve} computes the forward transform and the
#'function \code{IsCurve} computes the inverse [sCurve] transform, or back-transform.
#'
#' @aliases sCurve IsCurve
#' @param x a numeric vector to be transformed by \code{sCurve} or
#'back-transformed by \code{IsCurve}. Missing
#'values are allowed and result in corresponding missing values in the output.
#' @param location the transition point in the s-curve transform.
#' @param scale the scaling factor for the data, the slope at the transition
#'point in the s-curve transform. Must be greater than 0.
#' @param shape a value that determines how quickly the curve approaches the
#'limits of -1 or 1. Must be greater than 0.
#' @return A numeric vector of the transformed or back-transformed values in
#'\code{x}.
#' @note The \code{sCurve} function is related to the \code{hyperbolic} 
#'function in that both can represent mixing models for flow in stream water 
#'chemistry. The \code{sCurve} function is more flexible when there are 
#'distinct upper and lower limits to the concentration. The \code{hyperbolic}
#'function is more flexible for open-ended concentrations for either high
#'or low flows. Also, \code{sCurve} would typically use log-transformed
#'values for flow.
#' @seealso \code{\link{hyperbolic}}
#' @keywords manip
#' @examples
#'\dontrun{
#'# Basic changes to the s-curve
#'curve(sCurve(x), -5,5, ylim=c(-1,1))
#'# Shift to left
#'curve(sCurve(x, location=1), -5,5, add=TRUE, col="red")
#'# increase slope
#'curve(sCurve(x, scale=2), -5,5, add=TRUE, col="cyan")
#'# increase rate
#'curve(sCurve(x, shape=2), -5,5, add=TRUE, col="purple")
#'}
#' @export
sCurve <- function(x, location=0, scale=1, shape=1) {
  z <- scale*(x - location)
  retval <- z/(1 + abs(z)^shape)^(1/shape)
  return(retval)
}

#' @rdname sCurve
#' @export
IsCurve <- function(x, location=0, scale=1, shape=1) {
  ## Preserve the sign of x
  zsign <- sign(x)
  x <- abs(x)^shape
  ## Back transform and restore sign
  z <- (x/(1 - x))^(1/shape) * zsign
  retval <- z/scale + location
  return(retval)
}
  