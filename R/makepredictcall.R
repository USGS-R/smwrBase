#' Utility Function for Safe Prediction
#' 
#' A utility to help \code{\link{model.frame.default}} create the correct matrices 
#'when predicting from models with \code{quadratic},
#'\code{hyperbolic}, or \code{boxCox} terms. Used only internally.
#' 
#' @rdname makepredictcall
#' @param var a variable.
#' @param call the term in the formula, as a call.
#' @return A replacement for \code{call} for the prediction variable.
#' @importFrom stats makepredictcall
##'@export
#makepredictcall <- function (var, call){
#
#  UseMethod("makepredictcall")
#}

#' @rdname makepredictcall
#' @export
#' @method makepredictcall quadratic
makepredictcall.quadratic <- function(var, call) {
  if (as.character(call)[1L] != "quadratic") 
    return(call)
  call$center <- attr(var, "xstar")
  return(call)
}

#' @rdname makepredictcall
#' @export
#' @method makepredictcall hyperbolic
makepredictcall.hyperbolic <- function(var, call) {
  if (as.character(call)[1L] != "hyperbolic") 
    return(call)
  call$scale <- attr(var, "scale")
  return(call)
}

#' @rdname makepredictcall
#' @export
#' @method makepredictcall boxCox
makepredictcall.boxCox <- function(var, call) {
  if (as.character(call)[1L] != "boxCox") 
    return(call)
  call$GM <- attr(var, "GM")
  return(call)
}

#' @rdname makepredictcall
#' @export
#' @method makepredictcall scaleRng
makepredictcall.scaleRng <- function(var, call) {
  if (as.character(call)[1L] != "scaleRng") 
    return(call)
  call$Min <- attr(var, "Min")
  call$Max <- attr(var, "Max")
  call$x.range <- attr(var, "x.range")
  return(call)
}