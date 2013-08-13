#' Utility Function for Safe Prediction
#' 
#' A utility to help \code{\link{model.frame.default}} create the right matrices 
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
#' @S3method makepredictcall quadratic
#' @method makepredictcall quadratic
makepredictcall.quadratic <- function(var, call) {
  if (as.character(call)[1L] != "quadratic") 
    return(call)
  call$center <- attr(var, "xstar")
  return(call)
}

#' @rdname makepredictcall
#' @S3method makepredictcall hyperbolic
#' @method makepredictcall hyperbolic
makepredictcall.hyperbolic <- function(var, call) {
  if (as.character(call)[1L] != "hyperbolic") 
    return(call)
  call$scale <- attr(var, "scale")
  return(call)
}

#' @rdname makepredictcall
#' @S3method makepredictcall boxCox
#' @method makepredictcall boxCox
makepredictcall.boxCox <- function(var, call) {
  if (as.character(call)[1L] != "boxCox") 
    return(call)
  call$GM <- attr(var, "GM")
  return(call)
}
