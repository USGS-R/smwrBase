#' @title Mathematical Functions for \code{timeDay} objects
#'
#' @description No mathematical functions, such as \code{log} or \code{exp} are allowed 
#'on time-of-day data. An error results when trying to use any mathematical function 
#'on objects of class "timeDay."
#'
#' @name Math-timeDay
#' @rdname Math-timeDay
#' @param x an object of class "timeDay."
#' @keywords methods
#' @exportMethod Math
#' @aliases Math,timeDay-method
setMethod("Math", "timeDay", function(x)
          stop(gettextf("'%s' not defined for timeDay objects", .Generic),
               domain=NA)
          )
