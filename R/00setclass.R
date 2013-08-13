#' timeDay class
#'
#' The timeDay class is a simple representation of the time of day. The
#'data are stores as seconds since midnight.
#'
#' @name timeDay-class
#' @rdname timeDay-class
#' @exportClass timeDay
setClass("timeDay", representation(time="numeric", format="character"))
