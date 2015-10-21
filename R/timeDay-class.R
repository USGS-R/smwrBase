#'Time of Day
#'
#'Class "timeDay" describes the time of day without any reference to the date.
#'The data are stored as seconds since midnight.
#'
#'
#' @name timeDay-class
#' @rdname timeDay-class
#' @slot time the time of day in seconds since midnight.
#' @slot format a character string indicating the format to display the time of day.
#' @section Objects from the Class: Objects can be created by calls of the form
#'\code{as.timeDay(time, format)}.
#' @keywords classes
#' @exportClass timeDay
#' @examples
#'
#'showClass("timeDay")
#'
setClass("timeDay", slots=list(time="numeric", format="character"))
