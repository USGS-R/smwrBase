#' @title Arithmetic Operators for \code{timeDay} objects
#'
#' @description Addition of time-of-day data to either "Date" or "POSIXt" classes. 
#'This is useful when dates and times are recorded in separate
#'columns in a dataset.
#'
#' @include timeDay-class.R
#' @name Arith-timeDay
#' @param e1,e2 timeDay and POSIXt or Date objects. Missing values are permitted in 
#' either argument and result in a missing value in the output.
#' @return A vector of class "POSIXct."
#' @import methods
#' @keywords methods manip
#' @examples
#'as.Date("2001-03-04") + as.timeDay("10:00")
#'\dontrun{
#'library(USGSwsData)
#'data(QW05078470)
#'# Note that the result is reported in the local time zone!
#'QW05078470$DATES + as.timeDay(QW05078470$TIMES)
#'}
#' @exportMethod Arith

#' @rdname Arith-timeDay
#' @aliases Arith,timeDay,POSIXt-method
setMethod("Arith", signature(e1="timeDay", e2="POSIXt"), function(e1, e2) {
  ## Only addition allowed
  if(.Generic != "+")
    stop(gettextf("'%s' not defined for 'timeDay' objects", .Generic),
         domain=NA)
  tzone <- attr(e2, "tzone")
  retval <- e2 + e1@time
  attr(e2, "tzone") <- tzone
  retval}
          )

#' @rdname Arith-timeDay
setMethod("Arith", signature( e1="POSIXt", e2="timeDay"), function(e1, e2) {
  ## Only addition allowed
  if(.Generic != "+")
    stop(gettextf("'%s' not defined for 'timeDay' objects", .Generic),
         domain=NA)
  tzone <- attr(e1, "tzone")
  retval <- e1 + e2@time
  attr(e1, "tzone") <- tzone
  retval}
          )
#' @rdname Arith-timeDay
setMethod("Arith", signature(e1="timeDay", e2="Date"), function(e1, e2) {
  ## Only addition allowed
  if(.Generic != "+")
    stop(gettextf("'%s' not defined for 'timeDay' objects", .Generic),
         domain=NA)
  ## Force e2 to POSIXct, format() required to preserve local time
  e2 <- as.POSIXct(format(e2))
    tzone <- attr(e2, "tzone")
  retval <- e2 + e1@time
  attr(e2, "tzone") <- tzone
  retval}
          )
#' @rdname Arith-timeDay
setMethod("Arith", signature( e1="Date", e2="timeDay"), function(e1, e2) {
  ## Only addition allowed
  if(.Generic != "+")
    stop(gettextf("'%s' not defined for 'timeDay' objects", .Generic),
         domain=NA)
  ## Force e1 to POSIXct, format() required to preserve local time
  e1 <- as.POSIXct(format(e1))
    tzone <- attr(e1, "tzone")
  retval <- e1 + e2@time
  attr(e1, "tzone") <- tzone
  retval}
          )

