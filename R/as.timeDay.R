#'Methods for Function \code{as.timeDay}
#'
#'Valid conversions for function \code{as.timeDay}.
#'
#'Inconsistent formats for \code{time} will result in an error. Missing values 
#'or empty strings in \code{time} will result in missing values in the output.
#'
#' @name as.timeDay
#' @rdname as.timeDay
#' @param time the time of day in character format.
#' @param format The format for converting the time of day. See \code{\link{strptime}}
#'for possible format specifiers.
#' @keywords methods manip
#' @examples
#'as.timeDay("10:00")
#'as.timeDay("3 PM", format="%I %p")
#' @exportMethod as.timeDay
setGeneric("as.timeDay", function(time, format) standardGeneric("as.timeDay")
)

#'@rdname as.timeDay
setMethod("as.timeDay", signature(time="timeDay", format="missing"),
          function(time, format)
          return(time))

#'@rdname as.timeDay
setMethod("as.timeDay", signature(time="numeric", format="missing"),
          function(time, format)
          return(new("timeDay", time=time, format="%H:%M:%S")))

#'@rdname as.timeDay
setMethod("as.timeDay", signature(time="character", format="character"),
          function(time, format) {
            tmp <- strptime(time, format=format)
            return(new("timeDay", time=tmp$hour*3600 + tmp$min*60 + tmp$sec, format=format))
          })

#'@rdname as.timeDay
setMethod("as.timeDay", signature(time="character", format="missing"),
          function(time, format) {
            # set empty strings to NA
            time[!nzchar(time)] <- NA_character_
            ckfmt <- unique(nchar(time[!is.na(time)]))
            if(length(ckfmt) != 1L)
              stop("format for time is not consistent")
            if(ckfmt == 4L)
              format="%H%M"
            else if(ckfmt == 5L)
              format="%H:%M"
            else if(ckfmt == 6L)
              format="%H%M%S"
            else if(ckfmt == 8L)
              format="%H:%M:%S"
            else
              stop("unable to guess a format for time")
            tmp <- strptime(time, format=format)
            return(new("timeDay", time=tmp$hour*3600 + tmp$min*60 + tmp$sec, format=format))
          })

