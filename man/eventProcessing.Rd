\name{eventNum}
\alias{eventLen}
\alias{eventNum}
\alias{eventSeq}
\title{Event Processing}
\usage{
  eventNum(event, reset = FALSE, na.fix = FALSE)

  eventLen(eventno, summary = FALSE)

  eventSeq(eventno)

  eventSeq(eventno)

  eventLen(eventno, summary = FALSE)
}
\arguments{
  \item{event}{a logical vector where \code{TRUE} indicates
  that an event occurred. Missing values are treated as
  instructed by \code{na.fix}.}

  \item{reset}{a logical value indicating whether the event
  is assumed to continue until the next event, or only
  while event is \code{TRUE}.}

  \item{na.fix}{the value to use where event has missing
  values (\code{NA}s).}

  \item{eventno}{an integer vector indicating the event
  number. Generally the output from the \code{eventNum}
  function.}

  \item{summary}{a logical value, controlling output. See
  \bold{Value} for details.}
}
\value{
  The function \code{eventNum} returns an integer vector
  the same length as \code{event} indicating the event
  sequence number.\cr

  The function \code{eventLen} returns an integer vector
  the same length as \code{eventno} indicating the sequence
  length of the event if \code{summary} is \code{FALSE}, or
  a named integer vector indicating the sequence length of
  each event if \code{summary} is \code{TRUE}.\cr

  The function \code{eventSeq} returns an integer vector
  the same length as \code{eventno} indicating the sequence
  number of each element in the the event.
}
\description{
  Computes the event number \code{eventNum}, the length of
  events \code{eventlen} or the sequence number for
  individual observations within an event \code{eventSeq}.
}
\examples{
## Notice the difference caused by setting reset to TRUE
eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE))
eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE)
## Notice the difference caused by setting reset to TRUE
eventSeq(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE)))
eventSeq(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE))
## Notice the difference caused by setting reset to TRUE
eventLen(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE))
## This is an example of the summary option
eventLen(eventNum(c(TRUE,TRUE,FALSE,FALSE,TRUE,FALSE), reset=TRUE), summary=TRUE)
}
\keyword{manip}

