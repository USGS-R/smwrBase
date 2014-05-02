\docType{methods}
\name{as.timeDay-methods}
\alias{as.timeDay}
\alias{as.timeDay-methods}
\alias{as.timeDay,character,character-method}
\alias{as.timeDay,character,missing-method}
\alias{as.timeDay,numeric,missing-method}
\alias{as.timeDay,timeDay,missing-method}
\title{Methods for Function \code{as.timeDay}}
\description{
  Valid conversion for function \code{as.timeDay}.
}
\details{
  Inconsistent formats for \code{time} will result in an
  error. Missing values in \code{time} will result in
  missing values in the output.
}
\section{Methods}{
  \describe{

  \item{list(signature(time = character, format =
  character))}{ Convert from character using a specific
  format. }

  \item{list(signature(time = character, format =
  missing))}{ Convert from character using a generic
  format. }

  \item{list(signature(time = numeric, format = missing))}{
  Convert from seconds storead as numeric values. }

  \item{list(signature(time = timeDay, format = missing))}{
  No change to "timeDay" objects. } }
}
\examples{
as.timeDay("10:00")
as.timeDay("3 PM", format="\%I \%p")
}
\keyword{manip}
\keyword{methods}

