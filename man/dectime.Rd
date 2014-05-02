\name{dectime}
\alias{dectime}
\title{Decimal Time}
\usage{
  dectime(dates, times, time.format, date.format,
    Date.noon = TRUE)
}
\arguments{
  \item{dates}{a vector of a valid date object, or
  character representation of dates. Missing values are
  permitted and produce corresponding missing values in the
  output.}

  \item{times}{a character representation of times. Missing
  values are permitted and produce corresponding missing
  values in the output.}

  \item{time.format}{format to convert \code{times}. See
  \bold{Details}.}

  \item{date.format}{format to convert \code{dates} if
  character.}

  \item{Date.noon}{set time to noon for \code{dates} of
  class "Date?" See \bold{Details}.}
}
\value{
  A vector representation of the data in decimal
  format--year and decimal fraction.
}
\description{
  Convert date/time data to be expressed as year and
  fractional part of year. This can be useful for plotting
  or representing time in a regression model.
}
\details{
  The format for \code{times} must be one of "hm," "hms,"
  or "ms." Note that this is actually a conversion
  function, see \bold{See Also}. If \code{times} is
  missing, \code{dates} is class "Date," and
  \code{Date.noon} is \code{TRUE}, then set the time to
  12:00, so that the decimal time represents the center of
  the day.
}
\examples{
dectime("11/11/1918", date.format="\%m/\%d/\%Y")
dectime(1988:1990)
}
\seealso{
  \code{\link{hm}}, \code{\link{strptime}}
}
\keyword{manip}

