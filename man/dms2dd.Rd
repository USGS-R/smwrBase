\name{dms2dd}
\alias{dms2dd}
\title{Decimal Degrees}
\usage{
  dms2dd(x, minutes = NULL, seconds = 0, split = "")
}
\arguments{
  \item{x}{a character or numeric vector coded as degrees,
  minutes, and seconds or a numeric vector of degrees.  The
  character string may contain a leading zero for values
  less than 100. Missing values are permitted and result in
  missing values in the output.}

  \item{minutes}{a vector of minutes. If supplied, then
  \code{x} is assumed to be a numeric vector of degrees.
  Missing values are permitted.}

  \item{seconds}{a vector of seconds. Assumed to be 0 if
  not supplied. Missing values are permitted.}

  \item{split}{the delimiter for \code{x} if \code{x} is
  character.}
}
\value{
  A numeric vector of decimal degrees the same length as
  \code{x}. Missing values are returned whereever \code{x},
  \code{minutes}, or \code{seconds} has a missing value.
}
\description{
  Convert data in degrees, minutes, and seconds format to
  decimal degrees.
}
\examples{
dms2dd(983206) # using a numeric value
# should be [1] 98.535
dms2dd("0983206") # using a character value
# should be [1] 98.535
dms2dd(98, 32, 6) # using numeric values for degrees, minutes and seconds
# should be [1] 98.535
dms2dd("98:32", split=":") # Note seconds not included in text
# should be [1] 98.53333
}
\keyword{manip}

