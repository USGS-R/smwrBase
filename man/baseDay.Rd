\name{baseDay}
\alias{baseDay}
\title{Base Day}
\usage{
  baseDay(x, numeric = TRUE)
}
\arguments{
  \item{x}{a vector of class POSIXt, Dates, or character
  that represents a date. Missing values are permitted.}

  \item{numeric}{a vector of class POSIXt, Dates, or
  character that represents a date. Missing values are
  permitted.}
}
\value{
  An integer value representing the 'base' day number if
  \code{numeric} is \code{TRUE}. Otherwise a factor with
  levels for every day of the year.
}
\description{
  Computes the 'base' day of the year, a reference value
  that can be used to group days for the computation of
  summary statistics.
}
\details{
  The 'base' day is computed so that all dates have the
  same reference value regardless of whether the year is a
  leap year or not.
}
\examples{
# The default numeric result
baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"))
# The result as a factor
baseDay(c("2000-02-29", "2000-03-01", "2001-03-01"), numeric=FALSE)
}
\keyword{manip}

