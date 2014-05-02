\name{regularSeries}
\alias{regularSeries}
\title{Regular Series}
\usage{
  regularSeries(x, times, period = "month",
    which = "middle", begin, end, k.period = 1)
}
\arguments{
  \item{x}{a vector of observations that represents a
  series.}

  \item{times}{a date like vector corresponding to data.}

  \item{period}{character string that is valid input to the
  the POSIXct method for seq is OK, specifying the spacing
  between successive periods. For example "year," "month,"
  or "day."}

  \item{which}{a character string indicating the method to
  use, or the name of a function. See \bold{Details} for
  options.}

  \item{begin}{the beginning date as POSIXt or as
  character.}

  \item{end}{the end date as POSIXt or as character.}

  \item{k.period}{the number of units of \code{period} in
  each period of the output series.}
}
\value{
  The function \code{regularSeries} returns a data frame
  with the following columns: \item{Season}{the season
  number.} \item{SeasonStartDate}{the starting date of the
  corresponding season number--the season includes dates
  greater than or equal to this date.}
  \item{SeasonEndDate}{the end date of the corresponding
  season number---the season includes dates strictly less
  than this date.} \item{Value}{the value from \code{x} for
  the corresponding season number.} \item{ValueDate}{the
  date from \code{times} for the corresponding season
  number if \code{which} was one of "earliest," "middle,"
  or "latest," otherwise missing.}
}
\description{
  Some time-series analyses require data that are uniformly
  spaced in time. Theis function will construct a regular
  series from randomly spaced data using any of several
  user-definable methods.
}
\details{
  For \code{regularSeries}, if there is no observation
  during a period, then that value is set to \code{NA}. If
  there is one observation, then the value is set to the
  value of that single observation. The value of
  \code{which} controls how periods with multiple
  observations are handled. Three character strings are
  recognized for selecting a single value: "earliest"
  selects the earliest observation in the period, "middle"
  selects the observation closest to the middle of the
  period, and "latest" selects the latest observation in
  the period. If \code{which} is not one of these, then it
  should be the name of a function such as mean or
  median.\cr
}
\examples{
\dontrun{
library(USGSwsData)
data(QW05078470)
with(QW05078470, regularSeries(P00665, DATES))
# there should be no values for season numbers 2, 5, or 10
}
}
\seealso{
  Refer to the documentation for \code{seaken} in the
  USGSwsStats package if it is installed.
}
\keyword{manip}

