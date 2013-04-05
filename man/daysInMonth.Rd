\name{daysInMonth}
\alias{daysInMonth}
\title{Days in a Month}
\usage{
  daysInMonth(month, year, cum = FALSE)
}
\arguments{
  \item{month}{the month number, must range in value from 1
  to 12. Missing values are permitted.}

  \item{year}{the calendar year, replicated in length to
  match \code{month}. Missing values are permitted.}

  \item{cum}{a logical value to indicate whether the
  cumulative days in the year \code{cum=TRUE} is returned
  or the number of days in the month.}
}
\value{
  A vector matching \code{month} of the requested number of
  days. Missing values are returned whereever either
  \code{month} or \code{year} is missing.
}
\description{
  Computes the number of days in a month or the total
  number of days in the year to the end of the month.
}
\examples{
## Check February on a leap year and regular year.
daysInMonth(c(2,2), c(2000, 2001))
}
\keyword{manip}

