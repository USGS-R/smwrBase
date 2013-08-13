\name{seasons}
\alias{seasons}
\title{Seasonal Categories}
\usage{
  seasons(x, breaks,
    Names = paste("Season Ending ", breaks, sep = ""))
}
\arguments{
  \item{x}{any vector of valid dates/times.}

  \item{breaks}{either month names of the end of the
  seasons or specific days in the form of "mm/dd," where mm
  is the 2-digit month and dd is the 2-digit day.}

  \item{Names}{optional names for the seasons.}
}
\value{
  A factor of seasonal categories.
}
\description{
  Create categories for any definitions of seasons by month
  and day.
}
\details{
  The default names for the seasons are of the form "Season
  Ending ...," where ... is derived from \code{breaks}.
}
\examples{
## Just two seasons
seasons(as.Date(c("2001-03-31", "2001-09-30")), breaks=c("June", "December"))
}
\seealso{
  \code{\link{month}}
}
\keyword{category}
\keyword{chron}
\keyword{manip}

