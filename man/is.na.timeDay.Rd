\name{is.na.timeDay}
\alias{is.na.timeDay}
\title{Missing Values}
\usage{
  \method{is.na}{timeDay} (x)
}
\arguments{
  \item{x}{the object to be tested.}
}
\value{
  A logical vector of the same length as its argument
  \code{x}, containing TRUE for those elements marked NA
  and FALSE otherwise.
}
\description{
  Indicate which elemnts are missing.
}
\examples{
is.na(as.timeDay(c("10:30", "11:00")))
}
\keyword{manip}

