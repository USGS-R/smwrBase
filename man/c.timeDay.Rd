\name{c.timeDay}
\alias{c.timeDay}
\title{Concatenate Data}
\usage{
  \method{c}{timeDay} (..., recursive = FALSE)
}
\arguments{
  \item{\dots}{any number of objects that can be converted
  to class "timeDay." The first must be a time-of-day
  object.}

  \item{recursive}{not used, required for other methods.}
}
\value{
  A single vector of class "timeDay."
}
\description{
  Combine time-of-day data into a single vector.
}
\examples{
c(as.timeDay("10:00"), as.timeDay("3 PM", format="\%I \%p"))
}
\keyword{manip}

