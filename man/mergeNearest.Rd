\name{mergeNearest}
\alias{mergeNearest}
\title{Merge Datasets}
\usage{
  mergeNearest(left, dates.left = "DATES",
    all.left = FALSE, suffix.left = "left", right,
    dates.right = "DATES", suffix.right = "right",
    Date.noon = TRUE, max.diff = "7 days")
}
\arguments{
  \item{left}{the left-hand dataset.}

  \item{dates.left}{the name of the column of dates in the
  left-hand dataset.}

  \item{all.left}{include all rows from the left-hand
  dataset regardless of whether there is a matching row in
  the right-hand dataset?}

  \item{suffix.left}{the suffix to apply to common column
  names in the left-hand dataset.}

  \item{right}{the right-hand dataset.}

  \item{dates.right}{the name of the column of dates in the
  right-hand dataset.}

  \item{suffix.right}{the suffix to apply to common column
  names in the right-hand dataset.}

  \item{Date.noon}{adjust columns of class "Date" to
  represent a noon observation rather than 12 AM?}

  \item{max.diff}{the maximum allowable difference in time
  for a match. See \bold{Details}.}
}
\value{
  A data.frame of the merged data with common column names
  renamed by the \code{suffix} arguments to avoid conflict.
}
\description{
  Merge two dataset based on the the nearest date between
  each observation.
}
\details{
  The format for \code{max.diff} should be a numeric value
  followed by a description of the time span. The time span
  must be one of "secs," "mins," "hours,", "days," or
  "weeks" for seconds, minutes, hours, days, or weeks.
}
\note{
  Water-quality data taken at a specific time frequently
  need to be merged with daily flow data or merged with
  other water-quality data such as replicate samples or
  samples of a different medium taken at about the same
  time, but having a different time stamp.
}
\examples{
library(USGSwsData)
data(Q05078470)
data(QW05078470)
# Set the actual time of sampling in QW05078470
QW05078470 <- transform(QW05078470, DATES=DATES + as.timeDay(TIMES))
mergeNearest(QW05078470, right=Q05078470)
# Notice the difference in selected dates
mergeNearest(QW05078470, right=Q05078470, Date.noon=FALSE)
}
\seealso{
  \code{\link{mergeQ}},
}
\keyword{manip}

