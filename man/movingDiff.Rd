\name{movingDiff}
\alias{movingDiff}
\title{Moving Differences}
\usage{
  movingDiff(x, span = 1, pos = "end")
}
\arguments{
  \item{x}{the data to be averaged or differenced. Missing
  values are permitted but result in missing values in the
  output.}

  \item{span}{the span of the differences.}

  \item{pos}{how to position the output data relative to
  the value returned; "center" means that the value
  represents the average or difference of the most central
  value relative to the \code{span}, "end" or "trailing"
  means the the value is the average or difference or the
  preceding \code{span} values, and "begin" or "leading"
  means the value is the average or difference or the
  following \code{span} values.}
}
\value{
  A vector of the same legnth as \code{x} containing the
  differences.
}
\description{
  Filter a regular series of data to compute a moving
  difference.
}
\note{
  For odd values of \code{span} and \code{pos} equal to
  "center", \code{order} equal 0 or 1 give the same
  result.\cr
}
\examples{
## Construct a simple valley
movingData <- abs(seq(-5, 5))
movingDiff(movingData, span=1)
}
\seealso{
  \code{\link{filter}}, \code{\link{diff}},
  \code{\link{movingAve}}
}
\keyword{manip}

