\name{movingAve}
\alias{movingAve}
\title{Moving Averages}
\usage{
  movingAve(x, span = 3, order = 0, pos = "center")
}
\arguments{
  \item{x}{the data to be averaged or differenced. Missing
  values are permitted but result in missing values in the
  output.}

  \item{span}{the length of the data to be averaged.}

  \item{order}{the polynomial order for averaging. Must be
  less than \code{span}.}

  \item{pos}{how to position the output data relative to
  the value returned; "center" means that the value
  represents the average or difference of the most central
  value realtive to the \code{span}, "end" or "trailing"
  means the the value is the average or difference or the
  preceding \code{span} values, and "begin" or "leading"
  means the value is the average or difference or the
  following \code{span} values.}
}
\value{
  A vector of the same legnth as \code{x} containing the
  averages.
}
\description{
  Filter a regular series of data to compute a moving
  average.
}
\note{
  For odd values of \code{span} and \code{pos} equal to
  "center", \code{order} equal 0 or 1 give the same
  result.\cr In general, there is no reason to use
  polynomial orders greater than 2 and \code{pos} should
  always be set to "center" for polynomial orders greater
  than 1 to avoid strange behavior due to end effects.\cr

  The weights for the averages are computed based on linear
  model theory (Wood and Hockens, 1970). They also discuss
  some artifacts resulting from smoothing.
}
\examples{
## Construct a simple valley
movingData <- abs(seq(-5, 5))
movingAve(movingData, span=5)
movingAve(movingData, span=5, order=2)
}
\references{
  Wood, L.C. and Hockens, S.N., 1970, Least squares
  smoothing operators: Geophysics v. 35, no. 6, p.
  1005-1019.
}
\seealso{
  \code{\link{filter}}, \code{\link{diff}},
  \code{\link{movingDiff}}
}
\keyword{manip}

