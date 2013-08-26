\name{scaleRng}
\alias{scaleRng}
\title{Scale Data}
\usage{
  scaleRng(x, Min = 0, Max = 1,
    x.range = range(x, na.rm = TRUE))
}
\arguments{
  \item{x}{any numeric vector. Missing values are permitted
  and result in missing values in the corresponding
  output.}

  \item{Min}{the minimum of the output range.}

  \item{Max}{the maximum of the output range.}

  \item{x.range}{the input range to map to the output
  range.}
}
\value{
  A numeric vector scaled to the specified range.
}
\description{
  Transforms numeric data to a specified range
}
\note{
  Some applications suggest or require data scaled to a
  consistent range. The function \code{scaleRng} will do
  that and can be used to back-transform the data.
}
\examples{
## simple case with back-transform
scaleRng(c(1.2, 2.3, 3.4, 5.6))
scaleRng(c(0.00, 0.25, 0.50, 1.00), Min=1.2, Max=5.6)
## now set the expected ranges
scaleRng(c(1.2, 2.3, 3.4, 5.6), x.range=c(1, 6))
scaleRng(c(0.04, 0.26, 0.48, 0.92), Min=1, Max=6, x.range=c(0,1))
}
\keyword{manip}

