\name{format.timeDay}
\alias{format.timeDay}
\title{Encode in a Common Format}
\usage{
  format.timeDay(x, format, ...)
}
\arguments{
  \item{x}{the object to be formatted to type "character."}

  \item{format}{the format to use for output. See
  \code{\link{strptime}} for supported format information.}

  \item{\dots}{further arguments to be passed from or to
  other methods.}
}
\value{
  A cahracter string representing the time of day.
}
\description{
  Format an object of class "timeDay."
}
\seealso{
  \code{\link{strptime}}
}
\keyword{format}
\keyword{manip}
\keyword{method}
\keyword{S3method}
\keyword{timeDay}

