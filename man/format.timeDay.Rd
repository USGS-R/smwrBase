\name{format.timeDay}
\alias{format.timeDay}
\title{Encode in a Common Format}
\usage{
  \method{format}{timeDay} (x, format, ...)
}
\arguments{
  \item{x}{the object to be formatted to type "character."}

  \item{format}{the format to use for output. See
  \code{\link{strptime}} for supported format information.}

  \item{\dots}{not used, required for other methods.}
}
\value{
  A vector of character strings representing the time of
  day values in \code{x}.
}
\description{
  Format an object of class "timeDay."
}
\seealso{
  \code{\link{strptime}}
}
\keyword{manip}

