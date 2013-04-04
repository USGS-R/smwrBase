\name{isDateLike}
\alias{isCharLike}
\alias{isDateLike}
\alias{isGroupLike}
\alias{isNumberLike}
\title{Test whether an object can be treated in a particular way}
\usage{
  isCharLike(x)

  isDateLike(x)

  isGroupLike(x)

  isNumberLike(x)
}
\arguments{
  \item{x}{any object.}
}
\value{
  A logical value \code{TRUE} if \code{x} meets the
  criteria, or \code{FALSE} if it does not.
}
\description{
  Tests if an object can be treated as a character, to name
  something; as a date; as a grouping variable, has
  distinct values; or as a number.
}
\details{
  The function \code{isCharLike} tests whether \code{x} is
  of class "character" or "factor." The function
  \code{isDateLike} tests whether \code{x} is of class
  "Date" or "POSIXt." The function \code{isGroupLike} tests
  whether \code{x} is of class "character" or "factor" or
  if \code{x} is of type "integer" or "logical." The
  function \code{isNumberLike} tests whether \code{x} is of
  type "numeric" or of class "Date."
}
\note{
  This function is most useful within other functions to
  control how that function handles a particular argument.
}
\examples{
## The first should be FALSE and the second TRUE
isDateLike(32)
isDateLike(as.Date("2004-12-31"))
}
\seealso{
  \code{\link{class}}, \code{\link{is.numeric}},
  \code{\link{is.factor}}, \code{\link{is.character}},
  \code{\link{is.integer}}, \code{\link{is.logical}}
}
\keyword{manip}

