\name{recode}
\alias{recode}
\alias{recode.character}
\alias{recode.factor}
\alias{recode.integer}
\alias{recode.numeric}
\title{Recode Data}
\usage{
  recode(x, from, to)

  \method{recode}{factor}(x, from, to)
    \method{recode}{integer}(x, from, to)
    \method{recode}{character}(x, from, to)
    \method{recode}{numeric}(x, from, to)

  recode.factor(x, from, to)

  recode.integer(x, from, to)

  recode.character(x, from, to)

  recode.numeric(x, from, to)
}
\arguments{
  \item{x}{a vector. Missing values \code{NA}s are
  allowed.}

  \item{to}{the replacement value.}

  \item{from}{the target value to match and replace.}
}
\value{
  An object like vector with each target value replaced by
  the specified value.
}
\description{
  Converts a specified value to another value.
}
\note{
  When used on numeric (type "double"), the recode function
  uses an approximate match, within a small tolerance range
  to avoid mismatches due to computations.\cr The function
  \code{sub} offers greater flexibility than \code{recode}
  for replacing parts of text instead of the complete text.
}
\seealso{
  \code{\link{sub}}, \code{\link{na2miss}},
  \code{\link{miss2na}},
}
\keyword{manip}

