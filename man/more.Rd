\name{more}
\alias{more}
\title{Display Data}
\usage{
  more(x, n = 20L, ...)
}
\arguments{
  \item{x}{any valid object, generally a data frame, matrix
  or table.}

  \item{n}{a positive integer indicating how many lines to
  print for a page.}

  \item{\dots}{additional arguments to be passed to methods
  for \code{head} or \code{tail}.}
}
\value{
  The object \code{x} is retruned invisibly.
}
\description{
  Display the contents of an object by pages.
}
\note{
  The function \code{more} is intended for interactive
  sessions. If used in a non-interactive session, it simply
  returns \code{x} invisibly.

  Several keyboard commands can be used to view the
  contents of \code{x}. The function \code{more} will
  display \code{n} lines of \code{x} and wait for user
  input. Any of the following commands can be entered by
  the user; either upper or lower case letters are
  accepted.

  q Quit

  t Go to top of \code{x}

  b Go to bottom of \code{x}

  u Go up 1/2 page

  p Go to previous page

  d Go down 1/2 page

  h or ? Print help

  any other letter Go down full page
}
\seealso{
  \code{\link{head}}
}
\keyword{print}

