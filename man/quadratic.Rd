\name{quadratic}
\alias{quadratic}
\title{Linear and Quadratic Terms}
\usage{
  quadratic(x, center = NULL)
}
\arguments{
  \item{x}{a numeric vector. Missing values are permitted
  and result in corresponding missing values in the
  output.}

  \item{center}{an optional value to use for the center of
  \code{x}.}
}
\value{
  A matrix of two columns---the centered value of \code{x}
  and its square.
}
\description{
  Computes orthogonal polynomials of degree 2 (Cohn and
  others, 1992). Used primarily in a linear regression
  formula.
}
\note{
  If \code{center} is specified, then the polynomials will
  not necessairly be orthogonal. If used in a linear
  regression formula, then the coefficient of the linear
  term is the slope at \code{center}.\cr The function
  \code{quadratic} differs from \code{poly} in that the
  data are not scaled, so the regression coefficients are
  directly interpretable in terms of the units of \code{x}.
}
\examples{
## first and second orthogonal polynomials for the sequence from 1 to 10
quadratic(seq(10))
}
\references{
  Cohn, T.A., Caulder, D.L., Gilroy, E.J., Zynjuk, L.D.,
  and Summers, R.M., 1992, The validity of a simple
  statistical model for estimating fluvial constituent
  loads---An empirical study involving nutrient loads
  entering Chesapeake Bay: Water Resources Research, v. 28,
  no. 5, p. 937--942.
}
\seealso{
  \code{\link{poly}}
}
\keyword{manip}

