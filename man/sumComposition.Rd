\name{sumComposition}
\alias{sumComposition}
\title{Percentage Composition}
\usage{
  sumComposition(x, ..., Range = 100)
}
\arguments{
  \item{x}{any numeric vector, matrix, or data frame
  containing only numeric columns.}

  \item{\dots}{any additional vectors or matrices.}

  \item{Range}{the output range, generally 100 for
  percentages or 1 for proportions.}
}
\value{
  A matrix with columns matching all of the data in
  \code{x} and \code{\dots{}} with rows summing to
  \code{Range}.
}
\description{
  Compute percentage, or proportional, composition
}
\details{
  Missing values are permitted in \code{x} or \code{\dots}
  and result in missing values in the output.
}
\examples{
# Create tiny dataset
TinyCations <- data.frame(Ca=c(32, 47, 28), Mg=c(10,12,15), Na=c(7, 5, 7))
sumComposition(TinyCations)
}
\keyword{array}
\keyword{manip}

