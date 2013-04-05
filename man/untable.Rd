\name{untable}
\alias{untable}
\title{Contingency Table}
\usage{
  untable(x, rows = "Rows", cols = "Columns", counts =
    FALSE)
}
\arguments{
  \item{x}{a contingency table. Missing values are not
  permitted.}

  \item{rows}{a character string indicating the name of the
  column containing the data for the rows. The default
  column name is "Rows."}

  \item{cols}{a character string indicating the name of the
  column containing the data for the columns. The default
  column name is "Columns."}

  \item{counts}{a logical value indicating whether there
  should be one row in the result for each observation, the
  default \code{counts} = \code{FALSE}, or whether there
  should be a column that contains the number of counts for
  each row and column class, \code{counts} = \code{TRUE}.}
}
\value{
  A data frame containing two columns named from rows and
  cols and an optional column named "Counts" if
  \code{counts} is set to \code{TRUE}.
}
\description{
  Constructs a data frame from the count data in a
  contingency table, using the column and row names as
  classes.
}
\note{
  The output for this function can be used for input to
  contingency table analysis functions that require a data
  frame rather than a contingency table. To convert a
  column from factor to ordered use the ordered function.
}
\examples{
## Create a small synthetic data matrix
mdat <- matrix(seq(6), nrow = 2, ncol=3,
   dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3")))
untable(mdat)
}
\seealso{
  \code{\link{ordered}}
}
\keyword{manip}

