\name{as.data.frame.timeDay}
\alias{as.data.frame.timeDay}
\title{Data Frames}
\usage{
  \method{as.data.frame}{timeDay} (x, row.names = NULL,
    optional = FALSE, ..., nm = deparse(substitute(x)))
}
\arguments{
  \item{x}{the object to be converted.}

  \item{row.names}{\code{NULL} or a character vector giving
  the row names for the data frame. Missing values are not
  allowed.}

  \item{optional}{logical. If \code{TRUE}, setting row
  names and converting column names to syntactic names is
  optional.}

  \item{\dots}{additional arguments to be passed to or from
  methods.}

  \item{nm}{the column name to create for \code{x}.}
}
\value{
  A data frame is created containing the data in \code{x}.
}
\description{
  Convert an object to class "data.frame."
}
\seealso{
  \code{\link{as.data.frame}}
}
\keyword{manip}

