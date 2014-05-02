\name{makeMeta}
\alias{makeMeta}
\title{Meta Data}
\usage{
  makeMeta(file.name = "")
}
\arguments{
  \item{file.name}{the name of the csv file, which may
  include the path.}
}
\value{
  The name of the meta file that was created.
}
\description{
  Create a template meta file for a csv file. The meta file
  is used by \code{importCSV} to define column types.
}
\note{
  The meta file that is created will only contain column
  types of character, numeric, integer, and logical. It may
  need to be edited by the user to redefine the column
  types actually needed for the data, for example columns
  of class "Date," "POSIXct," or "factor."
}
\seealso{
  \code{\link{importCSV}}
}
\keyword{IO}

