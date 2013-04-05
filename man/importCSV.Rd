\name{importCSV}
\alias{importCSV}
\title{Import Files}
\usage{
  importCSV(file.name = "")
}
\arguments{
  \item{file.name}{a character string specifying the name
  of the RDB file containing the data to be imported. This
  should be changed to file.name}
}
\value{
  A data frame with one column for each data column in the
  CSV file.
}
\description{
  Imports a comma separated variable file to a data frame.
}
\details{
  All of the dates in a date column must have the same
  format as the first non-blank date in the column. Any
  date with a format different from that of the first
  non-blank date in the column will be imported as
  \code{NA} (missing value). Dates imported as class "Date"
  using a 4-digit year, 2-digit month, and 2-digit day with
  the period (.), hyphen (-), slash (/), or no
  separator.\cr

  If a valid \code{date.format} is supplied, then the data
  are imported using \code{as.POSIXct} and time information
  can be included in the the data. If \code{date.format} is
  "none," then conversion of the date information is
  supressed and the data are retained as character strings.
}
\note{
  A NULL data frame is created if there are no data in the
  file.\cr
}
\examples{
## These datasets are available in USGSwsData as text files
TestDir <- system.file("misc", package="USGSwsData")
TestPart <- importCSV(file.path(TestDir, "TestPart.csv"))
}
\seealso{
  \code{\link{read.csv}}, \code{\link{scan}},
  \code{\link{read.table}}, \code{\link{as.Date}},
  \code{\link{as.POSIXct}},
}
\keyword{IO}
\keyword{manip}

