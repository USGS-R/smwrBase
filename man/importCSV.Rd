\name{importCSV}
\alias{importCSV}
\title{Import Files}
\usage{
  importCSV(file.name = "", tz = "")
}
\arguments{
  \item{file.name}{a character string specifying the name
  of the RDB file containing the data to be imported. This
  should be changed to file.name}

  \item{tz}{a character string indicating the time zone
  information for data imported as "POSIXct." The default
  is to use the local setting.}
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
  the period (.), hyphen (-), slash (/), or no separator.
  Time and date data are imported as class "POSIXct" and
  assumes the standard POSIX format for date and time.\cr
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

