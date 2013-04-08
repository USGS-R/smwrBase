\name{importRDB}
\alias{importRDB}
\title{Import Files}
\usage{
  importRDB(file.name = "", date.format =NULL, convert.type
    = TRUE)
}
\arguments{
  \item{file.name}{a character string specifying the name
  of the RDB file containing the data to be imported. This
  should be changed to file.name}

  \item{date.format}{a character string specifying the
  format of all date columns.}

  \item{convert.type}{convert data according to the format
  line? Setting \code{convert.type} to \code{FALSE} forces
  all data to be imported as character.}
}
\value{
  A data frame with one column for each data column in the
  RDB or CSV file.
}
\description{
  Imports a formatted, tab-delimited file to a data frame.
}
\details{
  All of the dates in a date column must have the same
  format as the first non-blank date in the column. Any
  date with a format different from that of the first
  non-blank date in the column will be imported as
  \code{NA} (missing value). By default, dates are imported
  as class "Date" using a 4-digit year, 2-digit month, and
  2-digit day with the period (.), hyphen (-), slash (/),
  or no separator.\cr

  If a valid \code{date.format} is supplied, then the data
  are imported using \code{as.POSIXct} and time information
  can be included in the the data. If \code{date.format} is
  "none," then conversion of the date information is
  supressed and the data are retained as character strings.
}
\note{
  A NULL data frame is created if there are no data in the
  file.\cr

  The header information contained in the RDB file is
  retained in the output dataset as \code{comment}.
}
\examples{
## These datasets are available in USGSwsData as text files
TestDir <- system.file("misc", package="USGSwsData")
TestFull <- importRDB(file.path(TestDir, "TestFull.rdb"))
}
\seealso{
  \code{\link{scan}}, \code{\link{read.table}},
  \code{\link{as.Date}}, \code{\link{as.POSIXct}},
  \code{\link{comment}}
}
\keyword{IO}
\keyword{manip}

