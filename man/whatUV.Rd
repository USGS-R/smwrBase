\name{whatUV}
\alias{whatUV}
\title{Data Inventory}
\usage{
  whatUV(gage)
}
\arguments{
  \item{gage}{a single USGS station identifier as a
  character string.}
}
\value{
  A data frame containing the columns Available Parameters,
  StartDate, ,EndDate,pCode, and name.
}
\description{
  Gets a description of unit-value data available for a
  USGS station and the beginning and ending dates of each
  parameter.
}
\examples{
UVavailable <- whatUV("04027000")
}
\references{
  Refer to NWIS web?
}
\seealso{
  \code{\link{readNWIS}}
}
\keyword{DataIO}

