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
  A data frame containing the columns site_no, the USGS
  station identifier; parm_cd, the parameter code;
  description, a desciption of the parameter code;
  begin_date, the earliest data available for the parameter
  code data; end_date, the latest data available for the
  parameter code data; and count_nu, the number of values
  avaialbel for the parameter code. ,EndDate, pCode, and
  name.
}
\description{
  Gets a description of unit-value data available for a
  USGS station and the beginning and ending dates of each
  parameter.
}
\examples{
\dontrun{
UVavailable <- whatUV("04027000")
print(UVavailable)
}
}
\references{
  Information about current water conditions in the United
  States and information about unit values can be obtained
  from \code{\link{http://waterdata.usgs.gov/usa/nwis/rt}}.
}
\seealso{
  \code{\link{readNWIS}}
}
\keyword{DataIO}

