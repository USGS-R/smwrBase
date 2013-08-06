\name{renCol}
\alias{renCol}
\title{Rename Columns}
\usage{
  renCol(data, p00010 = "Wtemp", p00045 = "Precip",
    p00060 = "Flow", p00065 = "GH", p00095 = "SpecCond",
    p00300 = "DO", p00400 = "pH", p62611 = "GWL",
    p63680 = "Turb", p72019 = "WLBLS", ...)
}
\arguments{
  \item{data}{the daily- or unit-values datset retrieved
  from NWISweb.}

  \item{p00010}{the base name for parameter code 00010.}

  \item{p00045}{the base name for parameter code 00045.}

  \item{p00060}{the base name for parameter code 00060.}

  \item{p00065}{the base name for parameter code 00065.}

  \item{p00095}{the base name for parameter code 00095.}

  \item{p00300}{the base name for parameter code 00300.}

  \item{p00400}{the base name for parameter code 00400.}

  \item{p62611}{the base name for parameter code 62611.}

  \item{p63680}{the base name for parameter code 63680.}

  \item{p72019}{the base name for parameter code 72019.}

  \item{\dots}{named arguments for the base name for any
  other parameter code. The form of the name must be like
  pXXXXXX, where XXXXX is the parameter code.}
}
\value{
  A dataset like \code{data} with selected columns renamed.
}
\description{
  Renames columns in a dataset retrieved from NWIS web.
  Daily and unit value columns have names derived from
  their data descriptor, parameter, and statistic codes.
  This function read information from the header to rename
  those columns to be more meaningful to humans.
}
\seealso{
  \code{\link{readNWIS}}
}
\keyword{IO}
\keyword{manip}

