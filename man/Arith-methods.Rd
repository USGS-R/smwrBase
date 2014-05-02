\docType{methods}
\name{Arith-methods}
\alias{Arith}
\alias{Arith-methods}
\alias{Arith,Date,timeDay}
\alias{Arith,Date,timeDay-method}
\alias{Arith,POSIXt,timeDay}
\alias{Arith,POSIXt,timeDay-method}
\alias{Arith,timeDay,Date}
\alias{Arith,timeDay,Date-method}
\alias{Arith,timeDay,POSIXt-method}
\title{Arithmetic Operators for \code{timeDay} objects}
\description{
  Addition of time-of-day data to either "Date" or "POSIXt"
  classes. This is useful when dates and times are recorded
  in separate columns in a dataset.
}
\details{
  Missing values are permitted in either argument and
  result in a missing value in the output.
}
\section{Methods}{
  \describe{

  \item{signature(e1 = "Date", e2 = "timeDay")}{ Only
  addition is permissible for these data. }

  \item{signature(e1 = "POSIXt", e2 = "timeDay")")}{ Only
  addition is permissible for these data. }

  \item{signature(e1 = "timeDay", e2 = "Date")}{ Only
  addition is permissible for these data. }

  \item{signature(e1 = "timeDay", e2 = "POSIXt")}{ Only
  addition is permissible for these data. } }
}
\examples{
as.Date("2001-03-04") + as.timeDay("10:00")
\dontrun{
library(USGSwsData)
data(QW05078470)
# Note that the result is reported in the local time zone!
QW05078470$DATES + as.timeDay(QW05078470$TIMES)
}
}
\keyword{manip}
\keyword{methods}

