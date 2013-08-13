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
\title{Arithmetic Methods for \code{timeDay} objects}
\description{
  Addition of time-of-day data to either "Date" or "POSIXt"
  classes.
}
\details{
  Missing values are permitted in either argument and
  result in a missing value in the output.
}
\section{Methods}{
  \describe{

  \item{list("signature(e1 = \"Date\", e2 =
  \"timeDay\")")}{ Addition is permissible for these data.
  }

  \item{list("signature(e1 = \"POSIXt\", e2 =
  \"timeDay\")")}{ Addition is permissible for these data.
  }

  \item{list("signature(e1 = \"timeDay\", e2 =
  \"Date\")")}{ Addition is permissible for these data. }

  \item{list("signature(e1 = \"timeDay\", e2 =
  \"POSIXt\")")}{ Addition is permissible for these data. }
  }
}
\keyword{manip}
\keyword{methods}

