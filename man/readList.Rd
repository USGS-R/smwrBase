\name{readList}
\alias{readList}
\title{Import Data}
\usage{
  readList(file, names = TRUE, sep = "", nlines = 1,
    convert=NULL)
}
\arguments{
  \item{file}{a character string specifying the name of the
  file.}

  \item{names}{take component names from the first entry in
  the line? If \code{FALSE}, then the components are
  sequentially numbered.}

  \item{sep}{the separator character for the data in each
  line. If a blank string (the default), then any whaite
  space is take as the separator.}

  \item{nlines}{the number of lines that represent a single
  collection of data,}

  \item{convert}{character string indicating how to convert
  the data. Must be a valid value for the \code{Class}
  arguemnt of \code{as}.}
}
\value{
  A list with one component for each \code{nlines} in the
  input file.
}
\description{
  Imports data arranged on lines into a list.
}
\examples{
# Make a small example dataset, and read it
cat("A 1 2 3 4\\nB 5 6 7\\nC 8 9\\n", file="readList.test")
readList("readList.test")
}
\seealso{
  \code{\link{as}}
}
\keyword{IO}
\keyword{manip}

