\name{hysteresis}
\alias{hysteresis}
\title{Compute Hysteresis}
\usage{
  hysteresis(x, step=3)
}
\arguments{
  \item{x}{the sequence of observations. Missing values are
  permitted and will be copied in the output.}

  \item{step}{the number of previous observations to use to
  compute the local mean. See \bold{Note}.}
}
\value{
  The observations in \code{x} with missing values replaced
  by interpolation.
}
\description{
  Compute a basis for estimating hysteresis in some
  variable rlated to \code{x}.
}
\note{
  The basis for estimating hysterisis is the current value
  \code{x} minus the mena of the previous \code{step}
  values. The first \code{step} values in the output will
  be missing, and each missing value will result in
  \code{step} plus 1 missing values.
}
\examples{
library(USGSwsData)
data(Q05078770)
# Plot flow and hysteresis to show looping
with(Q05078770, plot(log(FLOW), hysteresis(log(FLOW), 3), type='l'))
}
\references{
  Garrett, J.D., 2012, Concentrations, loads, and yields of
  select constituents from major tributaries of the
  Mississippi and Missouri Rivers in Iowa, water years
  2004-2008: U.S. Geological Survey Scientific
  Investigations Report 2012--5240, 61 9. Wang, P., and
  Linker, L.C., 2008, Improvement of regression simulation
  in fluvial sediment loads: Journal of Hydraulic
  Engineering, v. 134, no. 10, p. 1,527--1,531.
}
\seealso{
  \code{\link{anomalies}}
}
\keyword{manip}

