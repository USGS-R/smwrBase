\name{mergeQ}
\alias{mergeQ}
\title{Merge Flow with Water-quality Data}
\usage{
  mergeQ(QWdata, STAID = "STAID", FLOW = "FLOW", DATES =
    "DATES", Qdata = NULL, Prefix = NULL, Plot = TRUE, ...)
}
\arguments{
  \item{QWdata}{a data frame.}

  \item{STAID}{a character string of the name of the
  station-identifier column. The column name must agree in
  water-quality and flow datasets.}

  \item{FLOW}{a character string of the name of the flow
  column. The column name must agree in flow datasets and
  will be the name of the FLOW column in the merged
  dataset. See \bold{Details}}

  \item{DATES}{a character string of the name of the column
  containing the date information.The column name must
  agree in water-quality and flow datasets.}

  \item{Qdata}{a data frame containing daily-flow values.}

  \item{Prefix}{a character string indicating the prefix of
  the names of datasets containing daily flow values.}

  \item{Plot}{a logical value indicating whether to plot
  the joint distribution of sampled flows and observed
  flows.  See \bold{Notes} for a description of the plot.}

  \item{\dots}{defines the dataset containing daily flow
  values for each station identifier.}
}
\value{
  A data frame like QWdata with an attached flow column.
}
\description{
  Merges the FLOW (or other data) column from one or many
  daily-value datasets into a water-quality dataset with
  one or more stations.
}
\details{
  More than one column can be specified for \code{FLOW}
  when merging a single station and the flow data are
  speciifed in \code{Qdata}.
}
\note{
  The station-identifier columns must be of class
  character.\cr

  The are fours ways to merge flow and water-quality
  data:\cr

  A water-quality dataset that contains data for a single
  site does not require a STAID column.  Qdata must be
  supplied. See Example 1.\cr

  A water-quality dataset that contains data for one or
  more sites can be merged with a dataset that contains
  flow data for the sites in that water-quality dataset.
  This method will fail if there is not a complete list of
  station identifiers in the flow dataset. See Example 2.
  \cr

  A water-quality data set that contains data for one or
  more sites can be merged with flow data sets that have
  names based on STAID. The structure of the name must be
  some common prefix followed by the station identifier.
  The station identifier must conform to a valid S-PLUS
  name.  This method will fill in missing values (NAs) if a
  dataset corresponding to a station identifier is not
  available. See Example 3. \cr

  A water-quality dataset that contains data for one or
  more sites can be merged with flow datasets that have
  arbitrary names. Station identifiers that do not conform
  to valid S-PLUS names must be quoted. This method will
  fail if there is not a complete list of station
  identifiers supplied as arguments. See Example 4. \cr

  The plot shows the joint distribution of the sampled
  flows and observed flows from the sampling time period.
  The quantile-quantile plots are used to assess whether
  the sampled and observed flows have the same
  distribution. If the distributions are the same, then the
  plot will be approximately a straight line (included as a
  reference line). The extreme points can have more
  variability than points toward the center. A plot that
  shows the upper end trailing upward indicates that the
  largest flows have been under sampled and the sampled
  data may not give a reliable estimate of loads.
}
\examples{
library(USGSwsData)
data(Q05078470)
data(Q05078770)
data(Qall)
data(QW05078470)
data(QWall)
#                   Example 1
#
mergeQ(QW05078470, Qdata=Q05078470, Plot=FALSE)

#                   Example 2
#
mergeQ(QWall, FLOW="Flow", Qdata=Qall, Plot=FALSE)

#                   Example 3
#
mergeQ(QWall, Prefix="Q", Plot=FALSE)

#                   Example 4
# Note quotes required for station identifiers
mergeQ(QWall, "05078470"=Q05078470, "05078770"=Q05078770, Plot=FALSE)
}
\seealso{
  \code{\link{mergeNearest}},
}
\keyword{manip}

