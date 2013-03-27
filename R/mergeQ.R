#'Merge Flow with Water-quality Data
#'
#'Merges the FLOW (or other data) column from one or many daily-value datasets
#'into a water-quality dataset with one or more stations.
#'
#'More than one column can be specified for \code{FLOW} when merging a single
#'station and the flow data are speciifed in \code{Qdata}.
#'
#'@usage mergeQ(QWdata, STAID = "STAID", FLOW = "FLOW", DATES = "DATES", Qdata
#'= NULL, Prefix = NULL, Plot = TRUE, ...)
#'@param QWdata a data frame.
#'@param STAID a character string of the name of the station-identifier column.
#'The column name must agree in water-quality and flow datasets.
#'@param FLOW a character string of the name of the flow column. The column
#'name must agree in flow datasets and will be the name of the FLOW column in
#'the merged dataset. See \bold{Details}
#'@param DATES a character string of the name of the column containing the date
#'information.The column name must agree in water-quality and flow datasets.
#'@param Qdata a data frame containing daily-flow values.
#'@param Prefix a character string indicating the prefix of the names of
#'datasets containing daily flow values.
#'@param Plot a logical value indicating whether to plot the joint distribution
#'of sampled flows and observed flows.  See \bold{Notes} for a description of
#'the plot.
#'@param \dots defines the dataset containing daily flow values for each
#'station identifier.
#'@return A data frame like QWdata with an attached flow column.
#'@note The station-identifier columns must be of class character.\cr
#'
#'The are fours ways to merge flow and water-quality data:\cr
#'
#'A water-quality dataset that contains data for a single site does not require
#'a STAID column.  Qdata must be supplied. See Example 1.\cr
#'
#'A water-quality dataset that contains data for one or more sites can be
#'merged with a dataset that contains flow data for the sites in that
#'water-quality dataset. This method will fail if there is not a complete list
#'of station identifiers in the flow dataset. See Example 2. \cr
#'
#'A water-quality data set that contains data for one or more sites can be
#'merged with flow data sets that have names based on STAID. The structure of
#'the name must be some common prefix followed by the station identifier. The
#'station identifier must conform to a valid S-PLUS name.  This method will
#'fill in missing values (NAs) if a dataset corresponding to a station
#'identifier is not available. See Example 3. \cr
#'
#'A water-quality dataset that contains data for one or more sites can be
#'merged with flow datasets that have arbitrary names. Station identifiers that
#'do not conform to valid S-PLUS names must be quoted. This method will fail if
#'there is not a complete list of station identifiers supplied as arguments.
#'See Example 4. \cr
#'
#'The plot shows the joint distribution of the sampled flows and observed flows
#'from the sampling time period. The quantile-quantile plots are used to assess
#'whether the sampled and observed flows have the same distribution. If the
#'distributions are the same, then the plot will be approximately a straight
#'line (included as a reference line). The extreme points can have more
#'variability than points toward the center. A plot that shows the upper end
#'trailing upward indicates that the largest flows have been under sampled and
#'the sampled data may not give a reliable estimate of loads.
#'@seealso \code{\link{mergeNearest}},
#'@keywords manip
#'@examples
#'
#'library(USGSwsData)
#'data(Q05078470)
#'data(Q05078770)
#'data(Qall)
#'data(QW05078470)
#'data(QWall)
#'#                   Example 1
#'#
#'mergeQ(QW05078470, Qdata=Q05078470, Plot=FALSE)
#'
#'#                   Example 2
#'#
#'mergeQ(QWall, FLOW="Flow", Qdata=Qall, Plot=FALSE)
#'
#'#                   Example 3
#'#
#'mergeQ(QWall, Prefix="Q", Plot=FALSE)
#'
#'#                   Example 4
#'# Note quotes required for station identifiers
#'mergeQ(QWall, "05078470"=Q05078470, "05078770"=Q05078770, Plot=FALSE)
#'
#'

mergeQ <- function(QWdata, STAID="STAID", FLOW="FLOW", DATES="DATES",
                   Qdata=NULL, Prefix=NULL, Plot=TRUE, ...) {
  ## Coding history:
  ##    2006Jan18 DLLorenz Initial verion
  ##    2006Jan30 DLLorenz added Plot option to construct a comparison between
  ##                      sampled flows and actual flows
  ##    2011May27 DLLorenz Conversion to R
  ##    2012Jun05 DLLorenz For single station allow multiple columns
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb03 DLLorenz Prep for gitHub
  ##
  ## Get station ids
  STAs <- unique(QWdata[[STAID]])
  ## If NULL, then assume that it is single station only, and
  ## if length is 1 and Qdata is not null, then merge on Dates
  ## This requires a valid data frame for Qdata
  if(is.null(STAs) || (length(STAs) == 1L && !is.null(Qdata))) {
    if(is.null(Qdata))
      stop("Qdata must be specified if no STAID in QWdata")
    QWdt <- as.integer(as.Date(QWdata[[DATES]]))
    Qdt <- as.integer(as.Date(Qdata[[DATES]]))
    sel <- Qdt %in% QWdt
    for(i in FLOW) {
      flowvar <- i
      QWdata[[flowvar]] <- Qdata[[flowvar]][sel]
      if(Plot) {
        par(ask=TRUE)
        xQ <- QWdata[[flowvar]]
        yQ <- Qdata[[flowvar]][Qdt >= min(QWdt) & Qdt <= max(QWdt)]
        zz <- qqplot(xQ, yQ, plot.it=FALSE)
        ## Remove 0 flows if necessary
        drop <- (zz$x == 0) | (zz$y == 0)
        if(any(drop)) {
          zz$x <- zz$x[!drop]
          zz$y <- zz$y[!drop]
          cat("\nZero flows dropped from plot\n")
        }
        lims <- range(unlist(zz))
        plot(zz, log='xy', xlim=lims, ylim=lims,
             xlab='Sampled Flows', ylab='Observed Flows')
        abline(0,1)
        title(main=flowvar)
      }
    }
    return(QWdata)
  }
  ## If STAID is valid, loop through STAIDS
  if(length(FLOW) > 1L)
    stop("cannot specify more thatn one column for FLOW for potential multiple stations")
  ## Allow case of single STAID in QWdata and no STAID column in Qdata
  if(length(STAs) == 1L && !is.null(Qdata) && is.null(Qdata[[STAID]]))
    Qdata[[STAID]] <- rep(STAs, nrow(Qdata))
  ## Now what was specified (Qdata, Prefix, or ...)
  ## If Qdata or ..., make list
  if(!is.null(Qdata))
    Qdata <- split(Qdata, Qdata[[STAID]])
  else if(is.null(Prefix))
    Qdata <- list(...)
  ## Loop through STAIDS
  QDATA <- NULL
  for(i in STAs) {
    if(!is.null(Prefix)) {
      Qsta <- paste(Prefix, i, sep='') # the flow data set name
      if(!exists(Qsta)) next # skip if not found
      Qsta <- get(Qsta) # get the data
    }
    else # Get from list
      Qsta <- Qdata[[i]]
    if(is.null(Qsta))
      next # Skip if no data
    QWdt <- as.integer(as.Date(QWdata[[DATES]])[QWdata[[STAID]] == i])
    Qsamp <- Qsta[as.integer(as.Date(Qsta[[DATES]])) %in% QWdt,] # get the matching dates
    Qsamp[[STAID]] <- rep(i, nrow(Qsamp))
    QDATA <- rbind(QDATA, Qsamp[,c(STAID, DATES, FLOW)])
    if(Plot) {
      xQ <- Qsamp[[FLOW]]
      Qdt <- as.integer(as.Date(Qsta[[DATES]]))
      yQ <- Qsta[[FLOW]][Qdt >= min(QWdt) & Qdt <= max(QWdt)]
      zz <- qqplot(xQ, yQ, plot.it=FALSE)
      ## Remove 0 flows if necessary
      drop <- (zz$x == 0) | (zz$y == 0)
      if(any(drop)) {
        zz$x <- zz$x[!drop]
        zz$y <- zz$y[!drop]
        cat("\nZero flows dropped from plot for station: ", i,"\n", sep='')
      }
      lims <- range(unlist(zz))
      plot(zz, log='xy', xlim=lims, ylim=lims,
           xlab='Sampled Flows', ylab='Observed Flows')
      abline(0,1)
      title(i)
    }
  }
  ## Merge the data 
  QWdata <- merge(QWdata, QDATA, by=c(STAID, DATES), all.x=TRUE)
  return(QWdata)
}
