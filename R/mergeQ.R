#'Merge Flow into another Dataset
#'
#'Merges the FLOW (or other data) column from one or many daily-value datasets
#'into a another dataset with one or more stations.
#'
#'More than one column can be specified for \code{FLOW} when merging a single
#'station and the flow data are specified in \code{Qdata}.
#'
#' @param QWdata a data frame with at least a date column on which to merge.
#' @param STAID a character string of the name of the station-identifier column.
#'The column name must agree in the \code{QWdata} and flow datasets.
#' @param FLOW a character string of the name of the flow column. The column
#'name must agree in flow datasets and will be the column name in
#'the merged dataset. See \bold{Details}
#' @param DATES a character string of the name of the column containing the date
#'information. The column name must agree in \code{QWdata} and flow datasets.
#'All datasets must be sorted by date.
#' @param Qdata a data frame containing daily-flow values.
#' @param Prefix a character string indicating the prefix of the names of
#'datasets containing daily flow values.
#' @param Plot a logical value indicating whether to plot the joint distribution
#'of sampled flows and observed flows.  See \bold{Notes} for a description of
#'the plot. Used only if a single column is specified in \code{FLOW}.
#' @param \dots defines the dataset containing daily flow values for each
#'station identifier.
#' @return A data frame like \code{QWdata} with an attached flow column(s).
#' @note The station-identifier columns must be of class character.\cr
#'
#'The are fours ways to merge flow and water-quality data:\cr
#'
#'A dataset that contains data for a single site does not require
#'a STAID column.  Qdata must be supplied. This case must be used if
#'the flow record is incomplete or does not cover the range of dates in
#'\code{QWdata} all other methods will fail if that is the case. See Example 1.\cr
#'
#'A dataset that contains data for one or more sites can be
#'merged with a dataset that contains flow data for the sites in that
#'first dataset. This method will fail if there is not a complete list
#'of station identifiers in the flow dataset. See Example 2. \cr
#'
#'A data set that contains data for one or more sites can be
#'merged with flow data sets that have names based on STAID. The structure of
#'the name must be some common prefix followed by the station identifier. The
#'station identifier must conform to a valid name.  This method will
#'fill in missing values (NAs) if a dataset corresponding to a station
#'identifier is not available. See Example 3. \cr
#'
#'A dataset that contains data for one or more sites can be
#'merged with flow datasets that have arbitrary names. Station identifiers that
#'do not conform to valid names must be quoted. This method will fail if
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
#' @seealso \code{\link{mergeNearest}},
#' @keywords manip
#' @export
#' @examples
#'\dontrun{
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
#'}
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
  ##    2013Apr25 DLLorenz Partial fix for incomplete match of flow
  ##                       for a single station using Qdata
  ##    2014Jan06 DLLorenz Error trap for repeated days in QWdata
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
    bsel <- QWdt %in% Qdt
    ## Verify that the lengths are the same
    if(sum(sel) != length(bsel)) {
      stop("Cannot merge--probably because of replicated dates in QWdata")
    }
    for(i in FLOW) {
      flowvar <- i
      flowvals <- Qdata[[flowvar]][sel]
      if(any(!bsel)) { # Need to fill missing matches
        flowtmp <- rep(NA_real_, length(bsel))
        flowtmp[bsel] <- flowvals
        flowvals <- flowtmp
      }
      QWdata[[flowvar]] <- flowvals
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
    if(Plot && length(FLOW) == 1L) {
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
