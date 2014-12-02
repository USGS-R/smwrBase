#'Data import, export and manipulation functions
#'
#'This package has specialized functions for importing, managing, or
#'manipulating hydrologic data.
#'
#'\tabular{ll}{ Package: \tab USGSwsBase\cr 
#'Type: \tab Package\cr 
#'Version: \tab 0.9.0\cr 
#'Date: \tab 2014-11-18\cr 
#'License: \tab File LICENSE\cr 
#'Depends: \tab stat, lubridate, methods\cr }
#'
#'This package contains functions that import, manage, or manipipulate hydrologic data and 
#'functions that apply specialized transforms used in hydrologic analyses amd modeling.
#'A listing of the functions and their description is in the following table.
#'
#'\tabular{ll}{ Function \tab Description\cr 
#'%cn% \tab Identify character strings that contain the specified pattern.\cr
#'IboxCox \tab Apply the inverse Box-Cox power transformation.\cr
#'Ihyperbolic  \tab Apply the inverse hyperbolic transformation.\cr
#'IsCurve  \tab Apply the inverse s-curve transformation.\cr
#'anomalies \tab Break down time-series data into long- and short-term deviaitons (anomalies) and the high-frequency variation.\cr
#'as.timeDay \tab Convert data to objects of class "timeDay."\cr
#'baseDay \tab Computes the 'base' day of the year, a reference value that can be used to group days for the computation of summary statistics.\cr
#'boxCox \tab Apply a Box-Cox power transformation.\cr
#'coalesce \tab Merge a matrix or list of vectors selecting the first non missing value.\cr
#'conc.meq \tab Support function for conc2meq—produces a data frame read by conc2meq.\cr
#'conc2meq \tab Convert concentration in milligrams per liter to milli-equivalents per liter.\cr
#'daysInMonth \tab The number of days in a month.\cr
#'dectime \tab Convert dates and times to decimal time in years. \cr
#'dectime2Date \tab Convert decimal time in years to and object of class “Date.”\cr
#'dlpearsonIII \tab The density of the log-Pearson Type III distribution.\cr
#'dms2dd \tab Convert data in degrees, minutes and seconds to decimal degrees.\cr
#'dpearsonIII \tab The density of the Pearson Type III distribution.\cr
#'eventLen \tab Compute the length or duration of an event.\cr
#'eventNum \tab Compute the number of an event, identified by a TRUE value in a sequence.\cr
#'eventSeq \tab Compute the sequence number within an event.\cr
#'eventSeries \tab Create regular time-series data from recorded events.\cr
#'exportCSV \tab Export a data frame to a comma-separated values file.\cr
#'exportRDB \tab Export data to an ASCII relational-database file.\cr
#'fillMissing \tab Interpolate missing values in a regular time-series of data.\cr
#'findSites \tab Find USGS station identifiers.\cr\cr
#'fourier \tab Compute the Fourier series decomposition from date data.\cr
#'group2row \tab Unstack data oriented in columns to rows of data.\cr
#'hyperbolic \tab Apply a hyperbolic transformation.\cr
#'hyseresis \tab Compute a basis for estimating hysteresis effect in some variable related to x.\cr
#'importCSV \tab Import a data frame from a comma-separated values file.\cr\cr
#'importRDB \tab Import a data frame from an ASCII relational-database file.\cr
#'index.coalesce \tab Return the index column number instead of the values for the first non-missing value.\cr
#'isCharLike \tab Determine whether the data be treated like character data.\cr
#'isDateLike \tab Deterimne whether the data be treated like date data.\cr
#'isGroupLike \tab Determine whether the data be treated like grouping data.\cr
#'isNumberLike \tab Determine whether the data be treated as numeric data.\cr
#'makeMeta \tab Create a template meta file for a comma-separated values file.\cr
#'mergeNearest \tab Merge two datasets by the nearest date and time.\cr
#'mergeQ \tab Merge flow data with water-quality data.\cr
#'miss2na \tab Convert a coded missing value to NA.\cr
#'more \tab Display the contents of an object by pages.\cr
#'movingAve \tab Compute the moving average in regular time-series data.\cr
#'movingDiff \tab Compute the moving difference in regular time-series data.\cr
#'na2miss \tab Convert NA to a coded missing value.\cr
#'peaks \tab Compute the indices of peaks in time-series data.\cr
#'plpearsonIII \tab Compute the cumulative probability of the log-Pearson Type III distribution.\cr
#'ppearsonIII \tab Compute the cumulative probability of the Pearson Type III distribution.\cr
#'qlpearsonIII \tab Compute the quantile of the log-Pearson Type III distribution.\cr
#'qpearsonIII \tab Compute the quantile of the Pearson Type III distribution.\cr
#'quadratic \tab Compute a basis for an orthogonal second order polynomial.\cr
#'readList \tab Import data arranged on lines into a list.\cr
#'recode \tab Recode distinct values.\cr
#'regularSeries \tab Put data collected at arbitrary times into a regular time series.\cr
#'rlpearsonIII \tab Compute the Random variates of the log-Pearson Type III distribution.\cr
#'rpearsonIII \tab Compute the Random variates of the Pearson Type III distribution.\cr
#'sCurve \tab Apply the s-curve transformation.\cr
#'scaleRng \tab Scale data to a specified range.\cr
#'screenData \tab Screen data for missing values or gaps.\cr
#'seasons \tab Create seasonal categories from dates.\cr
#'select \tab Select a value based on the value of a logical, integer, or character  reference value.\cr
#'seqCollapse \tab Collapse a sequence of integers to a compact character string.\cr
#'setFileType \tab Support function to manage file suffixes.\cr
#'setTZ \tab Set the time zone information for dates and times.\cr
#'shiftData \tab Shift a time-series forward or backward.\cr
#'sumComposition \tab Compute the percentages of data within a matrix.\cr
#'timeDay \tab Various arithematic methods for manipulating time-of-day data, including signature, as.character.timeDay, etc.\cr
#'untable \tab Expand a 2-dimensional table into the raw values.\cr
#'waterYear \tab Compute the water year of date data. The water year ends on September 30 of the year.\cr
#'whichRowCol \tab Identify the row and column indexes for TRUE values in a logical matrix.\cr
#'}
#' @name USGSwsBase-package
#' @aliases USGSwsBase-package USGSwsBase
#' @docType package
#' @author Dave Lorenz <lorenz@@usgs.gov>
#'
#'Maintainer: Dave Lorenz <lorenz@@usgs.gov>
#' @seealso \code{\link[USGSwsData:USGSwsData-package]{USGSwsData}}
#' @references Lorenz, D.L., in preparation, USGSwsBase---an R package for managing
#'hydrologic data, version 0.9.0: U.S. Geological Survey Open File Report
#'2014-XXXX, ?p.
#' @keywords package
#' @import USGSwsDataRetrieval
#% Check citation too
NULL
