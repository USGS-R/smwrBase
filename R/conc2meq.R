#'Concentrations to Milliequivalents
#'
#'Convert concentrations in milligrams per liter (mg/L) to milliequivalents per
#'liter.
#'
#'
#' @param conc a numeric vector containing the concentration data in milligrams
#'per liter.
#' @param constituent the name of the constituent. Must be one of "aluminum,"
#'"ammonium," "bicarbonate," "bromide," "calcium," "carbonate," "chloride,"
#'"fluoride," "iron," "magnesium," "manganese," "nitrate as n," "nitrite as n,"
#'"phosphorus as p," "potassium," "sodium," "sulfate," or "sulfide." There must
#'be enough characters in the name to uniquely identify the constituent. The
#'case of the input name is ignored.
#' @return Vector containing the milliequivalent values. Missing values
#'\code{NA}s are returned if the constituent name is invalid.
#' @note The user must verify that the units of concentration are mg/L. Only
#'those constituents that are typically reported in mg/L (rather than
#'micrograms per liter) are provided in this function. Aluminum, iron, and
#'manganese and possibly sulfide are sometimes reported in micrograms per
#'liter. Such values should be divided by 1000.0 before using this function.
#'
#'The conversion for iron assumes that the dissolved iron is iron II.
#'
#'The conversion for phosphorus assumes that most of the phosphorus is
#'divalent. The actual conversion for phosphorus is very dependent on pH.
#'
#'The conversion factors are taken from table 9 (page 56) of Hem (1985). The
#'available conversion factors are stored in the list created by \code{conc.meq} in
#'smwrBase.\cr
#' @references Hem, J.D., 1985, Study and interpretation of the chemical
#'characteristics of natural water: USGS Water-Supply Paper 2254, 263 p.
#' @keywords manip
#' @export
#' @examples
#'
#'conc2meq(c(1,2,3), "Nitrate")
#'# should be: [1] 0.07139 0.14278 0.21417
conc2meq <- function(conc, constituent) {
  ## Coding history:
  ##    2002Jun13 DLLorenz Original coding.
  ##    2002Jul09 DLLorenz Converted constituent names to all lower case.
  ##    2003Jun23 DLLorenz Added ammonium as nh4.
  ##    2003Jun25 JRSlack  Clarified phosphorus.
  ##    2005Jul14 Dllorenz Date fix
  ##    2010Feb11 DLLorenz Changed name of component in conc.meq
  ##    2011Apr26 DLLorenz Conversion to R
  ##    2011Jul06 DLLorenz Prep for package
  ##    2012Dec14 DLLorenz Bug fix for case conversion
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  if(exists("conc.meq", mode="list")) # copy user's version
    conc.conv <- get("conc.meq")
  else
    conc.conv <- conc.meq()
  ## Force match and conversion to lower
  conv.fact <- conc.conv$conversion[pmatch(tolower(constituent), conc.conv$constituent)]
  conc <- conc * conv.fact
  return(conc)
}

