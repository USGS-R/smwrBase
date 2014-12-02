#'File or Object Name
#'
#'Create a new file or object name from an old name with an optional new
#'suffix.
#'
#' @param filename a single character string of the name of the file or object.
#' @param type character string identifying the new suffix.
#' @param replace logical: replace the current suffix?
#' @return A character string like filename but with a new suffix.
#' @note This function is designed as a support function for many USGS
#'functions.
#' @author Dave Lorenz, original coding by Jim Slack, retired.
#' @keywords MANIP
#' @examples
#'# Replace the .dat suffixze with .txt
#'setFileType("TestName.dat", "txt", replace=TRUE)
#' @export
setFileType <- function(filename, type="tmp", replace=FALSE) {
  ## Coding History:
  ##    2002Nov06 JRSlack  Original coding by modifying asFileType.
  ##    2005Jun21 DLLorenz Modified to append suffix if the is none.
  ##    2005Sep28 DLLorenz Fixed append suffix if the is none.
  ##    2011Feb25 DLLorenz Conversion to R
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb11 DLLorenz Prep for gitHub
  ##
  usgsMatchFileType <- function(filename) {
    regexpr("\\.[^\\/.]*$", filename)
  }
  ## the USGS version matches the last occurrence of . rather than the first
  file.end = nchar(filename)
  type.pos = usgsMatchFileType(filename)
  if(type.pos < 1L) # no suffix
    return(paste(filename, type, sep='.'))
  if(substring(filename, type.pos) != type)
    paste(substring(filename, 1L, last = ifelse(replace, type.pos -
                                    1L, file.end)), type, sep = '.')
  else filename
}
