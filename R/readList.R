#'Import Data
#'
#'Import data arranged on lines into a list.
#'
#' @param file a character string specifying the name of the file.
#' @param names logical, if \code{TRUE}, then take component names from the first 
#'entry in the line. If \code{FALSE}, then the components are sequentially 
#'numbered.
#' @param sep the separator character for the data in each line. If a blank
#'string (the default), then any white space is taken as the separator.
#' @param nlines the number of lines that represent a single collection of data,
#' @param convert character string indicating how to convert the data. Must be
#'a valid value for the \code{Class} argument of \code{as}.
#' @return A list with one component for each \code{nlines} in the input file.
#' @seealso 
#Flip for production/manual
#'\code{\link[methods]{as}}
#\code{as} (in methods package)
#' @keywords manip IO
#' @export
#' @examples
#'# Make a 3-line example dataset with component names A, B, and C.
#'cat("A 1 2 3 4\nB 5 6 7\nC 8 9\n", file="readList.test")
#'# Read the example dataset
#'readList("readList.test")
readList <- function(file, names=TRUE, sep="", nlines=1, convert=NULL) {
  ## Coding history:
  ##    2013Feb01 DLLorenz Original coding
  ##    2013Feb03 DLLorenz Added nlines arg.
  ##
  if(missing(file)) stop("readList requires a file name.")
  fileVecChar <- scan(file, what = "", sep = "\n", quiet=TRUE)
  retval <- list()
  Nlines <- length(fileVecChar)
  if(nlines > 1) {
    if(topad <- (Nlines %% nlines) != 0) {
      warning("Input data not a multiple of ", nlines,
              ", padding last line.")
      fileVecChar <- c(fileVecChar, rep("", topad))
    }
    collapse <- if(sep == "") " " else sep
    fileVecChar <- apply(matrix(fileVecChar, nrow=nlines), 2,
                         paste, collapse=collapse)
    Nlines <- length(fileVecChar)
  }
  for(i in seq(Nlines)) {
    VecChar <- scan(text=fileVecChar[i], what="", sep=sep, quiet=TRUE)
    if(names) {
      ID <- VecChar[1]
      VecChar <- VecChar[-1]
    }
    else
      ID <- i
    retval[[ID]] <- VecChar
  }
  if(!is.null(convert)) {
    ## Try to do a conversion
    attempt <- try(as("", Class=convert), silent=TRUE)
    if(class(attempt) == "try-error")
      warning("No way to convert to class ", convert,
              " returned as character")
    else 
      retval <- lapply(retval, as, Class=convert)
  }
  return(retval)
}
