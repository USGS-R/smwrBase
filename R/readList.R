#'Import Data
#'
#'Imports data arranged on lines into a list.
#'
#'@aliases  readList
#'@usage readList(file, names = TRUE, sep = "", nlines = 1,
#'convert=NULL)
#'@param file a character string specifying the name of the file.
#'@param names take component names from the first entry in the line? 
#'If \code{FALSE}, then the components are sequentially numbered.
#'@param sep the separator character for the data in each line. If a blank
#'string (the default), then any whaite space is take as the separator.
#'@param nlines the number of lines that represent a single collection of data,
#'@param convert character string indicating how to convert the data. Must be
#'a valid value for the \code{Class} arguemnt of \code{as}.
#'@return A list with one component for each \code{nlines} in the input file.
#'@seealso \code{\link{as}}
#'@keywords manip IO

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
