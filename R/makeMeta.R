#'Metadata
#'
#'Create a template meta file for a CSV file. The meta file is used by
#'\code{importCSV} to define column types.
#'
#'
#' @param file.name the name of the CSV file, which may include the path.
#' @return The name of the meta file that was created.
#' @note The meta file that is created will only contain column types of
#'character, numeric, integer, and logical. It may need to be edited by the
#'user to redefine the column types actually needed for the data, for example
#'columns of class "Date," "POSIXct," or "factor."
#' @seealso \code{\link{importCSV}}
#' @keywords IO
#' @export
makeMeta <- function(file.name="") {
  ## Coding history:
  ##    2012Nov05 DLLorenz Original Coding
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  ## Get the first few lines and create the data set
  x <- scan(file.name, nmax=6, what="", sep="\n", quiet=TRUE)
  x <- read.csv(text=x, header=TRUE, stringsAsFactors=FALSE)
  ## Get the meta info and process the file
  MetaName <- setFileType(file.name, "meta", replace=TRUE)
  mfile <- file(MetaName, "w") # Open for writing
  Names <- names(x)
  for(i in seq(along=Names)) { # Loop through all columns, setting type
    i.nm <- Names[i]
    Ci <- class(x[[i.nm]])[1]
    cat(i.nm, " ", Ci, file=mfile, "\n", sep='')
  }
  close(mfile)
  return(MetaName)
}
