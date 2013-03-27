#'Export Data
#'
#'Exports a data frame to a text-based file.
#'
#'@aliases exportCSV 
#'@usage exportCSV(x, file.name = "") 
#'@param x the data frame to be written.
#'@param file.name a character string naming the file for output.
#'@return The file name is returned.
#'@note The function \code{exportCSV} also writes a meta file that contins
#'information about column formatting.
#'@seealso \code{\link{write.table}}, \code{\link{importCSV}}
#'@keywords manip IO

exportCSV <- function(x, file.name="") {
  ## Coding history:
  ##    2011Feb25 DLLorenz Origial Coding
  ##    2011Jul06 DLLorenz Prep for package
  ##    2011Jul26 DLLorenz Add single quote for leading 0 data
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2012Nov03 DLLorenz Factor fix
  ##    2013Feb02 DLLorenz Prep for gitHub
  ##
  ## Convert any non numeric to character
  x.out <- as.data.frame(lapply(x, function(x) {
    if(is.numeric(x))
      x
    else
      as.character(x)
  }), stringsAsFactors=FALSE)
  ## Check for leading 0 and single quote if necessary
  zero.ck <- sapply(x.out, function(x) {
    if(is.numeric(x))
      return(F)
    length(grep('^0', x)) > 0L})
  for(i in which(zero.ck)) # Not executed if none
    x.out[[i]] <- paste("'", x.out[[i]], "'", sep='')
  write.csv(x.out, file=file.name, quote=TRUE, row.names=FALSE)
  ## Get the meta info and process the file
  MetaName <- setFileType(file.name, "meta", replace=TRUE)
  mfile <- file(MetaName, "w") # Open for writing
  Names <- names(x)
  for(i in seq(along=Names)) { # Loop through all columns, setting type
    i.nm <- Names[i]
    Ci <- class(x[[i.nm]])[1]
    cat(i.nm, Ci, file=mfile, sep=' ')
    if(zero.ck[i]) # Append "-quote"
      cat("-quote", file=mfile, sep='')
    if(Ci %in% c("factor", "ordered"))
      cat(' ', paste("'", levels(x[[i.nm]]), "'", sep=""), file=mfile, sep=' ')
    cat('\n', file=mfile)
    ## need to deal with QW data too
  }
  close(mfile)
  invisible(file.name)
}
