#'Export Data
#'
#'Exports a data frame to a text-based file.
#'
#'Setting the \code{meta} argument to \code{TRUE} generates a header that
#'contains a template that can be edited by the user to describe the contents
#'of the file.
#'
#' @param x the data frame to be written.
#' @param file.name a character string naming the file for output.
#' @param col.names a vector of colum names to use instead of the column names
#'in \code{x}.
#' @param meta a logical value indicating whether the header should include a
#'meta-data template for documentation or not.
#' @param code.rule an integer value indicating how many unique numeric values
#'should be included in the meta-data template for cases where each distinct
#'value has a descriptive meaning rather than a numeric meaning.
#' @return The file name is returned.
#' @seealso \code{\link{write.table}}, \code{\link{importRDB}}
#' @keywords manip IO
#' @export
exportRDB <- function(x, file.name="data.rdb", col.names=NULL, meta=FALSE,
                      code.rule=10) {
  ## Coding history:
  ##    2006Jan06 DLLorenz Initial verion
  ##    2009Jul17 DLLorenz Modifications to insert metadata header
  ##    2011Jun02 DLLorenz Conversion to R
  ##    2011Jul06 DLLorenz Prep for package
  ##    2012Jul13 DLLorenz Bug fix
  ##    2012Aug11 DLLorenz Integer fixes
  ##    2013Feb02 DLLorenz Prep for gitHub
  ## 
  ## Fix file output name (postfix .rdb if not .txt)
  if(regexpr("\\.[tT][xX][tT]$", file.name) < 0L)
    file.name <- setFileType(file.name, "rdb", TRUE)
  ## Make good RDB column names
  if(is.null(col.names)) {
    col.names <- names(x)
    ## Column names cannot contain a ., convert to _
    col.names <- gsub(x=col.names, pattern='\\.', replacement='_')
  }
  else if(length(col.names) != ncol(x))
    stop("Length of col.names does not match number fo columns in the dataset")
  ## Make format row and force non-numeric and non-date to be character
  colFmt <- character(length=length(col.names))
  colFmtN <- sapply(x, is.numeric)
  if(any(colFmtN))
    colFmt[colFmtN] <- "16N"
  colFmtD <- sapply(x, isDateLike)
  if(any(colFmtD))
    colFmt[colFmtD] <- "10D"
  colFmtS <- !(colFmtN | colFmtD)
  for(i in which(colFmtS)) {
    x[[i]] <- as.character(x[[i]])
    colFmt[i] <- paste(max(nchar(x[[i]]), 1L), 'S', sep="")
  }
  ## Write the data
  cat("# rdb\n", file=file.name)
  if(meta) { # write the metadata template
    ## Useful stuff
    seplines <- "# ----------------------------------------------------------------------------------------\n#"
    Contact <- list(name="Enter Name", agency="Enter Agency",
                    office="Enter Office or Center",
                    address="Enter address", cityst="Enter City, State, and Zip",
                    voice="Enter Phone", fax="Enter Fax",
                    email="Enter E-mail address")
    Contact.hdr <- c(name="", agency="#                   ",
                     office="#                   ",
                     address="#                   ",
                     cityst="#                   ",
                     voice="#                   voice: ",
                     fax="#                   fax:   ",
                     email="#                   email: ")
    ## Section 1 contact info
    cat(seplines, "\n# Begin METADATA documentation\n#\n# METADATA Date: ",
        date(), "\n# METADATA Contact: ", file=file.name, sep="", append=TRUE)
    if(exists(".Contact"))
      Contact.1 <- paste(Contact.hdr, unlist(get(".Contact")), sep="")
    else
      Contact.1 <- paste(Contact.hdr, unlist(Contact), sep="")
    cat(Contact.1, file=file.name, sep="\n", append=TRUE)
    cat("#\n#  Second Contact:  ", file=file.name, sep="", append=TRUE)
    Contact.1 <- paste(Contact.hdr, unlist(Contact), sep="")
    cat(Contact.1, file=file.name, sep="\n", append=TRUE)
    ## Section 2 data description
    cat("#\n#\n", seplines, "\n# METADATA Data Description:\n#\n#  Filename:",
        file.name, "\n#\n#  The data in this file are discussed in:\n#\n",
        "#  Provide Reference article or report\n#\n",
        "#  Add any description or notes\n#\n",
        "#  The tab-delimited data follwoing this metadata contains 1 row of attribute labels,\n",
        "#  1 row of field descriptions, and ", nrow(x), " rows of data\n#\n",
        file=file.name, sep="", append=TRUE)
    ## Section 3 data elements
    cat(seplines, "# METADATA Basic documentation of data set elements:\n#",
        "#  Data Attributes:\n#", file=file.name, sep="\n", append=TRUE)
    Data.el <- paste("#  ", format(col.names, justify='l'), format(colFmt), sep=" ")
    cat(Data.el,  file=file.name, sep="\n", append=TRUE)
    cat("#\n#  Note: The row of field descriptions describe the width of the field and data type.\n",
        "#        Data type S indicates a text attribute, N indicates a numeric atttribute, and D\n",
        "#        indicates a date attribute. The preceding number indicates the width of the text\n",
        "#        attribute and is fixed as 16 for numeric and 8 for date attributes.\n#\n",
        file=file.name, sep="", append=TRUE)
    ## Section 3 attribute descriptions
    for(i in col.names) {
      cat("#  Attribute label: ", i, "\n#\n",
          "#  Attribute description: Enter Description\n",
          "#  Source: Enter Source of data\n#\n",
          file=file.name, sep="", append=TRUE)
      ## Print codes and descriptions for selected character or factor
      ## Do all remarks code columns with suffix .rmk (changed to _rmk for RDB)
      if(regexpr("_[rR][mM][kK]$", i) > 0L) {
        Levs <- levels(as.factor(x[[make.names(i)]])) # need to convert back to .rmk
        Desc <- ifelse(Levs == "<", "Value less than reported",
                       ifelse(Levs == "E", "Error of value greaer than expected for method",
                              "Value as reported"))
        Codes.out <- paste("#   ", Levs, Desc, sep=' ')
        cat("#   Codes and dedscription:\n", file=file.name, sep="", append=TRUE)
        cat(Codes.out, file=file.name, sep="\n", append=TRUE)
        cat("#\n", file=file.name, sep="", append=TRUE)
      }
      else if(class(x[[make.names(i)]]) %in% c("factor", "character")) {
        ## check to print codes and levels
        doit <- FALSE
        Levs <- levels(as.factor(x[[make.names(i)]]))
        if(is.numeric(code.rule))
          if(length(Levs) <= code.rule)
            doit <- TRUE
        else
          if(make.names(i) %in% code.rule)
            doit <- TRUE
        if(doit) {
          Codes.out <- paste("#   ", Levs, "Enter Description", sep=' ')
          cat("#   Codes and dedscription:\n", file=file.name, sep="", append=TRUE)
          cat(Codes.out, file=file.name, sep="\n", append=TRUE)
          cat("#\n", file=file.name, sep="", append=TRUE)
        }
      } # Done with codes and level logic
      cat("#\n", file=file.name, sep="", append=TRUE)
    } # Done with Section 3
    cat("# End of METADATA documentation\n", file=file.name, sep="", append=TRUE)
  } # Done with meta
  cat(col.names, file=file.name, sep="\t", append=TRUE)
  cat("\n", file=file.name, append=TRUE)
  cat(colFmt, file=file.name, sep="\t", append=TRUE)
  cat("\n", file=file.name, append=TRUE)
  write.table(x, file=file.name, sep="\t", row.names=FALSE, col.names=FALSE,
              na="",, quote=FALSE, append=TRUE)
  invisible(file.name)
}
