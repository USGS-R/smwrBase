#' Display Data
#'
#' Display the contents of an object by pages.
#'
#' @param x any valid object, generally a data frame, matrix or table.
#' @param n a positive integer indicating how many lines to print for a page.
#' @param \dots additional arguments to be passed to methods for \code{head} or
#'\code{tail}.
#'
#' @return The object \code{x} is returned invisibly. Sections of \code{x} of
#'length \code{n} are displayed during the execution of the function.
#'
#' @note The function \code{more} is intended for interactive sessions. If used
#'in a non-interactive session, it simply returns \code{x} invisibly.
#'
#'Several keyboard commands can be used to view the contents of \code{x}. The 
#'function \code{more} will display \code{n} lines of \code{x} and wait for 
#'user input. Any of the following commands can be entered by the user; 
#'either upper- or lower-case letters are accepted.
#'
#'\code{q} Quit
#'
#'\code{t} Go to top of \code{x}
#'
#'\code{b} Go to bottom of \code{x}
#'
#'\code{u} Go up 1/2 page
#'
#'\code{p} Go to previous page
#'
#'\code{d} Go down 1/2 page
#'
#'\code{colName/pattern} Search for pattern in the column named colName
#'
#'\code{h} or \code{?} Print help
#'
#'\code{any other letter} Go down full page
#'
#'Searching for a pattern in a column uses \code{grep} to search for the
#'specified pattern in the character representation of the data in the column.
#'This makes it possible to search columns that are not type character.
#' @seealso \code{\link{head}}, \code{\link{tail}}, \code{\link{grep}}
#' @keywords print
#' @export
more <- function(x, n=20L, ...) {
  if(!interactive())
    return(invisible(x))
  ## OK, show parts of the object
  cur <- 0L
  ck <- ""
  skip <- FALSE
  while(!(ck %in% c("q", "Q"))) {
    if(!skip) {
      if(cur == 0L) {
        print(head(x, n=n, ...))
        cur <- n
      } else {
        print(head(tail(x, n=-cur, ...), n=n, ...))
        cur <- cur + n
      }
    }
    skip <- FALSE # get here from help
    ck <- readline()
    if(nchar(ck) > 1L) {
      # Set up to search column
      cat("searching...\n")
      sep <- regexpr("/", ck)
      if(sep < 0) {
        cat(ck, " is not understood\n", sep="")
        skip <- TRUE
      } else {
        # Search for pattern in named column
        colNm <- substring(ck, 1L, sep-1L)
        pat <- substring(ck, sep+1L)
        dta <- eval(x)[[colNm]]
        if(is.null(dta)) {
          cat("Column ", colNm, " not found in dataset\n", sep="")
          skip <- TRUE
        } else {
          dta <- as.character(dta)
          pos <- grep(pat, dta)
          pos <- pos[pos > cur]
          if(length(pos) == 0L) {
            cat(pat, " not found in column ", colNm, "\n", sep="")
            skip <- TRUE
          } else {
            # Finally found it, point to line above
            cur <- pos[1L] - 1L
          }
        }
      }
    } else if(ck %in% c("t", "T")) { # goto top
      cur <- 0L
    } else if(ck %in% c("b", "B")) { # goto bottom if possible
      bottom <- nrow(x)
      if(is.null(bottom)) {
        cat("Cannot go to the bottom of an object that is not rectangular\n")
      } else 
        cur <- bottom - n + 1L
    } else if(ck %in% c("u", "U")) { # go up 1/2 page
      cur <- max(0L, cur - as.integer(1.5*n))
    } else if(ck %in% c("p", "P")) { # go up full page
      cur <- max(0L, cur - 2L*n)
    } else if(ck %in% c("d", "D")) { # go down 1/2 page
      cur <- max(0L, cur - as.integer(0.5*n))
    } else if(ck %in% c("?", "h", "H")) { # help!
      cat("Commands:\n",
          "t ob T         go to the top\n",
          "b or B         go to the bottom (data frames or matrices only)\n",
          "u or U         go up 1/2 page\n",
          "p or P         go up full page\n",
          "d or D         go down 1/2 page\n",
          "col/pat        search for string pat in column col",
          "?, h, or H     Get this help\n",
          "q, Q, or <Esc> Quit\n", 
          "otherwise      go down full page", sep="")
      skip <- TRUE
    }
  }
  invisible(x)
}
