#' Print Data
#'
#' Print the contents of an object by pages.
#'
#'@param x any valid object, generally a data frame, matrix or table.
#'@param n a positive integer indicating how many lines to print for a page.
#'@param \dots additional arguments to be passed to methods for \code{head} or
#'\code{tail}.
#'
#'@return The object \code{x} is retruned invisibly.
#'@note The function \code{more} is intended for interactive sessions. If used
#'in a non-interactive session, it simply returns \code{x} invisibly.
#'
#'Several keyboard commands can be used to view the contents of \code{x}. The 
#'function \code{more} will display \code{n} lines of \code{x} and wait for 
#'user input. Any of the following commands can be entered by the user; 
#'either upper or lower case letters are accepted.
#'
#'q Quit
#'
#'t Go to top of \code{x}
#'
#'b Go to bottom of \code{x}
#'
#'u Go up 1/2 page
#'
#'p Go to previous page
#'
#'d Go down 1/2 page
#'
#'h or ? Print help
#'
#'any other letter Go down full page
#'@seealso \code{\link{head}}
#'@keywords print
#'@export
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
    if(ck %in% c("t", "T")) { # goto top
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
          "?, h, or H     Get this help\n",
          "q, Q, or <Esc> Quit\n", 
          "any other      go down full page", sep="")
      skip <- TRUE
    }
  }
  invisible(x)
}
