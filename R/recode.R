#'Recode Data
#'
#'Converts a specified value to another value.
#'
#'
#'@param x a vector. Missing values \code{NA}s are allowed.
#'@param to the replacement value.
#'@param from the target value to match and replace.
#'@return An object like vector with each target value replaced by the
#'specified value.
#'@note When used on numeric (type "double"), the recode
#'function uses an approximate match, within a small tolerance range to avoid
#'mismatches due to computations.\cr The function \code{sub} offers greater
#'flexibility than \code{recode} for replacing parts of text instead of the
#'complete text.
#'@seealso \code{\link{sub}}, \code{\link{na2miss}}, \code{\link{miss2na}},
#'@keywords manip
#'@examples
#'XT <- c(1, 2, 0, 4)
#'recode(XT, 0, 3)
#'@export
recode <- function (x, from, to){
  ## Coding history:
  ##    2012Feb17 DLLorenz Original coding.
  ##    2012Feb17          This version.
  ##
  UseMethod("recode")
}

#' @rdname recode
#' @method recode factor
#' @S3method recode factor
recode.factor <- function (x, from, to)  {
  ## Also valid for ordered
  from <- as.character(from)
  to <- as.character(to)
  levs <- levels(x)
  labs <- levs
  labs[labs == from] <- to
  return(factor(x, levels=levs, labels=labs))
}

#' @rdname recode
#' @method recode integer
#' @S3method recode integer
recode.integer <- function (x, from, to)
  return(ifelse(x == from, to, x))

#' @rdname recode
#' @method recode character
#' @S3method recode character
recode.character <- function (x, from, to)
  return(ifelse(x == from, to, x))

#' @rdname recode
#' @method recode numeric
#' @S3method recode numeric
recode.numeric <- function (x, from, to) {
  ## Be prepared for small differences due to computations
  tol <- max(abs(from) * .Machine$double.eps * 10, .Machine$double.eps*4)
  x[abs(x - from) < tol] <- to
  return(x)
}
