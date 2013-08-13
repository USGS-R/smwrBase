#'Partial Value Matching
#'
#'Matches partial values, such as substrings.
#'
#' @rdname zcn
#' @param x the character vector to be matched. Missing values are permitted.
#' @param pattern the pattern to be matched against, may be a regular
#'expression.
#' @return A vector the same length as \code{x} of locical values indicating
#'whether \code{pattern} is found in the element of \code{x} or not.
#' @seealso \code{\link{\%in\%}}, \code{\link{regexpr}}
#' @keywords manip
#' @export
#' @examples
#'
#'## A simple example
#'c("abc", "def") %cn% 'c'
"%cn%" <- function(x, pattern)
  ## Coding history:
  ##    2009Feb13 DLLorenz Original Coding
  ##    2011Apr26 DLLorenz Conversion to R
  ##    2013Feb02 DLLorenz Prep for gitHub
  sapply(x, function(X, pattern)
         regexpr(pattern, X), pattern = pattern) > 0
