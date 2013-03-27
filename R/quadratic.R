#'Linear and Quadratic Terms
#'
#'Computes orthogonal polynomials of degree 2 (Cohn and others, 1992). Used
#'primarily in a linear regression formula.
#'
#'
#'@usage quadratic(x, center = NULL)
#'@param x a numeric vector at which to evaluate the polynomial. Missing values
#'are permitted and result in corresponding missing values in the output.
#'@param center an optional value to use for the center of \code{x}.
#'@return A matrix of two columns---the centered value of \code{x} and its
#'square.
#'@note If \code{center} is specified, then the polynomials will not
#'necessairly be orthogonal. If used in a linear regression formula, then the
#'coefficient of the linear term is the slope at \code{center}. If used to
#'predict new values, then \code{center} must be specified, or the predicted
#'values will not be correct. If need be, the regression model can be created
#'using the default value for \code{center} and that value used in the
#'subsequent call to \code{predict}.\cr The function \code{quadratic} differs
#'from \code{poly} in that the data are not scaled, so the regression
#'coefficients are directly interpretable in terms of the units of \code{x}.
#'@seealso \code{\link{poly}}
#'@references Cohn, T.A., Caulder, D.L., Gilroy, E.J., Zynjuk, L.D., and
#'Summers, R.M., 1992, The validity of a simple statistical model for
#'estimating fluvial constituent loads---An empirical study inviolving nutirent
#'loads entering Chesapeake Bay: Water Resources research, v. 28, no. 5, p.
#'937--942.
#'@keywords manip
#'@examples
#'
#'## first and second orthogonal polynomials for the sequence from 1 to 10
#'quadratic(seq(10))
#'

quadratic <- function(x, center=NULL) {
  ## Coding history:
  ##    2009Apr17 DLLorenz Original coding
  ##    2011Nov05 DLLorenz Entry into R package and added center arg
  ##    2013Feb03 DLLorenz Prep for gitHub.
  ##
  if(is.null(center)) {
    ## Compute the centering value
    meanx <- mean(x, na.rm=T)
    xstar <- meanx + sum((x - meanx)^3, na.rm=T) / 2 /
      sum((x - meanx)^2, na.rm=T)
    if(is.na(xstar)) xstar <- meanx # allow for constant
    ## round it to a 'nice' value
    xr <- log10(diff(range(x, na.rm=T)))
    xstar = round(xstar, 3 - xr)
  }
  else
    xstar <- center # Use the supplied value
  retval <- cbind(x-xstar, (x-xstar)^2)
  Names <- paste("(", as.character(xstar), ")", sep='')
  dimnames(retval) <- list(NULL, paste(Names, 1:2, sep=''))
  attr(retval, "xstar") <- xstar
  return(retval)
}
