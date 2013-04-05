#'Pearson Type III distribution
#'
#'Density, cumulative probability, quantiles and random generation for the
#'Pearson Type III distribution.
#'
#'Elements of x, q, or p that are missing will result in missing values in the
#'retruned data.
#'
#'@rdname PearsonIII
#'@aliases PearsonIII dpearsonIII ppearsonIII qpearsonIII rpearsonIII
#'@usage dpearsonIII(x, mean = 0, sd = 1, skew = 0) 
#'
#'ppearsonIII(q, mean = 0, sd = 1, skew = 0)
#'
#'qpearsonIII(p, mean = 0, sd = 1, skew = 0) 
#'
#'rpearsonIII(n, mean = 0, sd = 1, skew = 0)
#'@param x,q vector of quantiles.
#'@param p vector of probabilities.
#'@param n number of observations. If length(n) > 1, then the length is taken
#'to be the number required.
#'@param mean vector of means of the distribution of the data.
#'@param sd vector of standard deviation of the distribution of the data.
#'@param skew vector of skewness of the distribution of the data.
#'@return Either the density (\code{dpearsonIII}), cumulative probability
#'(\code{ppearsonIII}), quantile (\code{qpearsonIII}), or random sample
#'(\code{rpearsonIII}) for the described distribution.
#'@export
#'@note The log-Pearson Type III distribtuion is used extensitvely in flood-
#'frequency analysis in the United States. The Pearson Type III forms the basis
#'for that distribution.
#'@seealso \code{\link{dlpearsonIII}}, \code{\link{dnorm}}
#'@keywords manip distribution
#'@examples
#'## Simple examples
#'dpearsonIII(c(.5, .75, .9), 1.5, .25, 0)
#'## compare to normal
#'qnorm(c(.5, .75, .9), 1.5, .25)
#'## Make a skewed distribution
#'dpearsonIII(c(.5, .75, .9), 1.5, .25, 0.25)
dpearsonIII <- function(x, mean = 0, sd = 1, skew = 0) {
  ## Coding history:
  ##    2009Aug12 DLLorenz Initial version with combined code
  ##    2009Aug14 DLLorenz Debugged p- functions
  ##    2011Jun07 DLLorenz Conversion to R
  ##    2013Feb03 DLLorenz Prep for gitHub
  ##
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the dgamma function or the dnorm function to return the
  ## quantiles desired.
  if(abs(skew) < 1e-006) return(dnorm(x, mean, sd))
  shape <- 4/skew^2
  rate <- sqrt(shape/sd^2)
  mn <- shape/rate
  if(skew > 0)
    x <- x + mn - mean
  else
    x <- mn - x + mean
  return(dgamma(x, shape, rate))
}

#'@rdname PearsonIII
#'@export
#'@examples
#'## Simple examples
#'ppearsonIII(c(.5, .75, .9), 1.5, .25, 0)
#'## compare to normal
#'qnorm(c(.5, .75, .9), 1.5, .25)
#'## Make a skewed distribution
#'ppearsonIII(c(.5, .75, .9), 1.5, .25, 0.25)
ppearsonIII <- function(q, mean = 0, sd = 1, skew = 0) {
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the dgamma function or the dnorm function to return the
  ## quantiles desired.
  if(abs(skew) < 1e-006) return(pnorm(q, mean, sd))
  shape <- 4/skew^2
  rate <- sqrt(shape/sd^2)
  mn <- shape/rate
  if(skew > 0) {
    q <- q + mn - mean
    return(pgamma(q, shape, rate))
  }
  else {
    q <- mn + mean - q
    return(1 - pgamma(q, shape, rate))
  }
}

#'@rdname PearsonIII
#'@export
#'@examples
#'## Simple examples
#'qpearsonIII(c(.5, .75, .9), 1.5, .25, 0)
#'## compare to normal
#'qnorm(c(.5, .75, .9), 1.5, .25)
#'## Make a skewed distribution
#'qpearsonIII(c(.5, .75, .9), 1.5, .25, 0.25)
qpearsonIII <- function(p, mean = 0, sd = 1, skew = 0) {
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the qgamma function or the qnorm function to return the
  ## quantiles desired.
  if(skew == 0)
    q <- qnorm(p)
  else if(skew > 0) {
    shape <- 4/skew^2
    q <- (qgamma(p, shape) - shape)/sqrt(shape)
  }
  else {
    shape <- 4/skew^2
    q <- (shape - qgamma(1 - p, shape))/sqrt(shape)
  }
  return(q * sd + mean)
}

#'@rdname PearsonIII
#'@export
#'@examples
#' # Simple examples
#'rpearsonIII(c(.5, .75, .9), 1.5, .25, 0)
rpearsonIII <- function(n, mean = 0, sd = 1, skew = 0) {
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the qgamma function or the qnorm function to return the
  ## quantiles desired.
  if(skew == 0)
    q <- rnorm(n)
  else {
    shape <- 4/skew^2
    q <- (rgamma(n, shape) - shape)/sqrt(shape)
    if(skew < 0)
      q <- -q
  }
  return(q * sd + mean)
}
