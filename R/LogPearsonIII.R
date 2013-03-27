#'Log-Pearson Type III distribution
#'
#'Density, cumulative probability, quantiles and random generation for the
#'log-Pearson Type III distribution.
#'
#'Elements of x, q, or p that are missing will result in missing values in the
#'retruned data.
#'
#'@rdname LogPearsonIII
#'@aliases LogPearsonIII dlpearsonIII plpearsonIII qlpearsonIII rlpearsonIII
#'@usage dlpearsonIII(x, meanlog = 0, sdlog = 1, skew = 0)
#'
#' plpearsonIII(q, meanlog = 0, sdlog = 1, skew = 0)
#'
#' qlpearsonIII(p, meanlog = 0, sdlog = 1, skew = 0)
#'
#' rlpearsonIII(n, meanlog = 0, sdlog = 1, skew = 0)
#'@param x,q vector of quantiles.
#'@param p vector of probabilities.
#'@param n number of observations. If length(n) > 1, then the length is taken
#'to be the number required.
#'@param meanlog vector of means of the distribution of the log-transformed
#'data.
#'@param sdlog vector of standard deviation of the distribution of the
#'log-transformed data.
#'@param skew vector of skewness of the distribution of the log-transformed
#'data.
#'@return Either the density (\code{dlpearsonIII}), cumulative probability
#'(\code{plpearsonIII}), quantile (\code{qlpearsonIII}), or random sample
#'(\code{rlpearsonIII}) for the described distribution.
#'@note The log-Pearson Type III distribtuion is used extensitvely in flood-
#'frequency analysis in the United States.
#'@seealso \code{\link{dpearsonIII}}, \code{\link{dlnorm}}
#'@keywords manip distribution
#'@examples
#'
#'## Simple examples
#'qlpearsonIII(c(.5, .75, .9), 1.5, .25, 0)
#'## compare to normal
#'qlnorm(c(.5, .75, .9), 1.5, .25)
#'## Make a skewed distribution
#'qlpearsonIII(c(.5, .75, .9), 1.5, .25, 0.25)
#'

dlpearsonIII <- function(x, meanlog = 0, sdlog = 1, skew = 0) {
  retval <- dpearsonIII(log(x), meanlog, sdlog, skew)/x
  return(ifelse(x == 0, 0, retval))
}

plpearsonIII <- function(q, meanlog = 0, sdlog = 1, skew = 0) {
  if(skew == 0)
    return(plnorm(q, meanlog, sdlog))
  return(ppearsonIII(log(q), meanlog, sdlog, skew))
}

qlpearsonIII <- function(p, meanlog = 0, sdlog = 1, skew = 0) {
  return(exp(qpearsonIII(p, meanlog, sdlog, skew)))
}

rlpearsonIII <- function(n, meanlog = 0, sdlog = 1, skew = 0) {
  return(exp(rpearsonIII(n, meanlog, sdlog, skew)))
}

