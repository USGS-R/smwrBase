#'Pearson Type III distribution
#'
#'Density, cumulative probability, quantiles, and random generation for the
#'Pearson Type III distribution.
#'
#'Elements of \code{x}, \code{q}, or \code{p} that are missing will result in 
#'missing values in the returned data.
#'
#' @rdname PearsonIII
#' @aliases PearsonIII dpearsonIII ppearsonIII qpearsonIII rpearsonIII
#' @param x,q vector of quantiles. Missing values are permitted and result in
#'corresponding missing values in the output.
#' @param p vector of probabilities.
#' @param n number of observations. If length(\code{n}) > 1, then the length is taken
#'to be the number required.
#' @param mean vector of means of the distribution of the data.
#' @param sd vector of standard deviation of the distribution of the data.
#' @param skew vector of skewness of the distribution of the data.
#' @return Either the density (\code{dpearsonIII}), cumulative probability
#'(\code{ppearsonIII}), quantile (\code{qpearsonIII}), or random sample
#'(\code{rpearsonIII}) for the described distribution.
#' @export
#' @note The log-Pearson Type III distribution is used extensitvely in flood-frequency 
#'analysis in the United States. The Pearson Type III forms the basis
#'for that distribution.
#' @seealso \code{\link{dlpearsonIII}}, \code{\link{dnorm}}
#' @keywords manip distribution
#' @examples
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
  ##    2014Jan10 DLLorenz Vectorized for skew
  ##
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the dgamma function or the dnorm function to return the
  ## quantiles desired.
  ##
  ## Replicate all values to ensure consistent results
  Nout <- max(length(x), length(mean), length(sd), length(skew))
  x <- rep(x, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- dnorm(x, mean, sd)
  }
  if(all(skeworg == 0))
    return(ret0)
  shape <- 4/skew^2
  rate <- sqrt(shape/sd^2)
  mn <- shape/rate
  x <- ifelse(skew > 0, x + mn - mean, mn - x + mean)
  rets <- dgamma(x, shape, rate)
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) + 
      rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets)
}

#' @rdname PearsonIII
#' @export
#' @examples
#'## Simple examples
#'ppearsonIII(c(.5, .75, .9), 1.5, .25, 0)
#'## compare to normal
#'qnorm(c(.5, .75, .9), 1.5, .25)
#'## Make a skewed distribution
#'ppearsonIII(c(.5, .75, .9), 1.5, .25, 0.25)
ppearsonIII <- function(q, mean = 0, sd = 1, skew = 0) {
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the pgamma function or the pnorm function to return the
  ## quantiles desired.
  Nout <- max(length(q), length(mean), length(sd), length(skew))
  q <- rep(q, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- pnorm(q, mean, sd)
  }
  shape <- 4/skew^2
  rate <- sqrt(shape/sd^2)
  mn <- shape/rate
  q <- ifelse(skew > 0, q + mn - mean, mn + mean - q)
  rets <- pgamma(q, shape, rate)
  ## Adjust for negative skew
  rets <- ifelse(skew > 0, rets, 1-rets)
  ## Adjust for near 0 skew
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) + 
                       rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets)
}

#' @rdname PearsonIII
#' @export
#' @examples
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
  Nout <- max(length(p), length(mean), length(sd), length(skew))
  p <- rep(p, length.out=Nout); mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- qnorm(p)
  }
  shape <- 4/skew^2
  rets <- ifelse(skew > 0, (qgamma(p, shape) - shape)/sqrt(shape),
                 (shape - qgamma(1 - p, shape))/sqrt(shape))
  if(any(ckskew)) {
    rets[ckskew] <- (ret0[ckskew]*((1e-6)-abs(skeworg[ckskew])) + 
                       rets[ckskew]*abs(skeworg[ckskew]))/1e-6
  }
  return(rets * sd + mean)
}

#' @rdname PearsonIII
#' @export
#' @examples
#' # Simple examples
#'rpearsonIII(c(.5, .75, .9), 1.5, .25, 0)
rpearsonIII <- function(n, mean = 0, sd = 1, skew = 0) {
  ## the Pearson Type III distribution is simply a generalized gamma distribution
  ## therefore, use the qgamma function or the qnorm function to return the
  ## quantiles desired.
  Nout <- if(length(n) == 1) n else length(n)
  mean <- rep(mean, length.out=Nout)
  sd <- rep(sd, length.out=Nout); skew <- rep(skew, length.out=Nout)
  skeworg <- skew
  ckskew <- abs(skew) < 1e-6
  if(any(ckskew)) {
    skew[ckskew] = sign(skew[ckskew]) * 1e-6
    skew[skew == 0] <- 1e-6 # Catch these
    ret0 <- rnorm(n)
  }
  shape <- 4/skew^2
  rets <- (rgamma(n, shape) - shape)/sqrt(shape)
  rets[skew < 0] <- -rets[skew < 0]
  if(any(ckskew))
    rets[ckskew] <- ret0[ckskew]
  return(rets * sd + mean)
}
