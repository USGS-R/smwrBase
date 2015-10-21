#'Log-Pearson Type III distribution
#'
#'Density, cumulative probability, quantiles, and random generation for the
#'log-Pearson Type III distribution.
#'
#'Elements of x, q, or p that are missing will result in missing values in the
#'returned data.
#'
#' @rdname LogPearsonIII
#' @aliases LogPearsonIII dlpearsonIII plpearsonIII qlpearsonIII rlpearsonIII
#' @param x,q vector of quantiles.
#' @param p vector of probabilities.
#' @param n number of observations. If length(\code{n}) > 1, then the length is taken
#'to be the number required.
#' @param meanlog vector of means of the distribution of the log-transformed
#'data.
#' @param sdlog vector of standard deviation of the distribution of the
#'log-transformed data.
#' @param skew vector of skewness of the distribution of the log-transformed
#'data.
#' @return Either the density (\code{dlpearsonIII}), cumulative probability
#'(\code{plpearsonIII}), quantile (\code{qlpearsonIII}), or random sample
#'(\code{rlpearsonIII}) for the described distribution.
#' @note The log-Pearson Type III distribution is used extensively in 
#'flood-frequency analysis in the United States.
#' @export
#' @seealso 
#Flip for production/manual
#'\code{\link{dpearsonIII}}, \code{\link[stats]{dlnorm}}
#\code{\link{dpearsonIII}}, \code{dlnorm} (in stats package)
#' @keywords manip distribution
#' @examples
#'
#'## Simple examples
#'dlpearsonIII(c(.5, .75, .9), 1.5, .25, 0)
#'## compare to normal
#'qlnorm(c(.5, .75, .9), 1.5, .25)
#'## Make a skewed distribution
#'dlpearsonIII(c(.5, .75, .9), 1.5, .25, 0.25)
dlpearsonIII <- function(x, meanlog = 0, sdlog = 1, skew = 0) {
  retval <- dpearsonIII(log(x), meanlog, sdlog, skew)/x
  return(ifelse(x == 0, 0, retval))
}

#' @rdname LogPearsonIII
#' @export
#' @examples
#'
#'## Simple examples
#'plpearsonIII(c(.5, .75, .9), 1.5, .25, 0)
#'## compare to normal
#'qlnorm(c(.5, .75, .9), 1.5, .25)
#'## Make a skewed distribution
#'plpearsonIII(c(.5, .75, .9), 1.5, .25, 0.25)
plpearsonIII <- function(q, meanlog = 0, sdlog = 1, skew = 0) {
  return(ppearsonIII(log(q), meanlog, sdlog, skew))
}

#' @rdname LogPearsonIII
#' @export
#' @examples
#'
#'## Simple examples
#'qlpearsonIII(c(.5, .75, .9), 1.5, .25, 0)
#'## compare to normal
#'qlnorm(c(.5, .75, .9), 1.5, .25)
#'## Make a skewed distribution
#'qlpearsonIII(c(.5, .75, .9), 1.5, .25, 0.25)
qlpearsonIII <- function(p, meanlog = 0, sdlog = 1, skew = 0) {
  return(exp(qpearsonIII(p, meanlog, sdlog, skew)))
}

#' @rdname LogPearsonIII
#' @export
#' @examples
#'
#'## Simple examples
#'rlpearsonIII(c(.5, .75, .9), 1.5, .25, 0)
#'## compare to normal
#'qlnorm(c(.5, .75, .9), 1.5, .25)
#'## Make a skewed distribution
#'rlpearsonIII(c(.5, .75, .9), 1.5, .25, 0.25)
rlpearsonIII <- function(n, meanlog = 0, sdlog = 1, skew = 0) {
  return(exp(rpearsonIII(n, meanlog, sdlog, skew)))
}

