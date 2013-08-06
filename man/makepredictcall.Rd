\name{makepredictcall}
\alias{makepredictcall}
\alias{makepredictcall.boxCox}
\alias{makepredictcall.hyperbolic}
\alias{makepredictcall.quadratic}
\title{Utility Function for Safe Prediction}
\usage{
  makepredictcall(var, call)

  \method{makepredictcall}{quadratic} (var, call)

  \method{makepredictcall}{hyperbolic} (var, call)

  \method{makepredictcall}{boxCox} (var, call)
}
\arguments{
  \item{var}{a variable.}

  \item{call}{the term in the formula, as a call.}
}
\value{
  A replacement for \code{call} for the prediction
  variable.
}
\description{
  A utility to help \code{\link{model.frame.default}}
  create the right matrices when predicting from models
  with \code{quadratic}, \code{hyperbolic}, or
  \code{boxCox} terms. Used only internally.
}

