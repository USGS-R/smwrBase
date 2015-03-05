context("Transformation functions")

test_that("Transformations working", {
  testthat::skip_on_cran()
  X3 <- c(1,2,3)
  # Various Box-Cox transforms
  expect_that(attr(boxCox(X3), "GM"), 
              equals(1.8171206))
  expect_that(as.vector(boxCox(X3)), 
              equals(c(0,1,2)))
  expect_that(as.vector(boxCox(X3, lambda=0)), 
              equals(c(0.0, 1.259532, 1.996311)))
  expect_that(as.vector(boxCox(X3, lambda=0.5)), 
              equals(c(0.0, 1.11672486, 1.97361799)))
  expect_that(as.vector(boxCox(X3, lambda=-1)), 
              equals(c(0.0, 1.65096362, 2.20128483)))
  # The inverse function too
  expect_that(as.vector(IboxCox(boxCox(X3))), 
              equals(X3))
  expect_that(as.vector(IboxCox(boxCox(X3, lambda=0), lambda=0)), 
              equals(X3))
  expect_that(as.vector(IboxCox(boxCox(X3, lambda=2), lambda=2)), 
              equals(X3))
  # The hyperbolic transform
  expect_that(attr(hyperbolic(X3), "scale"), 
              equals(2))
  expect_that(as.vector(hyperbolic(X3)), 
              equals(c(1.333333333, 2, 2.4)))
  expect_that(as.vector(hyperbolic(X3, factor=1)), 
              equals(c(3.333333333, 3.63636364, 3.75)))
  expect_that(as.vector(hyperbolic(X3, factor=-1)), 
              equals(c(0.190476190, 0.363636364, 0.521739130)))
  # The inverse function too
  expect_that(as.vector(Ihyperbolic(hyperbolic(X3))), 
              equals(X3))
  expect_that(as.vector(Ihyperbolic(hyperbolic(X3, factor=0.5), factor=0.5)), 
              equals(X3))
  expect_that(as.vector(Ihyperbolic(hyperbolic(X3, factor=-.5), factor=-.5)), 
              equals(X3))
  # Scale range
  expect_that(attributes(scaleRng(X3)),
              equals(list(Min=0, Max=1, x.range=c(1,3), class="scaleRng")))
  expect_that(as.vector(scaleRng(X3, Min=-1, Max=1)),
              equals(c(-1, 0, 1)))
  # And inverse function
  expect_that(IscaleRng(scaleRng(X3, Min=-1, Max=1)),
              equals(X3))
  # The S-curve
  expect_that(sCurve(X3),
              equals(c(0.5, 0.6666666666667, 0.75)))
  expect_that(sCurve(X3, location=2),
              equals(c(-.5, 0., 0.5)))
  expect_that(sCurve(X3, location=2, scale=3),
              equals(c(-.75, 0., 0.75)))
  # And inverse function
  expect_that(IsCurve(sCurve(X3)),
              equals(X3))
  expect_that(IsCurve(sCurve(X3, location=2), location=2),
              equals(X3))
  expect_that(IsCurve(sCurve(X3, location=1, scale=2), location=1, scale=2),
              equals(X3))
  # Quadratic here too
  expect_that(attr(quadratic(X3), "xstar"),
              equals(2))
  expect_that(quadratic(X3)[,1L],
              equals(c(-1, 0, 1)))
  expect_that(quadratic(X3)[,2L],
              equals(c(1, 0, 1)))
})

test_that("Date/Time transforms working", {
  testthat::skip_on_cran()
  XD <- as.Date(c("2001-01-15", "2002-02-23", "2003-04-12", "2004-07-02"))
  # Base day
  expect_that(baseDay(XD),
              equals(c(15L, 54L, 103L, 184L)))
  expect_that(as.character(baseDay(XD, numeric=FALSE)),
              equals(c("Jan 15", "Feb 23", "Apr 12", "Jul 02")))
  expect_that(baseDay2decimal(baseDay(XD, numeric=FALSE, year="water")),
              equals(c(0.0382513661, 0.1448087432, 0.2786885246, 0.5)))
  # Decimal time
  expect_that(dectime(XD),
              equals(c(2001.03973, 2002.14658, 2003.27808, 2004.50137)))
  expect_that(dectime2Date(dectime(XD)),
              equals(XD, tolerance=1e-6)) # These are expected to be only close
  # Fourier
  expect_that(fourier(XD),
              equals(fourier(dectime(XD))))
  expect_that(fourier(XD, 2)[, 1L],
              equals(sin(dectime(XD)*2*pi)))
  expect_that(fourier(XD, 2)[, 3L],
              equals(sin(dectime(XD)*4*pi)))
  expect_that(fourier(XD, 2)[, 2L],
              equals(cos(dectime(XD)*2*pi)))
  expect_that(fourier(XD, 2)[, 4L],
              equals(cos(dectime(XD)*4*pi)))
})
