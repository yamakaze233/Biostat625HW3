library(testthat)
library(MLRANOVA)

test_that("multivariate_linear_regression works correctly", {
  set.seed(625)
  X <- matrix(rnorm(100), ncol = 5)
  Y <- X %*% c(1, 2, 3, 4, 5) + rnorm(20)
  model <- multivariate_linear_regression(X, Y)

  expect_is(model, "list")
  expect_true("coefficients" %in% names(model))
  # Add more checks as appropriate
})
