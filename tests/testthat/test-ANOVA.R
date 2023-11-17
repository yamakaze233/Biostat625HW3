library(testthat)
library(MLRANOVA)

test_that("anova_linear_regression works correctly", {
  set.seed(625)
  X <- matrix(rnorm(100), ncol = 5)
  Y <- X %*% c(1, 2, 3, 4, 5) + rnorm(20)
  model <- multivariate_linear_regression(X, Y)
  anova_result <- anova_linear_regression(model, X, Y)

  expect_is(anova_result, "data.frame")
})
