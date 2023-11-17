library(testthat)
library(MLRANOVA)

# Test for matrix multiplication
test_that("Matrix multiplication works correctly", {
  mat1 <- matrix(1:4, nrow = 2)
  mat2 <- matrix(1:4, nrow = 2)
  result <- matrixMultiply(mat1, mat2)
  expect_equal(result, mat1 %*% mat2)
})

# Test for matrix inverse
test_that("Matrix inverse works correctly", {
  mat <- matrix(c(4, 7, 2, 6), nrow = 2)
  inv_result <- matrixInverse(mat)
  expect_equal(inv_result, solve(mat))
})
