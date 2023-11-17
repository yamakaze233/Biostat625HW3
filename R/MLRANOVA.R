#' @useDynLib MLRANOVA
#' @importFrom Rcpp sourceCpp
#' @importFrom stats pf pt
#' @importFrom bench mark
NULL

#' Multivariate Linear Regression
#'
#' This function performs multivariate linear regression to predict the response variable given a set of predictors using the least squares method.
#' @param X A numeric matrix of predictor variables.
#' @param Y A numeric vector of the response variable.
#' @return A list containing the regression coefficients, the residuals, the residual standard error, and the multiple R-squared statistic.
#' @export

multivariate_linear_regression <- function(X, Y) {
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  X <- cbind(1, X) # Adding intercept

  # Coefficients
  XtX_inv <- matrixInverse(matrixMultiply(t(X), X))
  coefficients <- matrixMultiply(XtX_inv, t(X)) %*% Y

  # Predictions and Residuals
  predictions <- matrixMultiply(X, coefficients)
  residuals <- Y - predictions

  # Variance and Standard Errors
  n <- nrow(X)
  p <- ncol(X)
  degrees_of_freedom <- n - p
  sigma_squared <- sum(residuals^2) / degrees_of_freedom
  var_beta <- sigma_squared * solve(t(X) %*% X)
  std_errors <- sqrt(diag(var_beta))

  # T-statistics and P-values
  t_values <- coefficients / std_errors
  p_values <- 2 * pt(-abs(t_values), df = degrees_of_freedom)

  # R-squared
  SSR <- sum((predictions - mean(Y))^2)
  SST <- sum((Y - mean(Y))^2)
  r_squared <- SSR / SST

  return(list(coefficients = coefficients,
              std_errors = std_errors,
              t_values = t_values,
              p_values = p_values,
              df = degrees_of_freedom,
              r_squared = r_squared))
}

#' ANOVA for Linear Regression Models
#'
#' This function performs an Analysis of Variance (ANOVA) for a fitted linear regression model.
#' It computes the sum of squares due to regression (SSR), sum of squares error (SSE),
#' total sum of squares (SST), degrees of freedom, mean squares, F-statistic, and the p-value for the F-statistic.
#'
#' @param model A list containing the components of a fitted linear regression model.
#' This should include the model coefficients, the matrix of predictors (X), and the vector of responses (Y).
#' @param X A numeric matrix of predictor variables.
#' @param Y A numeric vector of the response variable used in the linear regression model.
#' @return A data frame with the ANOVA table, including the source of variation, sum of squares, degrees of freedom, mean squares, F value, and the associated p-value.
#' @export
#' @examples
#' X <- matrix(rnorm(100), ncol = 5)
#' Y <- X %*% runif(5) + rnorm(20)
#' fit <- multivariate_linear_regression(X, Y)
#' anova_table <- anova_linear_regression(fit, X, Y)
#' print(anova_table)

anova_linear_regression <- function(model, X, Y) {
  # Extracting necessary components from the model
  coefficients <- model$coefficients
  if (!is.matrix(X)) {
    X <- as.matrix(X)
  }
  if (ncol(X) == length(coefficients) - 1) {
    X <- cbind(1, X)  # Add column for intercept
  }
  residuals <- Y - X %*% coefficients

  # Sum of Squares
  SST <- sum((Y - mean(Y))^2)
  SSR <- sum((X %*% coefficients - mean(Y))^2)
  SSE <- sum(residuals^2)

  # Degrees of Freedom
  df_total <- length(Y) - 1
  df_model <- length(coefficients) - 1
  df_residual <- df_total - df_model

  # Mean Squares
  MSR <- SSR / df_model
  MSE <- SSE / df_residual

  # F-Statistic
  F_statistic <- MSR / MSE

  # P-value
  p_value <- pf(F_statistic, df_model, df_residual, lower.tail = FALSE)

  # Create a data frame for the ANOVA table
  anova_table <- data.frame(
    Source = c("Model", "Residuals", "Total"),
    'Df' = c(df_model, df_residual, df_total),
    'Sum Sq' = c(SSR, SSE, SST),
    'Mean Sq' = c(MSR, MSE, NA),
    'F value' = c(F_statistic, NA, NA),
    'Pr(>F)' = c(p_value, NA, NA)
  )

  return(anova_table)
}
