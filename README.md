# MLRANOVA

The 'MLRANOVA' package provides functions for performing multivariate linear regression and analysis of variance (ANOVA).

## Installation

To install the latest release version from CRAN, use:

```{r}
install.packages("MLRANOVA")
```

To install the development version directly from GitHub, use:

```{r}
# install.packages("devtools")
devtools::install_github("yamakaze233/Biostat625HW3/MLRANOVA")
```

## Example
Here is a basic example of how MLRANOVA can be used:

```{r}
library(MLRANOVA)

# Example of using multivariate linear regression
X <- matrix(rnorm(500), ncol = 5)
Y <- X %*% c(1, 2, 3, 4, 5) + rnorm(100)
model <- multivariate_linear_regression(X, Y)

# Example of using ANOVA on the linear regression model
anova_result <- anova_linear_regression(model, X, Y)
```
## Features
Perform multivariate linear regression analysis.
Conduct ANOVA tests on linear regression models.
Utilize efficient C++ implementations for core calculations.

## Contributing
Contributions to MLRANOVA are welcome! Feel free to open an issue or submit a pull request on [GitHub](https://github.com/yamakaze233/Biostat625HW3).

## License
This package is released under the MIT License. See the LICENSE file for more details.

## Getting Help
For questions and support, please open an issue in the [GitHub issue tracker](https://github.com/yamakaze233/Biostat625HW3/issues).


# Biostat625HW3
