
# Linear regression using QR decomposition

<!-- badges: start -->
[![R-CMD-check](https://github.com/HBeijer/linreg/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HBeijer/linreg/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of this package is to estimate a multiple linear regression model using QR decomposition. QR decomposition is used to estimate the coefficients because it presents the numerical rounding errors common in ordinary least squares.

## Installation

The package linreg can be installed from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("HBeijer/linreg")
```

## Example

Here are some examples of how linear regression models can be built and used with the linreg package.

``` r
library(linreg)

## Multiple linear regression model

model <- linreg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)

print(model)
```

