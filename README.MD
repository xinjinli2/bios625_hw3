# linearReg: Linear Regression Analysis Tools

`linearReg` is an R package that provides tools for fitting and summarizing linear regression models using the ordinary least squares (OLS) method. The package is intuitive and practical, making it suitable for both educational and real-world data analysis.

<!-- badges: start -->
[![R-CMD-check](https://github.com/xinjinli2/bios625_hw3/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/xinjinli2/bios625_hw3/actions/workflows/R-CMD-check.yaml)
[![Codecov test coverage](https://codecov.io/gh/xinjinli2/bios625_hw3/graph/badge.svg)](https://app.codecov.io/gh/xinjinli2/bios625_hw3)
<!-- badges: end -->

## Installation

You can install the `linearReg` package from GitHub:

```r
# Install devtools if you haven't already
install.packages("devtools")

# Install linearReg from GitHub
devtools::install_github("xinjinli2/linearReg")
```r

## Quick Start

Here is a simple example using the built-in `iris` dataset:

```r
library(linearReg)

# Fit a linear regression model
model <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, data = iris)

# Display a summary of the model
model_summary(model)

# Get confidence intervals for coefficients
ci <- getConfidenceInterval(model)
print(ci)

# R-squared and adjusted R-squared values
R_squared <- getRSquared(model)
adjusted_R_squared <- getAdjustedRSquared(model)

# F-statistic and p-value
F_statistic <- getFStatistic(model)
```r

## Example Data
The examples above use the `iris` dataset. You can apply these functions to your own data by replacing `iris` with your data frame.

## Help Document
Here is an example that shows how to view help documents
```r
?fitLinearModel
?model_summary
```r

## Author
Cindy Li
Email: xinjinli@umich.edu





