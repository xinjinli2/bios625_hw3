library(testthat)
library(linearReg)
data("iris")

context("Linear Model Tests on Iris Dataset")

test_that("fitLinearModel returns valid model output", {
  result <- fitLinearModel(Petal.Length ~ Petal.Width, iris)

  expect_true(is.list(result))
  expect_true("beta_hat" %in% names(result))
  expect_true("residuals" %in% names(result))

  expect_true(is.numeric(result$beta_hat))
  expect_length(result$beta_hat, 2)
})

test_that("fitLinearModel handles non-numeric input correctly", {
  expect_error(fitLinearModel(Petal.Length ~ Species, iris))
})

test_that("Model accuracy is reasonable", {
  model <- fitLinearModel(Petal.Length ~ Petal.Width, iris)

  R_squared <- model$R_squared
  expect_true(R_squared > 0.5)
})
