library(testthat)
library(linearReg)
data("iris")

context("Linear Model Tests on Iris Dataset")

test_that("fitLinearModel returns valid model output", {
  result <- fitLinearModel(iris$Petal.Width, iris$Petal.Length)

  expect_true(is.list(result))
  expect_true("coefficients" %in% names(result))
  expect_true("residuals" %in% names(result))

  expect_true(is.numeric(result$coefficients))
  expect_length(result$coefficients, 2)
})

test_that("fitLinearModel handles non-numeric input gracefully", {
  expect_error(fitLinearModel(iris$Species, iris$Petal.Length))
})

test_that("Model accuracy is reasonable", {
  model <- fitLinearModel(iris$Petal.Width, iris$Petal.Length)

  R_squared <- summary(model)$r.squared
  expect_true(R_squared > 0.5)
})

