library(testthat)
library(linearReg)
data("iris")

context("Linear Model Tests on Iris Dataset")

# Fit both models for comparison
std_mod <- lm(Petal.Length ~ Petal.Width + Sepal.Width, iris)
my_mod <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Width, iris)
my_mod_summary <- model_summary(my_mod)

test_that("fitLinearModel returns valid model output", {
  expect_true(is.list(my_mod))
  expect_true("coefficients" %in% names(my_mod))
  expect_true("SE_beta_hat" %in% names(my_mod))
  expect_true("t_values" %in% names(my_mod))
  expect_true("p_values" %in% names(my_mod))
  expect_true("fitted.values" %in% names(my_mod))
  expect_true("residuals" %in% names(my_mod))
  expect_true("MSE" %in% names(my_mod))
  expect_true("R_squared" %in% names(my_mod))
  expect_true("adjusted_R_squared" %in% names(my_mod))
  expect_true("F_statistic" %in% names(my_mod))
  expect_true("df1" %in% names(my_mod))
  expect_true("df2" %in% names(my_mod))

  expect_true(is.numeric(my_mod$coefficients))
  expect_length(my_mod$coefficients, 3)
})

test_that("model_summary returns valid model output", {
  expect_true(is.list(my_mod_summary))
  expect_true("Estimate" %in% names(my_mod_summary))
  expect_true("Std.Error" %in% names(my_mod_summary))
  expect_true("t.values" %in% names(my_mod_summary))
  expect_true("P.values" %in% names(my_mod_summary))
  expect_true("Signif" %in% names(my_mod_summary))
})


test_that("fitLinearModel handles only one predictor", {
  one_x_model <- fitLinearModel(Petal.Length ~ Petal.Width, iris)
  expect_length(one_x_model$coefficients, 2) # only one coef for petal.width and one for intercept
})


test_that("fitLinearModel handles missing response variable", {
  expect_error(
    fitLinearModel(random ~ Petal.Width, iris),
    "Undefined variables in the model formula"
  )
})


test_that("fitLinearModel residuals are computed correctly", {
  expect_equal(sum(my_mod$residuals), 0, tolerance = 1e-10)  # Residuals should sum to zero
})


test_that("coefficients are equal", {
  actual_coefs <- as.vector(my_mod$coefficients)
  names(actual_coefs) <- names(coef(std_mod))
  expected_coefs <- coef(std_mod)
  expect_equal(actual_coefs, expected_coefs)
})


test_that("confidence intervals are equal", {
  ci_custom <- getConfidenceInterval(my_mod)
  colnames(ci_custom) <- c("Lower", "Upper")
  ci_standard <- confint(std_mod)
  colnames(ci_standard) <- c("Lower", "Upper")

  ci_custom_matrix <- as.matrix(ci_custom)
  expect_true(all.equal(ci_custom_matrix, ci_standard))
})

test_that("R-squared values are equal", {
  R_squared_custom <- getRSquared(my_mod)
  R_squared_standard <- summary(std_mod)$r.squared

  expect_equal(R_squared_custom, R_squared_standard)
})

test_that("Adjusted R-squared values are equal", {
  adj_R_squared_custom <- getAdjustedRSquared(my_mod)
  adj_R_squared_standard <- summary(std_mod)$adj.r.squared

  expect_equal(adj_R_squared_custom, adj_R_squared_standard, tolerance = 1e-2)
})

test_that("F-statistics are equal", {
  F_stat_custom <- getFStatistic(my_mod)
  F_stat_standard <- summary(std_mod)$fstatistic[[1]]
  expect_equal(F_stat_custom, F_stat_standard, tolerance = 1e-4)
})

test_that("Model accuracy is reasonable", {
  R_squared <- getRSquared(my_mod)
  expect_true(R_squared > 0.5)
  adjusted_R_squared <- getAdjustedRSquared(my_mod)
  expect_true(adjusted_R_squared > 0.5)
})
