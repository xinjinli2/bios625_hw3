library(testthat)
library(linearReg)
data("iris")

context("Linear Model Tests on Iris Dataset")

# Fit both models for comparison
std_mod <- lm(Petal.Length ~ Petal.Width + Sepal.Width, iris)
custom_mod <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Width, iris)


test_that("fitLinearModel returns valid model output", {
  expect_true(is.list(custom_mod))
  expect_true("beta_hat" %in% names(custom_mod))

  expect_true(is.numeric(custom_mod$beta_hat))
  expect_length(custom_mod$beta_hat, 3)
})

test_that("coefficients are equal", {
  actual_coefs <- as.vector(custom_mod$beta_hat)
  names(actual_coefs) <- rownames(custom_mod$beta_hat)
  expected_coefs <- coef(std_mod)
  expect_equal(actual_coefs, expected_coefs)
})

test_that("confidence intervals are equal", {
  ci_custom <- getConfidenceInterval(custom_mod)
  ci_standard <- confint(std_mod)
  colnames(ci_standard) <- c("Lower", "Upper")

  ci_custom_matrix <- as.matrix(ci_custom)
  expect_true(all.equal(ci_custom_matrix, ci_standard))
})

test_that("R-squared values are equal", {
  R_squared_custom <- getRSquared(custom_mod)
  R_squared_standard <- summary(std_mod)$r.squared

  expect_equal(R_squared_custom, R_squared_standard)
})

# test_that("Adjusted R-squared values are equal", {
#   adj_R_squared_custom <- getAdjustedRSquared(custom_mod)
#   adj_R_squared_standard <- summary(std_mod)$adj.r.squared
#
#   expect_equal(adj_R_squared_custom, adj_R_squared_standard, tolerance = 1e-4)
# })

test_that("F-statistics are equal", {
  F_stat_custom <- getFStatistic(custom_mod)[[1]]
  F_stat_standard <- summary(std_mod)$fstatistic[[1]]
  expect_equal(F_stat_custom, F_stat_standard, tolerance = 1e-4)
})

test_that("P-values for F-statistics are equal", {
  p_value_custom <- getFStatistic(custom_mod)[[2]]
  p_value_standard <- pf(summary(std_mod)$fstatistic[1],
                         summary(std_mod)$fstatistic[2],
                         summary(std_mod)$fstatistic[3],
                         lower.tail = FALSE)[[1]]

  expect_equal(p_value_custom, p_value_standard, tolerance = 1e-4)
})

test_that("Model accuracy is reasonable", {
  R_squared <- getRSquared(custom_mod)
  expect_true(R_squared > 0.5)
})
