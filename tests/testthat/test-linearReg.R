library(testthat)
library(linearReg)
data("iris")

context("Linear Model Tests on Iris Dataset")

# Fit both models for comparison
std_mod <- lm(Petal.Length ~ Petal.Width + Sepal.Width, iris)
custom_mod <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Width, iris)
custom_mod_summary <- model_summary(custom_mod)

test_that("fitLinearModel returns valid model output", {
  expect_true(is.list(custom_mod))
  expect_true("beta_hat" %in% names(custom_mod))
  expect_true("se_beta_hat" %in% names(custom_mod))
  expect_true("t_stats" %in% names(custom_mod))
  expect_true("p_values" %in% names(custom_mod))
  expect_true("sigma_squared" %in% names(custom_mod))
  expect_true("R_squared" %in% names(custom_mod))
  expect_true("adjusted_R_squared" %in% names(custom_mod))
  expect_true("F_statistic" %in% names(custom_mod))
  expect_true("p_value_F" %in% names(custom_mod))
  expect_true("n" %in% names(custom_mod))
  expect_true("k" %in% names(custom_mod))
  expect_true("X" %in% names(custom_mod))

  expect_true(is.numeric(custom_mod$beta_hat))
  expect_length(custom_mod$beta_hat, 3)
})

test_that("model_summary returns valid model output", {
  expect_true(is.list(custom_mod_summary))
  expect_true("Estimate" %in% names(custom_mod_summary))
  expect_true("Std.Error" %in% names(custom_mod_summary))
  expect_true("t.value" %in% names(custom_mod_summary))
  expect_true("P.values" %in% names(custom_mod_summary))
  expect_true("Signif" %in% names(custom_mod_summary))
})


test_that("fitLinearModel handles only one predictor", {
  single_predictor_model <- fitLinearModel(Petal.Length ~ Petal.Width, iris)
  expect_length(single_predictor_model$beta_hat, 2)
})

test_that("fitLinearModel handles missing response variable", {
  expect_error(fitLinearModel(NonExistent ~ Petal.Width, iris), "Not all variables specified")
})

test_that("fitLinearModel residuals are computed correctly", {
  expect_equal(sum(custom_mod$residuals), 0, tolerance = 1e-10)  # Residuals should sum to zero
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

test_that("Adjusted R-squared values are equal", {
  adj_R_squared_custom <- getAdjustedRSquared(custom_mod)
  adj_R_squared_standard <- summary(std_mod)$adj.r.squared

  expect_equal(adj_R_squared_custom, adj_R_squared_standard, tolerance = 1e-2)
})

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
  adjusted_R_squared <- getAdjustedRSquared(custom_mod)
  expect_true(adjusted_R_squared > 0.5)
})
