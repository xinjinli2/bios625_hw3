## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup--------------------------------------------------------------------
# install `linearReg` package if you haven't installed yet
# install.packages("linearReg")
library(linearReg)

## -----------------------------------------------------------------------------
# Define the formula
formula = Petal.Length ~ Petal.Width + Sepal.Width

## -----------------------------------------------------------------------------
my_mod <- fitLinearModel(formula, iris)

## -----------------------------------------------------------------------------
my_mod$coefficients

## -----------------------------------------------------------------------------
my_mod$SE_beta_hat

## -----------------------------------------------------------------------------
my_mod$MSE

## -----------------------------------------------------------------------------
my_mod$R_squared

## -----------------------------------------------------------------------------
my_mod$adjusted_R_squared

## -----------------------------------------------------------------------------
my_mod$F_statistic

## -----------------------------------------------------------------------------
model_summary(my_mod)

## -----------------------------------------------------------------------------
getConfidenceInterval(my_mod)

## -----------------------------------------------------------------------------
getRSquared(my_mod)

## -----------------------------------------------------------------------------
getAdjustedRSquared(my_mod)

## -----------------------------------------------------------------------------
getFStatistic(my_mod)

## -----------------------------------------------------------------------------
# Fit models using both lm() and your custom function
lm_model <- lm(Petal.Length ~ Petal.Width + Sepal.Length, data = iris)
custom_model <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, data = iris)

# Compare coefficients
all.equal(coef(lm_model), custom_model$coefficients)

# Compare standard errors
all.equal(summary(lm_model)$coefficients[, "Std. Error"], custom_model$SE_beta_hat)

# Compare R-squared values
all.equal(summary(lm_model)$r.squared, custom_model$R_squared)

# Compare adjusted R-squared values
all.equal(summary(lm_model)$adj.r.squared, custom_model$adjusted_R_squared)


## -----------------------------------------------------------------------------
# Load necessary package
library(bench)

# using bench::mark to compare the efficiency of customized and standard linear regression model fittings. 
benchmark_model_fitting<- bench::mark(
  my_mod_coef = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Width, iris)$coefficients,
  lm_model_coef = coef(lm(Petal.Length ~ Petal.Width + Sepal.Width, data = iris)),
  iterations = 100
)

print(benchmark_model_fitting)

## -----------------------------------------------------------------------------
# benchmark_ci <- bench::mark(
#   my_mod_ci = as.numeric(unlist(getConfidenceInterval(my_mod))),
#   lm_model_ci = as.vector(confint(lm_model)),
#   iterations = 100
# )
# 
# print(benchmark_ci)
benchmark_ci <- bench::mark(
  my_mod_ci = { getConfidenceInterval(my_mod); NULL }, 
  lm_model_ci = { confint(lm_model); NULL },            
)

# Print the benchmark results
print(benchmark_ci)

## -----------------------------------------------------------------------------
benchmark_r_squared <- bench::mark(
  my_mod_r2 = { my_mod$R_squared; NULL },
  lm_model_r2 = { summary(lm_model)$r.squared; NULL },
  iterations = 100
)

print(benchmark_r_squared)

## -----------------------------------------------------------------------------
benchmark_adjusted_r_squared <- bench::mark(
  my_mod_adj_r2 = { getAdjustedRSquared(my_mod); NULL },
  lm_model_adj_r2 = { summary(lm_model)$adj.r.squared; NULL },
  iterations = 100
)

print(benchmark_adjusted_r_squared)

## -----------------------------------------------------------------------------
# Benchmark F-statistics calculation
benchmark_f_stat <- bench::mark(
  my_mod_f_stat = { getFStatistic(my_mod); NULL },
  lm_model_f_stat = { as.numeric(summary(lm_model)$fstatistic[1]); NULL },
  iterations = 100
)

print(benchmark_f_stat)

