#' Linear Regression Function
#'
#' Performs linear regression on the provided dataset using the ordinary least squares method.
#' The function calculates coefficients, residuals, various statistics including R-squared,
#' adjusted R-squared, and F-statistics, and returns a comprehensive summary of the model.
#'
#' @param formula An object of class "formula" (or one that can be coerced to that class):
#'   a symbolic description of the model to be fitted.
#' @param data A data.frame that contains the variables in the model.
#'
#' @return A list containing model coefficients, standard errors, t-statistics, p-values,
#' sigma squared, R-squared, adjusted R-squared, F-statistic, p-value of the F-statistic,
#' number of observations, and number of predictors.
#'
#' @importFrom stats model.matrix
#' @importFrom base solve
#'
#' @examples
#' data(iris)
#' model <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' print(model)
#'
#' @export
fitLinearModel <- function(formula, data) {

  terms <- all.vars(formula)

  # Check if all variables in formula are presented in the data
  if (!all(terms %in% names(data))) {
    stop("Not all variables specified in the formula are present in the dataset.")
  }

  predictors <- terms[-1]
  for (var in predictors) {
    if (!is.numeric(data[[var]])) {
      stop("Non-numeric data found for predictor '", var, "'. Numeric expected.")
    }
  }

  X <- model.matrix(formula, data)  # Create model matrix for predictors
  y <- data[[as.character(formula[[2]])]]  # Extract response variable based on formula

  if (nrow(X) != length(y)) {
    stop("Number of rows in X and length of y must be the same")
  }

  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  y_hat <- X %*% beta_hat
  residuals <- y - y_hat
  RSS <- sum(residuals^2)
  TSS <- sum((y - mean(y))^2)
  ESS <- TSS - RSS

  R_squared <- ESS / TSS
  adjusted_R_squared <- 1 - (1 - R_squared) * ((nrow(X) - 1) / (nrow(X) - ncol(X) - 1))

  sigma_squared <- RSS / (nrow(X) - ncol(X))
  var_beta_hat <- sigma_squared * solve(t(X) %*% X)
  se_beta_hat <- sqrt(diag(var_beta_hat))

  t_stats <- beta_hat / se_beta_hat
  p_values <- 2 * pt(-abs(t_stats), df = nrow(X) - ncol(X))

  F_statistic <- (ESS / (ncol(X) - 1)) / (RSS / (nrow(X) - ncol(X)))
  p_value_F <- pf(F_statistic, df1 = ncol(X) - 1, df2 = nrow(X) - ncol(X), lower.tail = FALSE)

  return(list(
    beta_hat = beta_hat,
    se_beta_hat = se_beta_hat,
    t_stats = t_stats,
    p_values = p_values,
    sigma_squared = sigma_squared,
    R_squared = R_squared,
    adjusted_R_squared = adjusted_R_squared,
    F_statistic = F_statistic,
    p_value_F = p_value_F,
    n = nrow(X),
    k = ncol(X),
    X = X
  ))
}


#' Model Summary
#'
#' Displays a summary of the linear regression model including estimates of the coefficients,
#' standard errors, t-values, p-values for coefficients, significance codes, residual standard error,
#' R-squared, adjusted R-squared, and F-statistic.
#'
#' @param model The model list object returned by `fitLinearModel()`.
#'
#' @return Prints the summary table
#'
#' @examples
#' data(iris)
#' model <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' model_summary(model)
#'
#' @export
model_summary <- function(model) {

  beta_hat <- model$beta_hat
  se_beta_hat <- model$se_beta_hat
  t_stats <- model$t_stats
  p_values <- model$p_values
  sigma_squared <- model$sigma_squared
  R_squared <- model$R_squared
  adjusted_R_squared <- model$adjusted_R_squared
  F_statistic <- model$F_statistic
  p_value_F <- model$p_value_F
  n <- model$n
  k <- model$k


  signif_codes <- ifelse(
    p_values < 0.001, "***",
    ifelse(p_values < 0.01, "**",
           ifelse(p_values < 0.05, "*",
                  ifelse(p_values < 0.1, ".", " ")))
  )


  model_summary <- data.frame(
    Estimate = beta_hat,
    Std.Error = se_beta_hat,
    t.value = t_stats,
    Pr...t.. = p_values,
    Signif = signif_codes
  )
  # rownames(model_summary) <- c("(Intercept)", paste0("X", 1:(k - 1)))
  rownames(model_summary) <- colnames(model$X)


  print(model_summary)


  cat("\nResidual standard error:", sqrt(sigma_squared), "on", n - k, "degrees of freedom\n")
  cat("Multiple R-squared:", R_squared, ", Adjusted R-squared:", adjusted_R_squared, "\n")
  cat("F-statistic:", F_statistic, "on", k - 1, "and", n - k, "DF, p-value:", format.pval(p_value_F), "\n")
  cat("Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1\n\n")
}

#' Confidence Interval for Model Coefficients
#'
#' Calculates the confidence intervals for the regression coefficients at the specified confidence level.
#'
#' @param model The model list object returned by `fitLinearModel`.
#' @param level The confidence level for the interval (default is 0.95).
#'
#' @return Returns a data frame with estimates of the coefficients and their lower and upper bounds.
#'
#' @examples
#' data(iris)
#' model <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' ci <- getConfidenceInterval(model)
#' print(ci)
#'
#' @export
getConfidenceInterval <- function(model, level = 0.95) {

  beta_hat <- model$beta_hat
  se_beta_hat <- model$se_beta_hat
  n <- model$n
  k <- model$k


  alpha <- 1 - level
  t_crit <- qt(1 - alpha / 2, df = n - k)


  lower_bound <- beta_hat - t_crit * se_beta_hat
  upper_bound <- beta_hat + t_crit * se_beta_hat


  ci_summary <- data.frame(
    Lower = lower_bound,
    Upper = upper_bound
  )
  # rownames(ci_summary) <- c("(Intercept)", paste0("X", 1:(k - 1)))
  rownames(ci_summary) <- colnames(model$X)

  return(ci_summary)
}

#' Return the R Squared value for the model
#'
#' @param model The model list object returned by `fitLinearModel`.
#'
#' @return Returns R Squared for model
#'
#' @examples
#' data(iris)
#' model <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' R_squared <- getRSquared(model)
#' print(R_squared)
#'
#' @export
getRSquared <- function(model) {
  return(model$R_squared)
}

#' Return the adjusted R Squared value for the model
#'
#' @param model The model list object returned by `fitLinearModel`.
#'
#' @return Returns adjusted_R_squared Squared for model
#'
#' @examples
#' data(iris)
#' model <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' adjusted_R_squared <- getAdjustedRSquared(model)
#'
#' @export
getAdjustedRSquared <- function(model) {
  return(model$adjusted_R_squared)
}


#' Return the F statistics value for the model
#'
#' @param model The model list object returned by `fitLinearModel`.
#'
#' @return Returns F statistics for model
#'
#' @examples
#' data(iris)
#' model <- fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' F_statistic <- getFStatistic(model)
#' print(F_statistic)
#'
#' @export
getFStatistic <- function(model) {
  return(list(F_statistic = model$F_statistic, p_value = model$p_value_F))
}

