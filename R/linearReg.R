#' Linear Regression Function
#'
#' This function performs linear regression on input dataset using the ordinary least squares method.
#' It calculates beta coefficients, standard errors of each coefficient, mean squared error (MSE),
#' and returns various statistics, including R-squared, adjusted R-squared, and F statistics.
#'
#' @param formula A formula describing the model to be fitted.
#' @param data A data.frame that contains the variables in the model.
#'
#' @return A list containing model coefficients, standard errors, MSE, R-squared, adjusted R-squared,
#' F-statistic, p-value of the F-statistic, number of observations, and number of predictors, and fitted values.
#'
#' @importFrom stats model.matrix pt pf qt
#'
#' @examples
#' data(iris)
#' model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' print(model)
#'
#' @export
fitLinearModel <- function(formula, data) {

  all_x <- all.vars(formula)
  if (!all(all_x %in% names(data))) {
    stop("Undefined variables in the model formula.")
  }

  X <- model.matrix(formula, data)
  y <- data[[as.character(formula[[2]])]]

  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  beta_hat <- as.numeric(beta_hat)  # Convert beta_hat to a numeric vector
  names(beta_hat) <- colnames(X)

  fitted_values <- X %*% beta_hat
  residuals <- y - fitted_values

  n <- nrow(X)
  p <- ncol(X)

  SSE <- sum(residuals^2)
  MSE <- SSE / (n - p)

  SSY <- sum((y - mean(y))^2)

  R2 <- 1 - SSE / SSY
  adjusted_R2 <- 1 - (1 - R2) * ((n - 1) / (n - p))

  inverse_X_trans_X <- solve(t(X) %*% X)
  se_beta_hat <- sqrt(diag(inverse_X_trans_X) * MSE)

  t_stats <- beta_hat / se_beta_hat
  p_values <- 2 * pt(-abs(t_stats), df = n - p)

  # F-statistic
  F_stat <- ((SSY - SSE) / (p - 1)) / (SSE / (n - p))
  p_value_F <- pf(F_stat, df1 = p - 1, df2 = n - p, lower.tail = FALSE)

  return(list(
    coefficients = beta_hat,
    SE_beta_hat = se_beta_hat,
    t_values = t_stats,
    p_values = p_values,
    fitted.values = fitted_values,
    residuals = residuals,
    MSE = MSE,
    R_squared = R2,
    adjusted_R_squared = adjusted_R2,
    F_statistic = F_stat,
    df1 = p - 1,
    df2 = n - p
  ))
}

#' Model Summary
#'
#' The purpose of this function is to show a full summary of the linear regression model, including
#' estimates of the coefficients, standard errors, t-values, and p-values of fitted coefficients.
#'
#' @param model The model list object returned by `fitLinearModel()`.
#'
#' @return Prints the summary table
#'
#' @examples
#' data(iris)
#' model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' model_summary(model)
#'
#' @export
model_summary <- function(model) {

  coefficients <- model$coefficients
  std_errors <- model$SE_beta_hat
  t_values <- model$t_values
  p_values <- model$p_values
  mse <- model$MSE
  R2 <- model$R_squared
  adjusted_R2 <- model$adjusted_R_squared
  F_statistic <- model$F_statistic
  df1 <- model$df1
  df2 <- model$df2

  signif_codes <- ifelse(
    p_values < 0.001, "***",
    ifelse(p_values < 0.01, "**",
           ifelse(p_values < 0.05, "*",
                  ifelse(p_values < 0.1, ".", " ")))
  )

  summary_table <- data.frame(
    Estimate = coefficients,
    Std.Error = std_errors,
    t.values = t_values,
    P.values = p_values,
    Signif = signif_codes
  )

  rownames(summary_table) <- colnames(model$X)
  # print(summary_table)

  cat("\nResidual standard error:", sqrt(mse), "on", df2, "degrees of freedom\n")
  cat("Multiple R-squared:", R2, ", Adjusted R-squared:", adjusted_R2, "\n")
  cat("F-statistic:", F_statistic, "on", df1, "and", df2, "DF, p-value:", format.pval(model$p_value_F), "\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")

  return(summary_table)
}



#' Confidence Interval for Model Coefficients
#'
#' This function calculates the confidence intervals for the fitted linear regression model with specified confidence level.
#'
#' @param model The model list object returned by `fitLinearModel`.
#' @param level The confidence level for the interval (default is 0.95).
#'
#' @return Returns a data frame with estimates of the coefficients and their lower and upper bounds.
#'
#' @examples
#' data(iris)
#' model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' ci = getConfidenceInterval(model)
#' print(ci)
#'
#' @export
getConfidenceInterval <- function(model, level = 0.95) {

  beta_hat <- model$coefficients
  se_beta_hat <- model$SE_beta_hat
  n <- length(model$fitted.values)
  k <- length(beta_hat)


  alpha <- 1 - level
  t_crit <- qt(1 - alpha / 2, df = n - k)

  lwr_bd <- beta_hat - t_crit * se_beta_hat
  upr_bd <- beta_hat + t_crit * se_beta_hat

  lwr_col_name <- paste0(round((1 - level) / 2 * 100, 1), " %")
  upr_col_name <- paste0(round((level + (1 - level) / 2) * 100, 1), " %")

  ci_summary <- data.frame(
    lwr_bd = lwr_bd,
    upr_bd = upr_bd
  )

  colnames(ci_summary) <- c(lwr_col_name, upr_col_name)

  rownames(ci_summary) <- names(beta_hat)

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
#' model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' R_squared = getRSquared(model)
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
#' model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' adjusted_R_squared = getAdjustedRSquared(model)
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
#' model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
#' F_statistic = getFStatistic(model)
#' print(F_statistic)
#'
#' @export
getFStatistic <- function(model) {
  return(model$F_statistic)
}

