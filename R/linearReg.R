#' Linear Regression Function
#'
#' This function performs linear regression on input dataset using ordinary least squares method.
#' This function calculates beta coefficients, standard errors of each coefficient, mean squared error (MSE or sigma squared hat).
#' It also returns various statistics including R-squared, adjusted R-squared, F statistics.
#'
#' @param formula An formula described the model to be fitted.
#' @param data A data.frame that contains the variables in the model.
#'
#' @return A list containing model coefficients, standard errors, sigma squared, R-squared, adjusted R-squared,
#' F-statistic, p-value of the F-statistic, number of observations, and number of predictors, and fitted values.
#'
#' @importFrom stats model.matrix pt qt pf
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

  # if (nrow(X) != length(y)) {
  #   stop("Number of rows in X and length of y must be the same")
  # }

  beta_hat <- solve(t(X) %*% X) %*% t(X) %*% y
  y_hat <- X %*% beta_hat
  residuals <- y - y_hat
  SSE <- sum(residuals^2)
  SSY <- sum((y - mean(y))^2)
  SSR <- SSY - SSE

  R_2 <- SSR / SSY
  adjusted_R_2 <- 1 - (1 - R_2) * ((nrow(X) - 1) / (nrow(X) - ncol(X) - 1))

  sigma_2 <- SSE / (nrow(X) - ncol(X))
  var_beta_hat <- sigma_2 * solve(t(X) %*% X)
  se_beta_hat <- sqrt(diag(var_beta_hat))

  t_stats <- beta_hat / se_beta_hat
  p_values <- 2 * pt(-abs(t_stats), df = nrow(X) - ncol(X))

  F_stats <- (SSR / (ncol(X) - 1)) / (SSE / (nrow(X) - ncol(X)))
  p_value_F <- pf(F_stats, df1 = ncol(X) - 1, df2 = nrow(X) - ncol(X), lower.tail = FALSE)

  return(list(
    coefficients = beta_hat,
    se_beta_hat = se_beta_hat,
    t_stats = t_stats,
    p_values = p_values,
    sigma_squared = sigma_2,
    R_squared = R_2,
    adjusted_R_squared = adjusted_R_2,
    F_statistic = F_stats,
    p_value_F = p_value_F,
    n = nrow(X),
    k = ncol(X),
    fitted.values = y_hat,
    X = X
  ))
}


#' Model Summary
#'
#' The purpose of this function to show a full summary of the linear regression model, including
#' estimates of the coefficients, standard errors, t-values, and p-values of fitted coefficients.
#' It also displays significane codes, residual standard error, R squared, adjusted R squared,
#' and F statistics, degrees of freedoms, and p-values.
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
    P.values = p_values,
    Signif = signif_codes
  )

  rownames(model_summary) <- colnames(model$X)
  print(model_summary)


  cat("\nResidual standard error:", sqrt(sigma_squared), "on", n - k, "degrees of freedom\n")
  cat("Multiple R-squared:", R_squared, ", Adjusted R-squared:", adjusted_R_squared, "\n")
  cat("F-statistic:", F_statistic, "on", k - 1, "and", n - k, "DF, p-value:", format.pval(p_value_F), "\n")
  cat("Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n\n")
  # return(model_summary)
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

  beta_hat <- model$beta_hat
  se_beta_hat <- model$se_beta_hat
  n <- model$n
  k <- model$k


  alpha <- 1 - level
  t_crit <- qt(1 - alpha / 2, df = n - k)

  lwr_bd <- beta_hat - t_crit * se_beta_hat
  upr_bd <- beta_hat + t_crit * se_beta_hat

  lwr_col_name <- paste0(round((1 - level) / 2 * 100, 1), "%")
  upr_col_name <- paste0(round((level + (1 - level) / 2) * 100, 1), "%")

  ci_summary <- data.frame(
    lwr_bd = lwr_bd,
    upr_bd = upr_bd
  )

  colnames(ci_summary) <- c(lwr_col_name, upr_col_name)

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
  return(list(F_statistic = model$F_statistic, p_value = model$p_value_F))
}

