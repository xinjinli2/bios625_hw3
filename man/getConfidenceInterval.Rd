% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/linearReg.R
\name{getConfidenceInterval}
\alias{getConfidenceInterval}
\title{Confidence Interval for Model Coefficients}
\usage{
getConfidenceInterval(model, level = 0.95)
}
\arguments{
\item{model}{The model list object returned by `fitLinearModel`.}

\item{level}{The confidence level for the interval (default is 0.95).}
}
\value{
Returns a data frame with estimates of the coefficients and their lower and upper bounds.
}
\description{
This function calculates the confidence intervals for the fitted linear regression model with specified confidence level.
}
\examples{
data(iris)
model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
ci = getConfidenceInterval(model)
print(ci)

}
