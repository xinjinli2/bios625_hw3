pkgname <- "linearReg"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('linearReg')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("fitLinearModel")
### * fitLinearModel

flush(stderr()); flush(stdout())

### Name: fitLinearModel
### Title: Linear Regression Function
### Aliases: fitLinearModel

### ** Examples

data(iris)
model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
print(model)




cleanEx()
nameEx("getAdjustedRSquared")
### * getAdjustedRSquared

flush(stderr()); flush(stdout())

### Name: getAdjustedRSquared
### Title: Return the adjusted R Squared value for the model
### Aliases: getAdjustedRSquared

### ** Examples

data(iris)
model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
adjusted_R_squared = getAdjustedRSquared(model)




cleanEx()
nameEx("getConfidenceInterval")
### * getConfidenceInterval

flush(stderr()); flush(stdout())

### Name: getConfidenceInterval
### Title: Confidence Interval for Model Coefficients
### Aliases: getConfidenceInterval

### ** Examples

data(iris)
model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
ci = getConfidenceInterval(model)
print(ci)




cleanEx()
nameEx("getFStatistic")
### * getFStatistic

flush(stderr()); flush(stdout())

### Name: getFStatistic
### Title: Return the F statistics value for the model
### Aliases: getFStatistic

### ** Examples

data(iris)
model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
F_statistic = getFStatistic(model)
print(F_statistic)




cleanEx()
nameEx("getRSquared")
### * getRSquared

flush(stderr()); flush(stdout())

### Name: getRSquared
### Title: Return the R Squared value for the model
### Aliases: getRSquared

### ** Examples

data(iris)
model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
R_squared = getRSquared(model)
print(R_squared)




cleanEx()
nameEx("model_summary")
### * model_summary

flush(stderr()); flush(stdout())

### Name: model_summary
### Title: Model Summary
### Aliases: model_summary

### ** Examples

data(iris)
model = fitLinearModel(Petal.Length ~ Petal.Width + Sepal.Length, iris)
model_summary(model)




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
