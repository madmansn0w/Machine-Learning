library(MASS)
library(ISLR)
fix(Boston)
names(Boston)
#lm to fit simple linear regression
lm.fit=lm(medv~lstat,data=Boston)
attach(Boston)
lm.fit
summary(lm.fit)
names(lm.fit)

# Obtain confidence interval for the coefficient estimates:
confint(lm.fit)
# Obtain prediction intervals for the prediction of medv for a given lstat
# -- we find that the 95% confidence interval for an lstat value of 10 is (24.47, 25.63)
# -- and the 95% prediction interval is (12.828, 37.28) while the intervals are centered
# -- on the point with a value of 25.05 for medv when lstat = 10.
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="confidence")
predict(lm.fit,data.frame(lstat=(c(5,10,15))) ,interval="prediction")
# Plot medv and lstat along the least squares regression line.
plot(lstat, medv)
# abline() is used to draw any line,not just LSR line
abline(lm.fit)
abline(lm.fit,lwd=3)
abline(lm.fit,lwd=3,col="red")
plot(lstat,medv,col="red")
plot(lstat,medv,pch=20)
plot(lstat,medv,pch="+")
plot(1:20,1:20,pch=1:20

# diagnostic plots
par(mfrow=c(2,2))
plot(lm.fit)

# Linear regression fit
plot(predict(lm.fit), residuals(lm.fit))
# Studentized residual
plot(predict(lm.fit), rstudent(lm.fit))

# Some evidence of non-linearity in residual...
# We compute leerage statistics for predictors using hatvalues()
plot(hatvalues(lm.fit))
# Largest leverage statistic index is found with which.max()
which.max(hatvalues(lm.fit))

# To fit multiple linear regression model using least squares, we use the lm() function
# syntax: lm(y~x1+x2+x3)
# summary() outputs the regression coefficients for the predictors:
lm.fit=lm(medv~lstat+age, data=Boston)
summary(lm.fit)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -15.981  -3.978  -1.283   1.968  23.158

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 33.22276    0.73085  45.458  < 2e-16 ***
# lstat       -1.03207    0.04819 -21.416  < 2e-16 ***
# age          0.03454    0.01223   2.826  0.00491 ** 
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 6.173 on 503 degrees of freedom
# Multiple R-squared:  0.5513,    Adjusted R-squared:  0.5495 
# F-statistic:   309 on 2 and 503 DF,  p-value: < 2.2e-16

lm.fit=lm(medv~., data=Boston)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -15.981  -3.978  -1.283   1.968  23.158

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept) 33.22276    0.73085  45.458  < 2e-16 ***
# lstat       -1.03207    0.04819 -21.416  < 2e-16 ***
# age          0.03454    0.01223   2.826  0.00491 **
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 6.173 on 503 degrees of freedom
# Multiple R-squared:  0.5513,    Adjusted R-squared:  0.5495
# F-statistic:   309 on 2 and 503 DF,  p-value: < 2.2e-16
summary(lm.fit)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -15.595  -2.730  -0.518   1.777  26.199

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)  3.646e+01  5.103e+00   7.144 3.28e-12 ***
# crim        -1.080e-01  3.286e-02  -3.287 0.001087 **
# zn           4.642e-02  1.373e-02   3.382 0.000778 ***
# indus        2.056e-02  6.150e-02   0.334 0.738288    
# chas         2.687e+00  8.616e-01   3.118 0.001925 **
# nox         -1.777e+01  3.820e+00  -4.651 4.25e-06 ***
# rm           3.810e+00  4.179e-01   9.116  < 2e-16 ***
# age          6.922e-04  1.321e-02   0.052 0.958229
# dis         -1.476e+00  1.995e-01  -7.398 6.01e-13 ***
# rad          3.060e-01  6.635e-02   4.613 5.07e-06 ***
# tax         -1.233e-02  3.760e-03  -3.280 0.001112 **
# ptratio     -9.527e-01  1.308e-01  -7.283 1.31e-12 ***
# black        9.312e-03  2.686e-03   3.467 0.000573 ***
# lstat       -5.248e-01  5.072e-02 -10.347  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 4.745 on 492 degrees of freedom
# Multiple R-squared:  0.7406,    Adjusted R-squared:  0.7338 
# F-statistic: 108.1 on 13 and 492 DF,  p-value: < 2.2e-16

# We can look up R^2 via
summary(lm.fit)$r.sq
# R^2 = 0.7406427

# We can look up RSE via
summary(lm.fit)$sigma
# sigma, or the RSE is found to be 4.745298

# Compute variance inflation factors with vif()
library(car)
vif(lm.fit)
#     crim       zn    indus     chas      nox       rm      age      dis
# 1.792192 2.298758 3.991596 1.073995 4.393720 1.933744 3.100826 3.955945
#      rad      tax  ptratio    black    lstat
# 7.484496 9.008554 1.799084 1.348521 2.941491

# We see from the above data output that 'age' has a high p-value
# Since we may want to run a regression excluding this predictor