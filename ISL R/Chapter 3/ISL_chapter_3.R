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
plot(1:20,1:20,pch=1:20)

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
lm.fit1=lm(medv∼.-age ,data=Boston) 
summary(lm.fit1) # Alternatively: lm.fit1=update (lm.fit , ∼.-age)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -15.6054  -2.7313  -0.5188   1.7601  26.2243

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)
# (Intercept)  36.436927   5.080119   7.172 2.72e-12 ***
# crim         -0.108006   0.032832  -3.290 0.001075 **
# zn            0.046334   0.013613   3.404 0.000719 ***
# indus         0.020562   0.061433   0.335 0.737989
# chas          2.689026   0.859598   3.128 0.001863 **
# nox         -17.713540   3.679308  -4.814 1.97e-06 ***
# rm            3.814394   0.408480   9.338  < 2e-16 ***
# dis          -1.478612   0.190611  -7.757 5.03e-14 ***
# rad           0.305786   0.066089   4.627 4.75e-06 ***
# tax          -0.012329   0.003755  -3.283 0.001099 **
# ptratio      -0.952211   0.130294  -7.308 1.10e-12 ***
# black         0.009321   0.002678   3.481 0.000544 ***
# lstat        -0.523852   0.047625 -10.999  < 2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 4.74 on 493 degrees of freedom
# Multiple R-squared:  0.7406,    Adjusted R-squared:  0.7343
# F-statistic: 117.3 on 12 and 493 DF,  p-value: < 2.2e-16


#The syntax lstat:black tells R to include an interaction term between lstat and black.
# The syntax lstat*age simultaneously includes lstat, age,
# and the interaction term lstat×age as predictors; it is a shorthand for
# lstat+age+lstat:age.
summary(lm(medv~lstat*age, data=Boston))
# Residuals:
#     Min      1Q  Median      3Q     Max 
# -15.806  -4.045  -1.333   2.085  27.552

# Coefficients:
#               Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 36.0885359  1.4698355  24.553  < 2e-16 ***
# lstat       -1.3921168  0.1674555  -8.313 8.78e-16 ***
# age         -0.0007209  0.0198792  -0.036   0.9711
# lstat:age    0.0041560  0.0018518   2.244   0.0252 *
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 6.149 on 502 degrees of freedom
# Multiple R-squared:  0.5557,    Adjusted R-squared:  0.5531 
# F-statistic: 209.3 on 3 and 502 DF,  p-value: < 2.2e-16

# The lm() function can also accommodate non-linear transformations of the
# predictors. For instance, given a predictor X, we can create a predictor X2
# using I(X^2).
#We now perform a regression of medv onto lstat and lstat2.
lm.fit2=lm(medv∼lstat +I(lstat ^2), data=Boston)
summary(lm.fit2)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -15.2834  -3.8313  -0.5295   2.3095  25.4148

# Coefficients:
#              Estimate Std. Error t value Pr(>|t|)
# (Intercept) 42.862007   0.872084   49.15   <2e-16 ***
# lstat       -2.332821   0.123803  -18.84   <2e-16 ***
# I(lstat^2)   0.043547   0.003745   11.63   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 5.524 on 503 degrees of freedom
# Multiple R-squared:  0.6407,    Adjusted R-squared:  0.6393
# F-statistic: 448.5 on 2 and 503 DF,  p-value: < 2.2e-16

# Near-zero p-value associated with quadratic term suggests improved model.
# anova() quantifies the extent of the improvement of the quadratic fit.
lm.fit = lm(medv~lstat) # reads 'medv onto lstat' or 'medv regress on lstat'
anova(lm.fit, lm.fit2)
# Analysis of Variance Table
# Model 1: medv ~ lstat
# Model 2: medv ~ lstat + I(lstat^2)
#   Res.Df   RSS Df Sum of Sq     F    Pr(>F)
# 1    504 19472
# 2    503 15347  1    4125.1 135.2 < 2.2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Model 1 represents the linear submodel containing only one predictor, lstat,
# Model 2 corresponds to the larger quadratic model that has two predictors, lstat and lstat^2
# The null hypothesis is that the two models fit the data equally well, 
# and the alternative hypothesis is that the full model is superior.
# F-statistic reads 135.2 and the p-value ~ 0, which suggests a good fit.
par(mfrow=c(2,2))
plot(lm.fit2)

# poly() creates a polynomial for higher-order functions within lm()
lm.fit5 = lm(medv~poly(lstat, 5))
summary(lm.fit5)
# Residuals:
#      Min       1Q   Median       3Q      Max
# -13.5433  -3.1039  -0.7052   2.0844  27.1153

# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       22.5328     0.2318  97.197  < 2e-16 ***
# poly(lstat, 5)1 -152.4595     5.2148 -29.236  < 2e-16 ***
# poly(lstat, 5)2   64.2272     5.2148  12.316  < 2e-16 ***
# poly(lstat, 5)3  -27.0511     5.2148  -5.187 3.10e-07 ***
# poly(lstat, 5)4   25.4517     5.2148   4.881 1.42e-06 ***
# poly(lstat, 5)5  -19.2524     5.2148  -3.692 0.000247 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 5.215 on 500 degrees of freedom
# Multiple R-squared:  0.6817,    Adjusted R-squared:  0.6785 
# F-statistic: 214.2 on 5 and 500 DF,  p-value: < 2.2e-16

# Above suggests that additional polynomial terms, up to the 5th order, improves
# the model fit. p-values that do not have significant terms, suggest otherwise.

# Trying a log fit...
summary (lm(medv∼log(rm),data=Boston ))
# Residuals:
#     Min      1Q  Median      3Q     Max
# -19.487  -2.875  -0.104   2.837  39.816 

# Coefficients:
#             Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  -76.488      5.028  -15.21   <2e-16 ***
# log(rm)       54.055      2.739   19.73   <2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 6.915 on 504 degrees of freedom
# Multiple R-squared:  0.4358,    Adjusted R-squared:  0.4347 
# F-statistic: 389.3 on 1 and 504 DF,  p-value: < 2.2e-16

# Working with qualitative data...
fix(Carseats)
names(Carseats)
# [1] "Sales"       "CompPrice"   "Income"      "Advertising" "Population"   
# [6] "Price"       "ShelveLoc"   "Age"         "Education"   "Urban"        
# [11] "US"

# Shelveloc is qualitative. (Bad, medium, good)
# Given a qualitative variable such as Shelveloc, R generates dummy variables automatically.
lm.fit = lm(Sales∼.+Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)
# Residuals:
#     Min      1Q  Median      3Q     Max
# -2.9208 -0.7503  0.0177  0.6754  3.3413

# Coefficients:
#                      Estimate Std. Error t value Pr(>|t|)
# (Intercept)         6.5755654  1.0087470   6.519 2.22e-10 ***
# CompPrice           0.0929371  0.0041183  22.567  < 2e-16 ***
# Income              0.0108940  0.0026044   4.183 3.57e-05 ***
# Advertising         0.0702462  0.0226091   3.107 0.002030 ** 
# Population          0.0001592  0.0003679   0.433 0.665330    
# Price              -0.1008064  0.0074399 -13.549  < 2e-16 ***
# ShelveLocGood       4.8486762  0.1528378  31.724  < 2e-16 ***
# ShelveLocMedium     1.9532620  0.1257682  15.531  < 2e-16 ***
# Age                -0.0579466  0.0159506  -3.633 0.000318 ***
# Education          -0.0208525  0.0196131  -1.063 0.288361    
# UrbanYes            0.1401597  0.1124019   1.247 0.213171    
# USYes              -0.1575571  0.1489234  -1.058 0.290729    
# Income:Advertising  0.0007510  0.0002784   2.698 0.007290 **
# Price:Age           0.0001068  0.0001333   0.801 0.423812
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Residual standard error: 1.011 on 386 degrees of freedom
# Multiple R-squared:  0.8761,    Adjusted R-squared:  0.8719 
# F-statistic:   210 on 13 and 386 DF,  p-value: < 2.2e-16

# dummy variables: ShelveLocGood, ShelveLocMedium 
# contrasts() shows how the programming is done for dummy variables...
attach(Carseats)
contrasts(ShelveLoc)
#        Good Medium
# Bad       0      0
# Good      1      0
# Medium    0      1


# Writing custom functions..
# LoadLibraries() returns error if custom function is not created first.
LoadLibraries = function() {
    library(ISLR)
    library(MASS)
    print("Libraries have been loaded")
}
LoadLibraries()

# 3.7 Exercises...
# 1.
# Describe the null hypotheses to which the p-values given in Table 3.4
# correspond.
# Coefficient   Std. error      t-statistic         p-value
# Intercept     2.939           0.3119              9.42 < 0.0001
# TV            0.046           0.0014              32.81 < 0.0001
# radio         0.189           0.0086              21.89 < 0.0001
# newspaper     −0.001          0.0059              −0.18 0.8599

 # Baseline for intercept is sales.
 # p-value for sales (2,939 units) means that it is unlikely to be 0.
 # p-value higher than .05 is not statistically significant and indicates strong
 # evidence for the null hypothesis, which means we reject the alternative
 # hypothesis. (cannot accept null hypothesis, only reject or fail to reject)
 # TV is likely to have a positive effect on sales. (46 sales per $1000 spent)
 # Radio is likely to have a positive effect on sales. (189 sales per $1000 spent)
 # Newspaper is not likely to have any real relationship with sales due to a high p-value.

# 2.
# Carefully explain the differences between the KNN classifier and KNN
# regression methods.

# KNN classifier takes a single observation as input and returns an output that 
# is the average of the K-nearest inputs.

# KNN regression takes an assumes an abstract point as a real observation and estimates the 
# expected output such that it operates as a function that predicts outputs for inputs.
# Useful for prediction boundaries...

# 3.
# Suppose we have a data set with five predictors,
# X1 = GPA,
# X2 = IQ,
# X3 = Gender (1 Female, 0 Male)
# X4 = Interaction btwn GPA & IQ
# X5 = Interaction btwn GPA & Gender
# Output = starting salary after graduation (thousands)
# Assuming least squares fits the model and returns 
# B0 = 50,
# B1 = 20
# B2 = .07
# B3 = 35
# B4 = .01
# B5 = -10
# Which answer is correct, and why?
# i. For a fixed value of IQ and GPA, males earn more on average than females.
# ii. For a fixed value of IQ and GPA, females earn more on average than males.
# iii. For a fixed value of IQ and GPA, males earn more on average than females provided that the GPA is high enough.
# iv. For a fixed value of IQ and GPA, females earn more on average than males provided that the GPA is high enough.

# Baseline is males (encoded as 0)
# Fixed values of IQ & GPA => 50 + 20*X1 + .07*X2 + 35X3 + .01*X1X2 - 10*X1*X3
# Suggests low GPA  => Females earn more. High GPA, males earn more.
# iii is true, unless low GPA -> ii is true.

# Predict salary for a female with an IQ of 110 and a GPA of 4.0
# Female
50+(4.0*20)+(110*0.07)+35+0.01*110*4.0+(-10*4) # 137.1
# Male
50+(4.0*20)+(110*0.07)+0.01*110*4.0 # 142.1

# True or false: Since the coefficient for the GPA/IQ interaction 
# term is very small, there is very little evidence of an interaction effect. 
# Justify your answer.

# False. Even though it has a small coeff, it does not imply a lack of interaction.
# If the data had very low variance around the fit, this would result in a high valued confidence interval,
# even if the coeff is small in magnitude. 
