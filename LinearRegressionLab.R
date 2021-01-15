#Import libraries
library(MASS)
library(ISLR)

#Utilizing Boston data set within the MASS library
names(Boston)

#Fit linear regression model with medv as response and lstat as predictor
lm.fit = lm(medv~lstat, data=Boston)
attach(Boston)
lm.fit = lm(medv~lstat)
lm.fit

#Summary of Linear model 
summary(lm.fit)

names(lm.fit)
coef(lm.fit)
#Confidence Interval
confint(lm.fit)

#Predict responses with confidence intervals
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="confidence")

#Predict responses with prediction intervals
predict(lm.fit, data.frame(lstat=c(5,10,15)), interval="prediction")

#Visualize dataset points
plot(lstat, medv)
#Plot linear regression fit
abline(lm.fit)
abline(lm.fit, lwd=3)
abline(lm.fit, lwd=3,col="red")

#Plot linear model fit plots
par(mfrow=c(2,2))
plot(lm.fit)

#Multiple Linear Regression using lstat and age as predictors
lm.fit = lm(medv~lstat+age, data=Boston)
summary(lm.fit)

#Multiple linear regression using all 13 predictors in the Boston data set
lm.fit=lm(medv~., data=Boston)
summary(lm.fit)

lm.fit1=update(lm.fit, ~.-age)
summary(lm.fit1)

#Interaction Terms between lstat and age predictors
summary(lm(medv~lstat*age, data=Boston))

#Non-linear transformation of predictors
lm.fit2 = lm(medv~lstat+I(lstat^2))
summary(lm.fit2)

#Qualitative predictors
names(Carseats)
lm.fit = lm(Sales~. +Income:Advertising+Price:Age, data=Carseats)
summary(lm.fit)

attach(Carseats)
contrasts(ShelveLoc)
?contrasts
