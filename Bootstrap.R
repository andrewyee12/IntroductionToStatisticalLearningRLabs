library(ISLR)
library(boot)
#Bootstrap is used to estimate the accuracy of a statistic of interest

#Create function to compute alpha statistic
alpha.fn=function(data,index) {
  X=data$X[index]
  Y=data$Y[index]
  return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
}

#Estimate alpha with 100 observations in Portfolio dataset
alpha.fn(Portfolio,1:100)

#Randomly select 100 observations from range 1 to 100 with replacement
#constructing a new bootstrap data set to recompute alpha hat.
set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace =T))

#Utilize boot() function to implement bootstrap set with 1000 bootstrap estimates
boot(Portfolio,alpha.fn,R=1000)

#Bootstrap can be used to assess variability of the coefficient estimates
#and predictions from a statistical learning method.

#Assess variability of intercept and slope terms for linear regression
boot.fn = function(data,index)
return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)

#boot.fn() function can be used to create bootstrap estimates for intercept
#and slope terms by randomly sampling among observations with replacement.
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))

#use boot() function to compute standard errors of 1,000 bootstrap estimates
boot(Auto,boot.fn,1000)

#Bootstrap estimated that the intercept's standard error is 0.84 and
#the slope's standard error is 0.0073.

summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
  coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)

summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef
