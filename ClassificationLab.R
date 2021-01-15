#Lab: Logistic Regression, Linear Discriminant Analysis
#Quadratic Discriminant Analysis, K-Nearest Neighbors

#Import Stock market data from ISLR library
library(ISLR)
names(Smarket)
dim(Smarket)
summary(Smarket)


pairs(Smarket)
#Produce matrix of pairwise correlations among
#the predictors in the dataset.
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

#Fit logistic regression model to predict Direction
#of stock price based on precentage returns from 
#previous five days and volume

glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,
             data = Smarket, family=binomial)
summary(glm.fits)

#Access coefficients of fitted models predictors
coef(glm.fits)
summary(glm.fits)$coef[,4]

#Predict probabilities of market movement
glm.probs=predict(glm.fits, type="response")
glm.probs[1:10]
contrasts(Direction)

#Create vector of classification predictions, threshold
#being that market increase if probability is greater than
#0.5 or will decrease if less than
glm.pred=rep("Down", 1250)
glm.pred[glm.probs>.5] = "Up"

#Create confusion matrix to observe correct and incorrect classifications
table(glm.pred, Direction)
