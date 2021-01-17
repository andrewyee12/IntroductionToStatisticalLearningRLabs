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
mean(glm.pred==Direction)
#Training error rate is 47.8%, however training and testing model on same 
#training dataset can often lead to overoptimistic training error rate.

#Split data set into training and test data training observations
#are from 2001 to 2004 and test data will be observations from 2005 onwards
train=(Year<2005)
Smarket.2005 = Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]

#Predict on new test dataset as 2005
glm.fits=glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket,
             family=binomial, subset=train)
glm.probs=predict(glm.fits,Smarket.2005,type="response")
glm.pred=rep("Down",252)
glm.pred[glm.probs > .5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)
#New error rate using test data set is worst, indicating that it is 
#difficult to predict future market performance using previous returns

#Linear Discriminant Analysis 
lda.fit=lda(Direction~Lag1+Lag2,data=Smarket,subset=train)
lda.fit
plot(lda.fit)
