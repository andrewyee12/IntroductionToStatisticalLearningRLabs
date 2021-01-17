#An application of K-Nearest Neighbors to the Caravan dataset, which is 
#part of the ISLR library. Includes 85 predictors to measure the demographics
#of 5,822 individuals. Response variable is Purchase, indicating if an individual
#purchases caravan insurance policy.

#Import Caravan dataset
library(ISLR)
dim(Caravan)
attach(Caravan)
summary(Purchase)

#KNN may be susceptible to the scale of certain variables. For instance,
#a difference of $1,000 in salary and a difference of 50 years in age may
#potentially be treated in the same manner, despite a significant difference
#in difference. A way to mitigate this issue is to standardize the data.
#Scale all of the variables on a comparable scale, with a mean of 0 and 
#a standard deviation of 1.

standardized.X = scale(Caravan[,-86])
var(Caravan[,1])
var(Caravan[,2])
var(standardized.X[,1])
var(standardized.X[,2])

#Split observations to test set and training set
test = 1:1000
train.X = standardized.X[-test,]
test.X=standardized.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)

#K-Nearest Neighbors with k=1
knn.pred=knn(train.X,test.X,train.Y,k=1)
mean(test.Y!=knn.pred)
mean(test.Y!="No")

#K-Nearest Neighbors with k=3 and k=5
knn.pred=knn(train.X,test.X,train.Y,k=3)
table(knn.pred,test.Y)
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y)
