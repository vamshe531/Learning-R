#Boston data set from MASS library
#Example for practicing linear regression
library(MASS)
library(ISLR)
View(Boston)
names(Boston)
attach(Boston)

lm.fit <- lm(medv~lstat)

summary(lm.fit)
names(lm.fit) #other pieces of information in lin reg
coef(lm.fit) #to extract coefficients of lin reg model
confint(lm.fit) # confidence interval

#for predicting medv for a given value of lstat
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="confidence")
predict(lm.fit,data.frame(lstat=c(5,10,15)),interval="prediction")
