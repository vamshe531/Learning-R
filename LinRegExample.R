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

plot(lstat,medv, pch=20)
abline(lm.fit,lwd=3,col="blue")

par(mfrow=c(2,2))  #divides display into 2x2 
plot(lm.fit) #plotting the model will give disgnostic plots 
#check for non linearity, collinearity

residuals(lm.fit) #gives residuals in a table form
names(lm.fit) #lists all the other variables available in model
plot(predict(lm.fit), residuals(lm.fit))
plot(predict(lm.fit), rstudent(lm.fit))

plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) #gives which observataion has largest leverage statistic


#multiple linear regression analysis
ml.fit =lm(medv~lstat+age,data=Boston)
par(mfrow=c(2,2))
summary(ml.fit)

ml.fit2 =lm(medv~.,data=Boston) #does multiple regression using all variables

#Nonlinear transformation
#sometimes nonlinear transforms helps to fit the model better
lm.fit2 <- lm(medv~lstat+I(lstat^2))
summary(lm.fit2)
anova(lm.fit,lm.fit2) #using anova function to further quantify the extent to which quadratic fit is 
#superior to linear fit

#inorder to create a cubic fit we can do I(X^3) ut better approach is using poly()
lm.fit5 <- lm(medv~poly(lstat,5))
summary(lm.fit5)

#if these is a factor data in the data, ex:good, bad, medium), then 
#contrasts fucntion can be used to see what value is assigned to those factors
#see chapter 3 in ISLR textbook



