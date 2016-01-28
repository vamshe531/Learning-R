#Example code for learning logistic,LDA, QDA
library(ISLR)
names(Smarket)
summary(Smarket)
dim(Smarket)
attach(Smarket)
plot(Volume)
cor(Smarket) #will give an error since last column is not numeric
cor(Smarket[,-9])
pairs(Smarket[,-9])

#logistic regression model
glm.fit = glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial)
summary(glm.fit)
names(glm.fit)
coef(glm.fit)
glm.probs= predict(glm.fit,type="response") 
#The type="response" option tells R to output probabilities of the form P(Y = 1|X)
#sine no dataset is supplies, it predicts probabilities for training set
glm.probs[1:10] # prints the probabilities for first 10
contrasts(Direction) #shows what dummy variable is used for up and down

#since model predicted for training data, checking the accuracy of model
glm.pred =rep("Down",1250)  #creates a vector with 1250 Down values
glm.pred[glm.probs>0.5]="Up" #replaces DOwn with Up for glm.prob>0.5
table(glm.pred,Direction) #gives a matrix of actual vs predicted
mean(glm.pred==Direction)


train=(Year<2005)
Smarket.2005=Smarket[!train,]
dim(Smarket.2005)
Direction.2005=Direction[!train]
#the above 4 lines creates a subset of data (test data) where year=2005

# create model for training data only
glm.fit =glm(Direction~Lag1+Lag2+Lag3+Lag4+Lag5+Volume,data=Smarket,family=binomial,subset=train)
#testing the model on test data
glm.probs= predict(glm.fit,Smarket.2005,type="response")

glm.pred=rep("Down",252)
glm.pred[glm.probs>0.5]="Up"
table(glm.pred,Direction.2005)
mean(glm.pred!=Direction.2005) #calculates test error rate

#Linear Discriminant Analysis
