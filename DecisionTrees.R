library(C50)
credit <- read.csv("C:/Users/Vindhya/Documents/GitHub/Machine-Learning-with-R-datasets-master/credit.csv",stringsAsFactors=FALSE)
str(credit)
credit$checking_balance <- factor(credit$checking_balance)
credit$savings_balance <- factor(credit$savings_balance)
table(credit$checking_balance)
table(credit$savings_balance)
summary(credit$months_loan_duration)
summary(credit$amount)
table(credit$default)
#set.seed(12345)
credit_rand <- credit[order(runif(1000)),]
credit_rand$default <- factor(credit_rand$default)
credit_train <- credit_rand[1:900,]
credit_test <- credit_rand[901:1000,]


prop.table(table(credit_train$default))
prop.table(table(credit_test$default))

credit_model <- C5.0(credit_train[-21], credit_train$default)
summary(credit_model)

credit_pred <- predict(credit_model,credit_test)

table(credit_test$default, credit_pred)

credit_boost10 <- C5.0(credit_train[-21],credit_train$default, trials=10)
summary(credit_boost10)
error_cost <- matrix(c(0,1,4,0),nrow=2)
credit_cost <- C5.0(credit_train[-21],credit_train$default,costs=error_cost)
credit_cost_pred <- predict(credit_cost,credit_test)
table(credit_test$default, credit_cost_pred)
