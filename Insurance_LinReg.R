insurance <- read.csv("C:/Users/Vindhya/Documents/GitHub/Machine-Learning-with-R-datasets-master/insurance.csv",stringsAsFactors=FALSE)
head(insurance)
str(insurance)
attach(insurance)
summary(insurance)
hist(charges)
str(insurance)
table(region, smoker, sex)
table(smoker)
table(sex)

lm.fit <- lm(charges~bmi, data=insurance)
summary(medchrg)
par(mfrow=c(2,2))
plot(lm.fit)
plot(charges,bmi, data=insurance)
confint(lm.fit, level=0.99)

cor(insurance[c("age","bmi","charges")])
pairs(insurance[c("age","bmi","charges")])

library(psych)
pairs.panels(insurance[c("age","bmi","children","charges")])

lm.fit <- lm(charges~.,data=insurance)
lm.fit
summary(lm.fit)
lm.fit2 <- lm(charges~age+bmi+children+smoker)
lm.fit3 <- lm(charges~age+bmi+children+smoker+I(smoker)^2)
summary(lm.fit3)

insurance$bmi30 <- ifelse(insurance$bmi>30,1,0)
lm.fit4 <- lm(charges~age+insurance$bmi30+children+smoker)
summary(lm.fit4)
lm.fit5 <- lm(charges~age+insurance$bmi30+children+smoker+smoker:bmi30, data=insurance)
summary(lm.fit5)
