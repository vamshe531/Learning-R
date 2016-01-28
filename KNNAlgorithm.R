library(RWeka)
library(class)
library(gmodels)
wbcd <- read.csv("C:/Users/vamsi.reddy/Documents/GitHub/Machine-Learning-with-R-datasets/wisc_bc_data.csv", stringsAsFactors=FALSE)
wbcd <- wbcd[-1] #dropping first column
head(wbcd)
table(wbcd$diagnosis) #to check how many are malignant/bad
wbcd$diagnosis <- factor(wbcd$diagnosis, levels=c("B","M"),labels=c("Benign","Malignant"))
table(wbcd$diagnosis) #gives quantitative count of B and M
round(prop.table(table(wbcd$diagnosis))*100,digits=1) #to express the count of B or M in %
summary(wbcd)
summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

#since the measurement scales are different we need to normalize since distance calculation will be skewed
#function for normalizing the attributes
normalize <- function(x){
  return((x-min(x)/max(x)-min(x)))
}
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))
summary(wbcd_n)

#creating training, test sets, confirm is data is random if not need to select random rows
wbcd_train <- wbcd_n[1:469,]
head(wbcd_train)
wbcd_test <- wbcd_n[470:569,]
head(wbcd_test)
wbcd_train_labels <- wbcd[1:469,1]
table(wbcd_train_labels)
wbcd_test_labels <- wbcd[470:569,1]
table(wbcd_test_labels)
wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)

CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq=FALSE) #to check the performance
#alternatively you can use
table(wbcd_test_pred,wbcd_test_labels)


#alternative to normalization we can also standardize using z score
wbcd_z <- as.data.frame(scale(wbcd[-1]))
summary(wbcd_z) #make sure mean is always 0 if z score is used for standardization
#creating training, test sets, confirm is data is random if not need to select random rows
wbcd_train <- wbcd_z[1:469,]
head(wbcd_train)
wbcd_test <- wbcd_z[470:569,]
head(wbcd_test)
wbcd_train_labels <- wbcd[1:469,1]
table(wbcd_train_labels)
wbcd_test_labels <- wbcd[470:569,1]
table(wbcd_test_labels)
wbcd_test_pred_z <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)

CrossTable(x=wbcd_test_labels,y=wbcd_test_pred,prop.chisq=FALSE) #to check the performance
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred_z,prop.chisq=FALSE) #to check the performance


#make sure the model is checked against other values of k
