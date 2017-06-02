 #importing 5000 customers
library(readr)
customer <- read_csv("D:/Prateik/Quarter 2/STATS/Customer churn - final case study/CustomerChurnData_5000.csv")

#cleaning customer data set
customer <- customer[1:5000, -1]

#Randomly selecting train (80%) and test (20%)
n <- nrow(customer)
shuffled_customer <- customer[sample(n), ]
train_indices <- 1:round(0.8 * n)
train <- shuffled_customer[train_indices, ]
test_indices <- (round(0.8  * n) + 1):n
test <- shuffled_customer[test_indices, ]

#importing 1000 customers
#test <- read_csv("D:/Prateik/Quarter 2/STATS/Customer churn - final case study/CustomerChurnData_1000.csv")


dim(train)
dim(test)

#-------------------- 1st MODEL (AIC = 1791) - This model has all variables----------------------------
#Running logistic regression on all variables
model1 <- glm(train$`Churn (1 = Yes, 0 = No)`~.,family=binomial(link=logit),data=train)
summary(model1)

#On basis of model probabilities for test set
responsemodel1 <- predict(model1, newdata = test, type = "response")

#loop for calculating accuracies for cut-offs from 0.05 to 0.6
cutoff <- seq(0.05, 0.6, 0.05)
a <-c()
acc <- c()
j = 1
for(i in cutoff){
  a <- ifelse (responsemodel1 > i, 1, 0)
  t <- table(test$`Churn (1 = Yes, 0 = No)`, a)
  acc [j]<- sum(diag(t))/sum(t)
  print(paste0("Accuracy for ", i ," is ", acc[j]))
  j <- j+1
  a <- c()
}




#-------------------- 2nd MODEL (AIC 1789) - This model has all variables that are significant in 1st model (pvalue <0.05)----------------------------
#cropping train and set to variables being used
model2_train <- train[,-c(4,5,6,7,8,9,10)]
model2_test <- test[,-c(4,5,6,7,8,9,10)]

#running logistic regression on variables which are significant in "model" best AIC = 1789
model2 <- glm(model2_train$`Churn (1 = Yes, 0 = No)`~ ., family=binomial(link=logit), data=model2_train)
summary(model2)

#On basis of model1 probabilities for test set
responsemodel2 <- predict(model2, newdata=model2_test, type = "response")

#Probabilities on basis of model1
#responsemodel2 <- responsemodel2[1:1000]

#loop for calculating accuracies for cut-offs from 0.05 to 0.6
a <-c()
acc <- c()
j = 1
for(i in cutoff){
  a <- ifelse (responsemodel2 > i, 1, 0)
  t <- table(test$`Churn (1 = Yes, 0 = No)`, a)
  acc [j]<- sum(diag(t))/sum(t)
  print(paste0("Accuracy for ", i ," is ", acc[j]))
  j <- j+1
  a <- c()
}




#-------------------- 3rd MODEL (AIC 1791) - This model has all variables that are significant in 2nd model (pvalue <0.05)----------------------------
#Cropping train and test for model 3
model3_train <- model2_train[,-5]
model3_test <- model2_test[,-5]

#running logistic regression on variables
model3 <- glm (model3_train$`Churn (1 = Yes, 0 = No)`~ ., family=binomial(link=logit), data = model3_train)
summary(model3)

#On basis of model3 probabilities for test set
responsemodel3 <- predict(model3, newdata = model3_test, type = "response")

#Probabilities on basisi of model3
#responsemodel3 <- responsemodel3[1:1000]


#loop for calculating accuracies for cut-offs from 0.05 to 0.6
a <-c()
acc <- c()
j = 1
for(i in cutoff){
  a <- ifelse (responsemodel3 > i, 1, 0)
  t <- table(test$`Churn (1 = Yes, 0 = No)`, a)
  acc [j]<- sum(diag(t))/sum(t)
  print(paste0("Accuracy for ", i ," is ", acc[j]))
  j <- j+1
  a <- c()
}




