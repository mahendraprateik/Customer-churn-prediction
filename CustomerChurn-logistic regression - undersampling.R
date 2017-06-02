#importing the train0 and train1 data
library(readr)
train0 <- read_csv("D:/Prateik/Quarter 2/STATS/Customer churn - final case study/train0.csv")
train1 <- read_csv("D:/Prateik/Quarter 2/STATS/Customer churn - final case study/train1.csv")



#randomly selecting 600 from train0
n <- nrow(train0)
shuffled_train0 <- train0[sample(n), ]
train0_250 <- shuffled_train0[1:250, ]
test0_250 <- shuffled_train0[251:500, ]
dim(train0_250)
dim(test0_250)

#randomly selecting 200 from test0
p <- nrow(train1)
shuffled_train1 <- train1[sample(p), ]
train1_250 <- shuffled_train1[1:250, ]
test1_55 <- shuffled_train1[250:305, ] 
dim(train1_250)
dim(test1_55)


#combining 600 0s and 200 1s to create final train.
train <- rbind(train0_250, train1_250)

train <- train[, -c(1,4,5,6,7,8,9,10)]

#running logistic regression on train
m1 <- glm(train$`Churn (1 = Yes, 0 = No)`~.,family=binomial(link=logit), data=train)
summary(m1)

#combining 300 0s and 100 1s
test <- rbind(test0_250, test1_55)

#predict
r1 <- predict(m1, newdata=test, type = "response")

a1 <- ifelse(r1>0.6,1,0)
sum(a1)
t1 <- table(test$`Churn (1 = Yes, 0 = No)`,a1)
t1
sum(diag(t1))/sum(t1)


#testChurn_1000 predict
CustomerChurnData_1000=CustomerChurnData_1000[, -c(1,4,5,6,7,8,9,10)]
r2 <- predict(m1, newdata=CustomerChurnData_1000, type = "response")
a2 <- ifelse(r2>0.7,1,0)
sum(a2)



#final
cutoff <- seq(0.1, 0.6,0.05)
a <-c()
acc <- c()
precision <- c()
j = 1
for(i in cutoff){
  a <- ifelse (r1 > i, 1, 0)
  t <- table(test$`Churn (1 = Yes, 0 = No)`, a)
  #precision[j] <- (t[2,2])/(t[2,2] + t[1,2])
  acc [j]<- sum(diag(t))/sum(t)
  print(paste0("Accuracy for ", i ," is ", acc[j]))
  #print(paste0("the precision for ", i, " is ", precision[j]))
  j <- j+1
  a <- c()
  precision <- c()
}
a <- ifelse (r1 > 0.41, 1, 0)
> t <- table(test$`Churn (1 = Yes, 0 = No)`, a)


