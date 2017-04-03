setwd("D:/Deepti/R")
rm(list = ls())

library(mlbench)
library(caret)
library(e1071)
#library(help = "mlbench")

#??HouseVotes84

data(HouseVotes84)
housevote = HouseVotes84
names(housevote)
head(housevote)

#checking count of unique values in all column
sapply(housevote,function(x)length(unique(x)))
housevote1 <- sapply(housevote, as.character) # since your values are `factor`

#checking NA and convering them to 0
housevote1[is.na(housevote1)] <- 0
head(housevote1)

#converting to dataframe
housevote1 = as.data.frame(housevote1)

# dividing train and test data 70% and 30% respectively
hvindex <- sample(1:nrow(housevote1),size=nrow(housevote1)*.7)
trainhv <- housevote1[hvindex,]
testhv = housevote1[-hvindex,]

#creating model
hv_model = naiveBayes(Class ~ ., data = trainhv) 

# predict the outcome of the test data
hvtest_pred = predict(hv_model, testhv[,-c(1)]) 
hvoutput = testhv
hvoutput$pred = hvtest_pred
write.csv(hvoutput,"hvtest_pred.csv")

#predict the output of train to check train accuracy
hvtrain_pred = predict(hv_model, trainhv[,-c(1)])

#creating confusion matrix to check accuracy
confusionMatrix(hvtest_pred,testhv[,1])
confusionMatrix(hvtrain_pred,trainhv[,1])
