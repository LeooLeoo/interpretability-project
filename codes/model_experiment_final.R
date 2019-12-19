##load the data
data <-read.csv("~/Desktop/KI2/2.Current research trend/5.Block 2-project/6.github/interpretability-LIME/interpretability-project/processed_data/imputed_balanced_labs.csv")[,c(2:14,17)]
data$Gender<-as.factor(data$Gender)




######## 10-fold Random forest ###########
library(randomForest)
library(caret)


#10-fold cross validation for rf
set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE) 

# store our metrics for each model
accuracy <- vector()
precision <- vector()
recall <- vector()
fscore <- vector()

#start for loop
for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) #we holdout one fold for testing
  trainIndexes <- which(folds!=i,arr.ind=TRUE) #we train on the other folds
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  #Use the test and train data partitions
  set.seed(3)
  rf.data <- randomForest(data_all.train$SepsisLabel~ ., data =
                            data_all.train,ntree=300) #20, 50, 100, 300, 500
  prediction <- predict(rf.data, data_all.test, type="class")
  #generate the confusion matrix
  table <- table(prediction, data_all.test$SepsisLabel)
  #end for loop
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(table))
  precision <- c(precision,(table[2,2])/(table[2,2]+table[2,1]))
  recall <- c(recall,(table[2,2])/(table[2,2]+table[1,2]))
  fscore <- c(fscore,(2*table[2,2])/(2*table[2,2]+table[2,1]+table[1,2]))
}
accuracyaverage = mean(accuracy)
precisionaverage = mean(precision)
recallaverage = mean(recall)
fscoreaverage = mean(fscore)
accuracyaverage
precisionaverage
recallaverage
fscoreaverage
print(fscore)




######## 10-fold Support Vector Machine ###########
library(e1071)

# store our metrics for each model
accuracy <- vector()
precision <- vector()
recall <- vector()
fscore <- vector()

#start for loop
for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) #we holdout one fold for testing
  trainIndexes <- which(folds!=i,arr.ind=TRUE) #we train on the other folds
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  #Use the test and train data partitions
  set.seed(3)
  svm.model <- svm(data_all.train$SepsisLabel~ ., data =
                     data_all.train, kernel = "polynomial") #linear, radial, polynomial
  prediction <- predict(svm.model, data_all.test, type="class")
  #generate the confusion matrix
  table <- table(prediction, data_all.test$SepsisLabel)
  #end for loop
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(table))
  precision <- c(precision,(table[2,2])/(table[2,2]+table[2,1]))
  recall <- c(recall,(table[2,2])/(table[2,2]+table[1,2]))
  fscore <- c(fscore,(2*table[2,2])/(2*table[2,2]+table[2,1]+table[1,2]))
}
accuracyaverage = mean(accuracy)
precisionaverage = mean(precision)
recallaverage = mean(recall)
fscoreaverage = mean(fscore)
accuracyaverage
precisionaverage
recallaverage
fscoreaverage
print(fscore)





######## 10-fold AdaBoost ###########
library(adabag)

# store our metrics for each model
accuracy <- vector()
precision <- vector()
recall <- vector()
fscore <- vector()

#start for loop
for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) #we holdout one fold for testing
  trainIndexes <- which(folds!=i,arr.ind=TRUE) #we train on the other folds
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  #Use the test and train data partitions
  set.seed(3)
  ada <- adabag::boosting(SepsisLabel~., data = data_all.train, mfinal = 20)#20,50,100
  prediction <- adabag::predict.boosting(ada, data_all.test)
  #generate the confusion matrix
 
  table <-  prediction$confusion
  #end for loop
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(table))
  precision <- c(precision,(table[2,2])/(table[2,2]+table[2,1]))
  recall <- c(recall,(table[2,2])/(table[2,2]+table[1,2]))
  fscore <- c(fscore,(2*table[2,2])/(2*table[2,2]+table[2,1]+table[1,2]))
}
accuracyaverage = mean(accuracy)
precisionaverage = mean(precision)
recallaverage = mean(recall)
fscoreaverage = mean(fscore)
accuracyaverage
precisionaverage
recallaverage
fscoreaverage
print(fscore)



###### export the data #########
set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE) 
i = 9

testIndexes <- which(folds==i,arr.ind=TRUE) #we holdout one fold for testing
trainIndexes <- which(folds!=i,arr.ind=TRUE) #we train on the other folds
data_all.test <- datarandom[testIndexes, ]
data_all.train <- datarandom[trainIndexes, ]
#export the data
write.csv(data_all.train,file = "~/Desktop/KI2/2.Current research trend/5.Block 2-project/6.github/interpretability-LIME/interpretability-project/processed_data/final_model_train.csv")
write.csv(data_all.test,file = "~/Desktop/KI2/2.Current research trend/5.Block 2-project/6.github/interpretability-LIME/interpretability-project/processed_data/final_model_test.csv")


#### performance of the "best" model ######
set.seed(3)
rf.data <- randomForest(data_all.train$SepsisLabel~ ., data =
                          data_all.train,ntree=300) #20, 50, 100, 300, 500
prediction <- predict(rf.data, data_all.test, type="class")
#generate the confusion matrix
table <- table(prediction, data_all.test$SepsisLabel)
accuracy <- (table[1,1]+table[2,2])/sum(table)
precision <- (table[2,2])/(table[2,2]+table[2,1])
recall <- (table[2,2])/(table[2,2]+table[1,2])
fscore <- (2*table[2,2])/(2*table[2,2]+table[2,1]+table[1,2])
accuracy
precision
recall
fscore