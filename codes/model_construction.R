library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(Matrix)
library(xgboost)
library("data.table")
library(adabag)

#load the data
data <-read.csv(file="/Users/leeo/Desktop/KI2/2.Current\ research\ trend/5.Block\ 2/6.github/interpretability-project/processed_data/proceseed_data.csv",sep = ",") 
data <- data[,-1]
data$SepsisLabel <- as.factor(data$SepsisLabel)

######for the rest of the code, we will construct AdaBoost, XGBoost, randomforest and SVM
######and for part B, we will have an external validation for the best model

#####
# Adaboost classifier
set.seed(5)
n = nrow(data)
train.index = sample(n,floor(0.75*n))
train.data = (data[train.index,])
test.data = (data[-train.index,])

ada <- adabag::boosting(SepsisLabel~., data = train.data, mfinal = 20)
prediction_ada <- adabag::predict.boosting(ada, test.data)
res_ada<-prediction_ada$confusion
res_ada
accuracy <- (res_ada[1,1] + res_ada[2,2])/sum(res_ada)
precision <- res_ada[1,1]/(res_ada[1,1]+res_ada[2,1])
recall <- res_ada[1,1]/(res_ada[1,1]+res_ada[1,2])
accuracy
precision
recall


###XGBoost
#Label conversion
species = data$SepsisLabel
label = as.integer(data$SepsisLabel) -1#convert SepsisLabel to label
data$SepsisLabel = NULL
#Split the data for training and testing (75/25 split)
n = nrow(data)
train.index = sample(n,floor(0.75*n))
train.data = as.matrix(data[train.index,])
train.label = label[train.index]
test.data = as.matrix(data[-train.index,])
test.label = label[-train.index]
#Create the xgb.DMatrix objects
# Transform the two data sets into xgb.Matrix
xgb.train = xgb.DMatrix(data=train.data,label=train.label)
xgb.test = xgb.DMatrix(data=test.data,label=test.label)
#Define the main parameters
# Define the parameters for multinomial classification
#num_class = length(as.numeric(levels(species)))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=5
)
#Train the model
# Train the XGBoost classifer
xgb.fit=xgb.train(
  params=params,
  data=xgb.train,
  nrounds=10000,
  nthreads=1,
  early_stopping_rounds=10,
  watchlist=list(val1=xgb.train,val2=xgb.test),
  verbose=0
)
# Review the final model and results
xgb.fit #the classifier

# Predict outcomes with the test data
xgb.pred = predict(xgb.fit,test.data,reshape=T)
xgb.pred = as.data.frame(xgb.pred)
colnames(xgb.pred) = levels(species)

#Identify the class with the highest probability for each prediction
# Use the predicted label with the highest probability
xgb.pred$prediction = apply(xgb.pred,1,function(x) colnames(xgb.pred)[which.max(x)])
xgb.pred$label = levels(species)[test.label+1]

table <- table(xgb.pred$prediction, xgb.pred$label) 
# Calculate the final accuracy
accuracy <- (table[1,1]+table[2,2])/sum(table) 
precision <- (table[2,2])/(table[2,2]+table[2,1]) 
recall <- (table[2,2])/(table[2,2]+table[1,2]) 
print(paste("Final Accuracy of XGBoost =",sprintf("%1.2f%%", 100*accuracy)))
print(paste("Final Precision of XGBoost =",sprintf("%1.2f%%", 100*precision)))
print(paste("Final Recall of XGBoost =",sprintf("%1.2f%%", 100*recall)))
table





########SVM
data <-read.csv(file="/Users/leeo/Desktop/KI2/2.Current\ research\ trend/5.Block\ 2/6.github/interpretability-project/processed_data/proceseed_data.csv",sep = ",") 
data <- data[,-1]
data$SepsisLabel <- as.factor(data$SepsisLabel)
set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE) 

# store our metrics for each model
accuracy <- vector() 
precision <- vector()
recall <- vector()

#start for loop
for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) 
  trainIndexes <- which(folds!=i,arr.ind=TRUE) 
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  #Use the test and train data partitions
  set.seed(3)
  svm.model <- svm(data_all.train$SepsisLabel~ ., data =
                     data_all.train, kernel = "radial") 
  prediction <- predict(svm.model, data_all.test) 
  
  #generate the confusion matrix
  table <- table(prediction, data_all.test$SepsisLabel) 
  #end for loop
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(table)) 
  precision <- c(precision,(table[2,2])/(table[2,2]+table[2,1])) 
  recall <- c(recall,(table[2,2])/(table[2,2]+table[1,2])) 
}
table

accuracyaverage = mean(accuracy)
precisionaverage = mean(precision)
recallaverage = mean(recall)
print(paste("Final Accuracy of SVM =",sprintf("%1.2f%%", 100*accuracyaverage)))
print(paste("Final Precision of SVM =",sprintf("%1.2f%%", 100*precisionaverage)))
print(paste("Final Recall of SVM =",sprintf("%1.2f%%", 100*recallaverage)))





########Random Forest
# store our metrics for each model
data <-read.csv(file="/Users/leeo/Desktop/KI2/2.Current\ research\ trend/5.Block\ 2/6.github/interpretability-project/processed_data/proceseed_data.csv",sep = ",") 
data <- data[,-1]
data$SepsisLabel <- as.factor(data$SepsisLabel)
set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE) 

accuracy <- vector()
precision <- vector()
recall <- vector()

#start for loop
for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) 
  trainIndexes <- which(folds!=i,arr.ind=TRUE) 
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  #Use the test and train data partitions
  set.seed(3)
  rf.data <- randomForest(data_all.train$SepsisLabel~ ., data =
                            data_all.train,ntree=10) 
  prediction <- predict(rf.data, data_all.test, type="class")
  #generate the confusion matrix
  table <- table(prediction, data_all.test$SepsisLabel)
  #end for loop
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(table))
  precision <- c(precision,(table[2,2])/(table[2,2]+table[2,1]))
  recall <- c(recall,(table[2,2])/(table[2,2]+table[1,2]))
}
table

accuracyaverage = mean(accuracy)
precisionaverage = mean(precision)
recallaverage = mean(recall)
print(paste("Final Accuracy of RF =",sprintf("%1.2f%%", 100*accuracyaverage)))
print(paste("Final Precision of RF =",sprintf("%1.2f%%", 100*precisionaverage)))
print(paste("Final Recall of RF =",sprintf("%1.2f%%", 100*recallaverage)))






####################################################
##Part B:External validation: to be continued...
set.seed(3)
bestmodel.data <- randomForest(data$Cancer~.,data,ntree=10) 
test$Cancer <- as.factor(test$Cancer) #convert binaryvariables to factors
test$Smoker <- as.factor(test$Smoker)
test$bmi <-  BMI(test$Height,test$Weight)
test$Height <- NULL
test$Weight <- NULL

prediction <- predict(bestmodel.data, test, type="class") 
#generate the confusion matrix
table <- table(prediction, test$Cancer)
table
