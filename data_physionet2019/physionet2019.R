#Activity 1: Data prep and Cross Validation with Accuracy
#install.packages(c('tree','ISLR','randomForest', 'e1071'))
library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(Matrix)
library(xgboost)
library("data.table")

files <- list.files("~/Desktop/KI2/2.Current research trend/5.Block 2/4.PhysioNet challenge/trainingA_exp/", pattern=".psv", ignore.case=T)

ff <- function(input){
  data <- fread(input) 
}

a <- lapply(files, ff)
library(plyr) 

data <- ldply(a, function(x) rbind(x, fill = TRUE))
#write.csv(data, file = '~/Desktop/KI2/2.Current research trend/5.Block 2/4.PhysioNet challenge/dataset/raw_data.csv')
data[data == "NaN"] <- NA
#replacing missing values from data va to median
# for (i in 1:ncol(data)){ 
#   data[,i] <- as.numeric(data[,i])
#   data[is.na(data[,i]),i]<- median(data[,i], na.rm =TRUE)
# } 

for (i in 1:ncol(data)){
  print(i)
  #print(table(data[,i]))
  print(table(is.na(data[,i])))
}




data <- subset(data, select = c(1:8,35:41))
# test<-read.csv(file="~/Desktop/KI2/2.Current research trend/5.Block 2/4.PhysioNet challenge/training/p000300.psv",sep = "|") #load test data

for (i in 1:ncol(data)){ 
  data[is.na(data[,i]),i]<- median(data[,i],na.rm =TRUE)} #用中位数替代缺失值
#write.csv(data, file = '~/Desktop/KI2/2.Current research trend/5.Block 2/4.PhysioNet challenge/dataset/proceseed_data.csv')
data$SepsisLabel <- as.factor(data$SepsisLabel)#转换数据类型为因子

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
num_class = length(as.numeric(levels(species)))
params = list(
  booster="gbtree",
  eta=0.001,
  max_depth=5,
  gamma=3,
  subsample=0.75,
  colsample_bytree=1,
  objective="multi:softprob",
  eval_metric="mlogloss",
  num_class=num_class
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
xgb.fit

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
#result = sum(xgb.pred$prediction==xgb.pred$label)/nrow(xgb.pred)

accuracy <- (table[1,1]+table[2,2])/sum(table) #准确率
precision <- (table[2,2])/(table[2,2]+table[2,1]) #精确率
recall <- (table[2,2])/(table[2,2]+table[1,2]) #覆盖率
print(paste("Final Accuracy =",sprintf("%1.2f%%", 100*accuracy)))
print(paste("Final Precision =",sprintf("%1.2f%%", 100*precision)))
print(paste("Final Recall =",sprintf("%1.2f%%", 100*recall)))





###########################################
#10-fold cross validation for svm
set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data打乱原有数据
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE) #用cut()函数设置10个folds

# store our metrics for each model
accuracy <- vector() #设置空的容器
precision <- vector()
recall <- vector()

  #start for loop
for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) #设置1份数据最为测试集
  trainIndexes <- which(folds!=i,arr.ind=TRUE) #设置9份数据最为测试集
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  #Use the test and train data partitions
  set.seed(3)
  svm.model <- svm(data_all.train$SepsisLabel~ ., data =
                     data_all.train, kernel = "polynomial") #参数分别是class label, training data, kernel
  prediction <- predict(svm.model, data_all.test) #参数分别是model, testing data
  #generate the confusion matrix
  table <- table(prediction, data_all.test$SepsisLabel) #参数分别是预测的labels,原本的labels
  #end for loop
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(table)) #准确率
  precision <- c(precision,(table[2,2])/(table[2,2]+table[2,1])) #精确率
  recall <- c(recall,(table[2,2])/(table[2,2]+table[1,2])) #覆盖率
}
table
#accuracy
#precision
#recall
accuracyaverage = mean(accuracy)
precisionaverage = mean(precision)
recallaverage = mean(recall)
accuracyaverage
precisionaverage
recallaverage
##svm kernel = "linear" with accuracyaverage of 0.7636364
#precisionaverage of 0.7664286 #recallaverage of 0.8357143
##svm kernel = "polynomial" with accuracyaverage of 0.7545455
#precisionaverage of 0.7148268 #recallaverage of 0.9888889
##svm kernel = "radial" with accuracyaverage of 0.82
#precisionaverage of 0.7940476 #recallaverage of 0.9277778


####################################
#10-fold cross validation for random forest

# store our metrics for each model
accuracy <- vector()
precision <- vector()
recall <- vector()

#start for loop
for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) #设置1份数据最为测试集
  trainIndexes <- which(folds!=i,arr.ind=TRUE) #设置9份数据最为测试集
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  #Use the test and train data partitions
  set.seed(3)
  rf.data <- randomForest(data_all.train$SepsisLabel~ ., data =
                            data_all.train,ntree=10) ##参数分别是class label, training data, tree的数量
  prediction <- predict(rf.data, data_all.test, type="class")
  #generate the confusion matrix
  table <- table(prediction, data_all.test$SepsisLabel)
  #end for loop
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(table))
  precision <- c(precision,(table[2,2])/(table[2,2]+table[2,1]))
  recall <- c(recall,(table[2,2])/(table[2,2]+table[1,2]))
}
table
accuracy
precision
recall
accuracyaverage = mean(accuracy)
precisionaverage = mean(precision)
recallaverage = mean(recall)
accuracyaverage
precisionaverage
recallaverage
##random forest ntree=2 with accuracyaverage of  0.77
#precisionaverage of 0.8615476  #recallaverage of 0.7044444
##random forest ntree=5 with accuracyaverage of  0.8572727
#precisionaverage of 0.8454365  #recallaverage of 0.9121032
##random forest ntree=10 with accuracyaverage of  0.8672727
#precisionaverage of 0.8568254  #recallaverage of 0.9353175 ##best##


####################################################
##Part B
set.seed(3)
bestmodel.data <- randomForest(data$Cancer~.,data,ntree=10) #设置最好的模型
test$Cancer <- as.factor(test$Cancer) #convert binaryvariables to factors
test$Smoker <- as.factor(test$Smoker)
test$bmi <-  BMI(test$Height,test$Weight)
test$Height <- NULL
test$Weight <- NULL

prediction <- predict(bestmodel.data, test, type="class") #预测全新的数据
#generate the confusion matrix
table <- table(prediction, test$Cancer)
table
