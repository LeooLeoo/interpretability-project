data_A<- readRDS("./data_A.RDS")
data_B<- readRDS("./data_B.RDS")
data <- rbind(data_A, data_B)


datall <- subset(data, select = c(1:7,11,20,27,34:36,39:41))
summary(datall)

data <- datall

data <- subset(data, !is.na(data$HR))
data <- subset(data, !is.na(data$Temp))
summary(data)
data <- subset(data, !is.na(data$Resp))
data <- subset(data, !is.na(data$SBP))
summary(data)
data <- subset(data, !is.na(data$O2Sat))
summary(data)
data <- subset(data, !is.na(data$DBP))
summary(data)
data <- subset(data, !is.na(data$MAP))
summary(data)


library(naniar)
vis_miss(data[,8:11],warn_large_data=F)


library(mice)
library(ROSE)
library(caret)

#perform imputations for missing data using 'mice' package, returning optimal methods 
mice_imputes = mice(data, m=1, maxit = 1)

#finalising imputed data
Imputed_data = complete(mice_imputes)

data <- Imputed_data

table(data$SepsisLabel)

balanced <- ovun.sample(SepsisLabel~., data = data, method="under")$data

table(balanced$SepsisLabel)
#write.csv(data,file= "./mice_under.csv")


set.seed(3)
train = sample(nrow(balanced), nrow(balanced)*4/5)
test = -train

data_test=balanced[test,]
data_train=balanced[train,]

set.seed(3)
rf.data <-randomForest(data_train$SepsisLabel~.,data_train,ntree=500)

prediction_rf <- predict(rf.data, data_test, type = 'class')

cm <- confusionMatrix(prediction_rf, data_test$SepsisLabel, mode = "everything")
cm
