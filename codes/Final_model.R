data <- read.csv(file="imputed_balanced_labs.csv")
data$X = NULL
data$HospAdmTime = NULL
data$ICULOS = NULL
data$Gender <- as.factor(data$Gender)

library(tree)
library(ISLR)
library(randomForest)
library(e1071)
library(caret)
library(lime)


set.seed(3)
train = sample(nrow(data), nrow(data)*2/3)
test = -train

data_test=data[test,]
data_train=data[train,]

set.seed(3)
rf.data <-randomForest(data_train$SepsisLabel~.,data_train,ntree=500)

prediction_rf <- predict(rf.data, data_test, type = 'class')

cm <- confusionMatrix(prediction_rf, data_test$SepsisLabel, mode = "everything")
cm

#Define observations

observation <- data[c(5,28,163,226,360,7851,10676,7881,10581,10559),]
observation <- observation[sample(nrow(observation)),]

observation2 <- data[c(10,3688,3825,4569,28,7851,10011,9967,10027,10149),]
observation2 <- observation2[sample(nrow(observation2)),]

prediction_rf_test <- predict(rf.data, observation, type = 'class')
prediction_rf_test2 <- predict(rf.data, observation2, type = 'class')

cm <- confusionMatrix(prediction_rf_test, observation$SepsisLabel, mode = "everything")
cm

cm <- confusionMatrix(prediction_rf_test2, observation2$SepsisLabel, mode = "everything")
cm

#LIME
expln <- lime(data_train, as_classifier(rf.data), n_bins = 4)

explanation <- explain(x = observation[,names(observation)!="SepsisLabel"],
                       expln,
                       n_labels = 1,
                       n_features = 4,
                       feature_select = "highest_weights"
                       
)

plot_features(explanation)