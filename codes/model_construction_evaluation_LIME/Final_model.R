# This code is to find the suitable cases for LIME evaluation,
# and also implement the LIME algorithm with the chosen ML model.
# The output is the visualization explanation of LIME

library(tree)
library(ISLR)
library(randomForest)
library(lime)

#the data is fixed here
train <- read.csv(file="../../data/processed_data/deployed_data/final_model_train.csv") 
test <- read.csv(file="../../data/processed_data/deployed_data/final_model_test.csv")
train$X = NULL
test$X = NULL
#lapply(train,class)
train$Gender <- as.factor(train$Gender)
test$Gender <- as.factor(test$Gender)


#Train the model
set.seed(3)
rf.data <-randomForest(train$SepsisLabel~.,train,ntree=300)
prediction_rf <- predict(rf.data, test, type = 'class')

#Find test cases from test set.
x <-as.data.frame(predict(rf.data, test, type = 'prob'))

#find the case with probability more than 0.8
ns <- subset(x, x[,2] >0.8)
ns0 <- subset(x,x[,1]>0.8)

# observation1 <- sample(nrow(ns), 5, replace = FALSE)
# observation2 <- sample(nrow(ns0), 5, replace = FALSE)
# observation <- rbind(observation1,observation2)
observation <- test[c(672,768,1048,1079,1259,345,842,1036,1104,1171),]


#LIME implementation
expln <- lime(train, as_classifier(rf.data), n_bins = 4)
explanation <- explain(x = observation[,names(observation)!="SepsisLabel"], #can specify the case here
                       expln,
                       n_labels = 1,
                       n_features = 5,
                       feature_select = "highest_weights"
                       
)
plot_features(explanation)
