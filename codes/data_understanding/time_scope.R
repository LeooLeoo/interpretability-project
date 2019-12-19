# This code is to explore the data according to a 6-hour time scope
# The logic is to first find the index of septic patients, and put them into a list.
# Then find the index of the label=1 first appear, and then export the observations before it
# Same with the non septic patients. Except that, random select same amount of patients.
# Then combine this two as the balanced time scope dataset for random forest classifier
# The accuracy is 99.55%, which is too high

###### data loading ########
library("data.table")
files <- list.files("../../data/original_dataset/training_setA/",
                    full.names=T, pattern=".psv", ignore.case=T)
ff <- function(input){
  data <- fread(input) 
}
#load the data
a <- lapply(files, ff)



######## septic data ########
library(plyr) 
library(naniar)
# Find the septic patients' index
findSepticPatient <- function(){
  septic <- c()
  for (i in 1:20336){
    if (1 %in% a[[i]]$SepsisLabel == TRUE){
      septic <- c(septic,i)
    }
  }
  #print(septic)
  return(septic)
}
#select the septic patients  in a list
septicSubset<- findSepticPatient()
septicPatientSet <-lapply(septicSubset, function(x) a[[x]])

#select the observations 6 hour before septic status
septicObservationSet <-lapply(1:length(septicPatientSet), function(x) if (min(which(septicPatientSet[[x]]$SepsisLabel>0))>6) {
  septicPatientSet[[x]][(min(which(septicPatientSet[[x]]$SepsisLabel>0))-6):(min(which(septicPatientSet[[x]]$SepsisLabel>0))-1),] })
# index<- min(which(septicPatientSet[[7]]$SepsisLabel>0))
# septicObservation<- septicPatientSet[[7]][(index-6):(index-1),]
#remove the null(sepsis label is less than 6 hours):
septicObservationSet<- septicObservationSet[-which(sapply(septicObservationSet, is.null))]

#doesn't work with mice packages
data <- lapply(1:length(septicObservationSet), function(x) as.data.frame(septicObservationSet[[x]][,c(1:7,11,20,27,34:36,39:41)]))
#replace with median
missing <- function(data){
  for(x in 1:ncol(data)){
    data[is.na(data[,x]), x] <- median(data[,x], na.rm = TRUE)
  }
  return(data)
}
for (i in 1:length(data)){
  data[[i]]<-missing(data[[i]])
}

#combine the data
septic_obs_data <- ldply(data, function(x) rbind(x))
#export the septic data
write.csv(septic_obs_data, file= "../../data/exp_data/time_scope_exp/septic_obs_6h.csv",
          row.names= FALSE)
#replace the missing again
for(x in 1:ncol(septic_obs_data)){
  septic_obs_data[is.na(septic_obs_data[,x]), x] <- median(septic_obs_data[,x], na.rm = TRUE)
}
#relabel the class
septic_obs_data$SepsisLabel <- 1
#export the septic data again
write.csv(septic_obs_data, file= "../../data/exp_data/time_scope_exp/septic_obs_6h_complete.csv",
          row.names= FALSE)
#check the missing values
vis_miss(septic_obs_data,warn_large_data=F)




########## non-septic data ###########
# Find the non-septic patients' index
findNonSepticPatient <- function(){
  nonseptic <- c()
  for (i in 1:20336){
    if (1 %in% a[[i]]$SepsisLabel == F){
      nonseptic <- c(nonseptic,i)
    }
  }
  #print(septic)
  return(nonseptic)
}
#select the septic patients  in a list
nonSepticSubset<- findNonSepticPatient()
nonSepticPatientSet <-lapply(nonSepticSubset, function(x) as.data.frame(a[[x]][,c(1:7,11,20,27,34:36,39:41)]))

###randomly select 1384 negative patients
set.seed(3)
train = sample(length(nonSepticPatientSet), 1384)
nonSepticSampleSet <-lapply(train, function(x) nonSepticPatientSet[[x]])

####replace with median
for (i in 1:length(nonSepticSampleSet)){
  nonSepticSampleSet[[i]]<-missing(nonSepticSampleSet[[i]])
}

#combine the data
nonSeptic_obs_data <- ldply(nonSepticSampleSet, function(x) rbind(x))
###randomly select 8304 negative observations
set.seed(3)
train = sample(nrow(nonSeptic_obs_data), 8304)
nonSeptic_obs_data <-nonSeptic_obs_data[train,]
#export the septic data
write.csv(nonSeptic_obs_data, file= "../../data/exp_data/time_scope_exp/nonSeptic_obs_6h.csv",
          row.names= FALSE)
#replace the missing again
for(x in 1:ncol(nonSeptic_obs_data)){
  nonSeptic_obs_data[is.na(nonSeptic_obs_data[,x]), x] <- median(nonSeptic_obs_data[,x], na.rm = TRUE)
}
#export the septic data again
write.csv(nonSeptic_obs_data, file= "../../data/exp_data/time_scope_exp/nonSeptic_obs_6h_complete.csv",
          row.names= FALSE)

#combine the positive and negative data
binded_balanced_data <- rbind(septic_obs_data,nonSeptic_obs_data)
binded_balanced_data$SepsisLabel<- as.factor(binded_balanced_data$SepsisLabel)
#export the balanced complete data again
write.csv(binded_balanced_data, file= "../../data/exp_data/time_scope_exp/balanced_obs_6h_complete.csv",
          row.names= FALSE)





######## 10-fold Random forest ###########
library(randomForest)
library(caret)
data <- binded_balanced_data

#10-fold cross validation for rf
set.seed(3)
datarandom<-data[sample(nrow(data)),] #shuffle the data
folds <- cut(seq(1,nrow(data)),breaks=10,labels=FALSE) 

# store our metrics for each model
accuracy <- vector()
precision <- vector()
recall <- vector()

#start for loop
for (i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE) #we holdout one fold for testing
  trainIndexes <- which(folds!=i,arr.ind=TRUE) #we train on the other folds
  data_all.test <- datarandom[testIndexes, ]
  data_all.train <- datarandom[trainIndexes, ]
  #Use the test and train data partitions
  set.seed(3)
  rf.data <- randomForest(data_all.train$SepsisLabel~ ., data =
                            data_all.train,ntree=20) 
  prediction <- predict(rf.data, data_all.test, type="class")
  #generate the confusion matrix
  table <- table(prediction, data_all.test$SepsisLabel)
  #end for loop
  accuracy <- c(accuracy,(table[1,1]+table[2,2])/sum(table))
  precision <- c(precision,(table[2,2])/(table[2,2]+table[2,1]))
  recall <- c(recall,(table[2,2])/(table[2,2]+table[1,2]))
}
accuracyaverage = mean(accuracy)
precisionaverage = mean(precision)
recallaverage = mean(recall)
accuracyaverage
precisionaverage
recallaverage

# holdout data: confusion matrix
#               Reference
# Prediction         0    1
#               0 1618    8
#               1    7 1689

