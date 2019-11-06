setwd("~/OneDrive/Health Informatics/9. Current research and trends in health informatics/Project Sepsis/training")
install.packages("basefun")
library(basefun)
install.packages("variables")
library(variables)
install.packages("data.table")
library(data.table)
files <- list.files("~/OneDrive/Health Informatics/9. Current research and trends in health informatics/Project Sepsis/training", pattern=".psv", ignore.case=T)

ff <- function(input){
  data <- fread(input) 
}

a <- lapply(files, ff)

install.packages("plyr")
library(plyr) 
#read the original data and put them together
data <- ldply(a, function(x) rbind(x, fill = TRUE))
#export the raw_data
#write.csv(data, file = '~/Desktop/KI2/2.Current research trend/5.Block 2/6.github/interpretability-project/raw_data.csv')

data[data == "NaN"] <- NA
summary(data)

data$Gender <- as.factor(data$Gender)
data$SepsisLabel <- as.character(data$SepsisLabel)
data$SepsisLabel <- factor(x=data$SepsisLabel, levels = c("0", "1"), labels = c("non septic", "septic"))

summary(data)

#saveRDS(data,file="data_A.RDS")
########### delet observations with NA #############

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

############### extract labs + class lable ###########
lab <- subset(data, select = c(8:34,41))
summary(lab)

########## subset vital signs + demographics ##################
dat <- subset(data, select = c(1:7,35:41))
summary(dat)
########## subset vital signs + demographics minus unit and time ##################
dat <- subset(dat, select = c(1:9,14))
summary(dat)


############## data B ##############
setwd("~/OneDrive/Health Informatics/9. Current research and trends in health informatics/Project Sepsis/trainingB")

filesAB <- list.files("~/OneDrive/Health Informatics/9. Current research and trends in health informatics/Project Sepsis/trainingB", pattern=".psv", ignore.case=T)

install.packages("basefun")
library(basefun)
install.packages("variables")
library(variables)
install.packages("data.table")
library(data.table)

ffB <- function(input){
  dataAB <- fread(input) 
}

ab <- lapply(filesAB, ffB)

install.packages("plyr")
library(plyr) 
#read the original data and put them together
dataAB <- ldply(ab, function(x) rbind(x, fill = TRUE))
#export the raw_data
#write.csv(data, file = '~/Desktop/KI2/2.Current research trend/5.Block 2/6.github/interpretability-project/raw_data.csv')

dataAB[dataAB == "NaN"] <- NA

summary(dataAB)
dataAB$Gender <- as.factor(dataAB$Gender)
dataAB$SepsisLabel <- as.character(dataAB$SepsisLabel)
dataAB$SepsisLabel <- factor(x=dataAB$SepsisLabel, levels = c("0", "1"), labels = c("non septic", "septic"))

summary(dataAB)

#saveRDS(dataAB,file="data_B.RDS")

##################  delete NA's #####################

dataAB <- subset(dataAB, !is.na(dataAB$HR))
dataAB <- subset(dataAB, !is.na(dataAB$Temp))
summary(dataAB)
dataAB <- subset(dataAB, !is.na(dataAB$Resp))
dataAB <- subset(dataAB, !is.na(dataAB$SBP))
summary(dataAB)
dataAB <- subset(dataAB, !is.na(dataAB$O2Sat))
summary(dataAB)
dataAB <- subset(dataAB, !is.na(dataAB$DBP))
summary(dataAB)
dataAB <- subset(dataAB, !is.na(dataAB$MAP))
summary(dataAB)

############ subset for labs #############

labAB <- subset(dataAB, select = c(8:34,41))
summary(labAB)

############## subset for vital signs + demographics ###############

datAB <- subset(dataAB, select = c(1:7,35:36,41))
summary(datAB)

########### bind the data for vital signs + demographics into one  #############
library(dplyr)

allData <- bind_rows(dat,datAB)

########### bind the data for labs into one #############
allLab <- bind_rows(lab,labAB)

############### class distribution #################

barplot(prop.table(table(allData$SepsisLabel)), 
        col = rainbow(5), ylim = c(0,0.7), main = "Class distibution")

xa <- subset(allData, allData$SepsisLabel=="septic") ## to understand the number of "septic" observations

#######   ROSE - randomly over sampling examples to get balance in the class lable #######
install.packages("ROSE")
library(ROSE)

allDataB <- ovun.sample(SepsisLabel~., allData, method="under",N=13676, subset=options("subset")$subset, na.action=options("na.action")$na.action, seed=135)
allDataB <- allDataB$data

barplot(prop.table(table(allDataB$SepsisLabel)), 
        col = rainbow(5), ylim = c(0,0.7), main = "Class distibution")
summary(allDataB) #### all data balansed by class

########## preprocess labs without imputation #########
####### subset SOFA variables + class lable
allLab <- subset(allLab, select = c(4,13:14,27:28))
summary(allLab)
########### delete all NA's
l <- na.omit(allLab)
summary(l)

########### balance lab data by class lables 
l <- ovun.sample(SepsisLabel~., l, method="over",N=338, subset=options("subset")$subset, na.action=options("na.action")$na.action, seed=135)
l <- l$data
summary(l)

#########  increase the nr of rows in the balanced data, each row repeats 41 times, N=13858 #########

l <- l[rep(1:nrow(l),each=41),]
summary(l)

############  subset according to the classes ###############

l0 <- subset(l, SepsisLabel=="non septic")
l1 <- subset(l, SepsisLabel=="septic")
summary(l0)
summary(l1)

########## balance the nr of row with the allDataB "vital signs + demographics" N=13676 ##############

l0<- l0[-c(1:91),] 
summary(l0)
l1<- l1[-c(1:91),] 
summary(l1)

############# separate allDataB into 2, acorrding to the class lable ###########

al0 <- subset(allDataB, SepsisLabel=="non septic")
al1 <- subset(allDataB, SepsisLabel=="septic")

########## merge data class 0 + labs class 0  ###########
allab0 <- bind_cols(al0[,-10],l0)

########## merge data class 1 + labs class 1   ###########

allab1 <-  bind_cols(al1[,-10],l1)

############  merge all data into one data set ################

datall <- bind_rows(allab0,allab1)

barplot(prop.table(table(datall$SepsisLabel)), 
        col = rainbow(5), ylim = c(0,0.9), main = "Class distibution")

##saveRDS(datall,file="data_final14v.RDS")

##################### cut data into train and test 70/30%

#barplot(prop.table(table(datall$SepsisLabel)), 
        #col = rainbow(5), ylim = c(0,0.9), main = "Class distibution")

set.seed(135)
ind <- sample(2, nrow(datall), replace = TRUE, prob = c(.7,.3))

train <-datall[ind==1,]
test <- datall[ind==2,]

#saveRDS(train,file="train.RDS")
#saveRDS(test,file="test.RDS")

barplot(prop.table(table(train$SepsisLabel)), 
        col = rainbow(5), ylim = c(0,0.8), main = "Class distibution train")

barplot(prop.table(table(test$SepsisLabel)), 
        col = rainbow(5), ylim = c(0,0.8), main = "Class distibution test")

#####  Random Forest (package 'randomForest') num.trees=500    ##### + ##### CROSS VALIDATION ######

library(e1071)
library(corrplot)
library(ggplot2)
library(caret)
library(lattice)
library(naivebayes)
library(klaR)
library(MASS)
library(ranger)
library(ROCR)
library(pROC)
library(randomForest)


seed=123
model<- train(SepsisLabel~., train, method = "rf", trControl = trainControl(method = "cv", number = 10, verboseIter = TRUE))
predictRF <- predict(model,test[,-14])
tab <- table(predictRF, test$SepsisLabel)
tab
cm <- confusionMatrix(predictRF, test$SepsisLabel, mode = "everything")
cm
roc<- multiclass.roc(as.numeric(test$SepsisLabel), as.numeric(predictRF))
cm
roc$auc

############ LIME ###############
install.packages("lime")
library(lime)


testTEST0 <- subset(test, SepsisLabel=="non septic")
testTEST1 <- subset(test, SepsisLabel=="septic")
z <- subset(testTEST0[578,])
o <- subset(testTEST1[105,])
testTEST <- rbind(z,o)


# Create an explainer object
explainer <- lime(train, model)
#explainer$model$modelInfo$parameters

# Explain new observation
explanation <- explain(testTEST, explainer, n_labels = 1, n_features = 13)
explanation
# And can be visualised directly
plot_features(explanation)





