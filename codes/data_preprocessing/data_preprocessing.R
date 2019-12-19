# This code is to preprocess the raw data:
# First we select the included variables
# then we remove the data whose vital signs are missing
# after this, we replace with the missing values of lab tests with MICE algorithm
# Then, we undersampling the data to make it balanced
# The output of this code is a csv file with 13,673 observation and 41 variables
# And the sepsis:non sepsis = 1:1

library(naniar)
library(mice)
library(ROSE)
library(caret)
#load the data(1,552,210 obs 41 variables at first)
data <- read.csv("../../data/exp_data/complete_dataset.csv")

#feature selection
data <- subset(data, select = c(1:7,11,20,27,34:36,41))
summary(data)

#remove the obs with vital sign missings (399,007 obs)
data <- subset(data, !is.na(data$HR)  & 
                 !is.na(data$Temp) &
                 !is.na(data$Resp) &
                 !is.na(data$SBP) &
                 !is.na(data$O2Sat) &
                 !is.na(data$DBP) &
                 !is.na(data$MAP)
                 )
table(data$SepsisLabel)

#deal with imssing values in lab tests
vis_miss(data[,8:11],warn_large_data=F)
#perform imputations for missing data using 'mice' package, 
#returning optimal methods, takes about 10 minutes to compute
mice_imputes = mice(data, m=1, maxit = 1)
#finalising imputed data
data = complete(mice_imputes)
vis_miss(data[,8:11],warn_large_data=F)

#undersample the data (13,673 obs)
table(data$SepsisLabel)
set.seed(3) #need to set seed before undersampling
balanced <- ovun.sample(SepsisLabel~., data = data, method="under")$data
balanced$SepsisLabel <-as.factor(as.numeric(balanced$SepsisLabel))
table(balanced$SepsisLabel)

#export the data
write.csv(balanced, file= "../../data/processed_data/mice_undersample.csv",
                    row.names= FALSE)
