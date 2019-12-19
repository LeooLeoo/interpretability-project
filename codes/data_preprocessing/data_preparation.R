# This code is to load all the data from traning set A & B,
# and then merge them into a new .csv file
# the data is the addition of set A and B, without any preprocessing
library(plyr) 
library(data.table)

files <- list.files("../../data/original_dataset/training_setA/",
                    full.names=T, pattern=".psv", ignore.case=T) #same with setB
#load the data set A
ff <- function(input){
  data <- fread(input) 
}
a <- lapply(files, ff)
data_setA <- ldply(a, function(x) rbind(x, fill = TRUE))
data_setA[is.na(data_setA)] <- NA
#load the data set B
files_B <- list.files("../../data/original_dataset/training_setB/",
                    full.names=T, pattern=".psv", ignore.case=T) #same with setB
#load the data set A
b <- lapply(files_B, ff)
data_setB <- ldply(b, function(x) rbind(x, fill = TRUE))
data_setB[is.na(data_setB)] <- NA
#combine A and B
data <- rbind(data_setA, data_setB)

#export the data (the size will be out of github limit)
write.csv(data, file= "../../data/exp_data/complete_dataset.csv",
          row.names= FALSE)
