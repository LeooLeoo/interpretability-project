#get the files name in the folder
files <- list.files("~/Desktop/KI2/2.Current research trend/5.Block 2/6.github/interpretability-project/data_physionet2019", pattern=".psv", ignore.case=T)

ff <- function(input){
  data <- fread(input) 
}

a <- lapply(files, ff)
library(plyr) 
#read the original data and put them together
data <- ldply(a, function(x) rbind(x, fill = TRUE))
#export the raw_data
#write.csv(data, file = '~/Desktop/KI2/2.Current research trend/5.Block 2/6.github/interpretability-project/raw_data.csv')


data[data == "NaN"] <- NA
#check the missing rate of variables
for (i in 1:ncol(data)){
  print(i)
  #print(table(data[,i]))
  print(table(is.na(data[,i])))
}
#choose the variables whose miss rate is less than 65%
data <- subset(data, select = c(1:7,35:41))

#replacing missing values with median
for (i in 1:ncol(data)){
  data[,i] <- as.numeric(data[,i])
  data[is.na(data[,i]),i]<- median(data[,i], na.rm =TRUE)
}
#export the preproceseed_data
write.csv(data, file = '~/Desktop/KI2/2.Current research trend/5.Block 2/6.github/interpretability-project/preproceseed_data.csv')

