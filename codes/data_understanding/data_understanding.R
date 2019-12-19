#Data exploration of PhysioNet 2019
# This code is for understanding the original dataset from Physionet 2019,
# which consist of setA and setB. We first need to load the data according to
# their path, and then combine all the patients into "data".
# We then explore the relationship between SepsisLable and other variables.
# And then we calculate the distribution of sepsis and nonsepsis patient.
# The output is summaried in a ppt

###### 1. data loading ########
library("data.table")
library(plyr) 
files <- list.files("../../data/original_dataset/training_setA/",
                    full.names=T, pattern=".psv", ignore.case=T) #same with setB
#load the data
ff <- function(input){
  data <- fread(input) 
}
a <- lapply(files, ff)
#combine the data
binded.data <- ldply(a, function(x) rbind(x, fill = TRUE))
data<- binded.data
data[is.na(data)] <- NA





###### 2. data exploration ########
library(tidyverse)
library(caret)
library(GGally)
library(treemap)
library(dplyr)
#summary(data)
ggplot(data, aes(SepsisLabel)) +
  geom_bar()
ggplot(data, aes(ICULOS)) +
  geom_bar()

###### relationship between SepsisLable and ICULOS
p1 <- ggplot(data, aes(x = factor(SepsisLabel), y = ICULOS)) +
  geom_point(alpha = .2)
p2 <- ggplot(data, aes(x = factor(SepsisLabel), y = ICULOS)) +
  geom_jitter(alpha = .2, width = .2)
p3 <- ggplot(data, aes(x = factor(SepsisLabel), y = ICULOS)) +
  geom_boxplot()
p4 <- ggplot(data, aes(x = factor(SepsisLabel), y = ICULOS)) +
  geom_violin()
gridExtra::grid.arrange(p1, p2, p3, p4, nrow = 2)

#### density plot
p1 <- ggplot(data, aes(x = ICULOS, color = factor(SepsisLabel))) +
  geom_freqpoly() 
p2 <- ggplot(data, aes(x = ICULOS, color = factor(SepsisLabel), fill = factor(SepsisLabel))) +
  geom_density(alpha = .15) 
gridExtra::grid.arrange(p1, p2, nrow = 2)

### ggridges
ggplot(data, aes(x = ICULOS, y = factor(SepsisLabel))) + 
  ggridges::geom_density_ridges()

###HospAdmTime & SepsisLabel
ggplot(data, aes(x = HospAdmTime, y = factor(SepsisLabel))) + 
  ggridges::geom_density_ridges() +
  scale_x_continuous(breaks = c(-1000,-500,-200,-100,0,10))
###BaseExcess & SepsisLabel
ggplot(data, aes(x = BaseExcess, y = factor(SepsisLabel))) + 
  ggridges::geom_density_ridges() +
  scale_x_continuous(breaks = c(-25,0,25,50,100))
###pH & SepsisLabel
ggplot(data, aes(x = pH, y = factor(SepsisLabel))) + 
  ggridges::geom_density_ridges() +
  scale_x_continuous(breaks = c(7.0,7.2,7.4,7.6,7.8))
###HCO3 & SepsisLabel
ggplot(data, aes(x = HCO3, y = factor(SepsisLabel))) + 
  ggridges::geom_density_ridges() +
scale_x_continuous(breaks = c(10,20,25,30,40))
###FiO2 & SepsisLabel #######!
ggplot(data, aes(x = FiO2, y = factor(SepsisLabel))) + 
  ggridges::geom_density_ridges() 
###BUN & SepsisLabel #######!
ggplot(data, aes(x = BUN, y = factor(SepsisLabel))) + 
  ggridges::geom_density_ridges() +
scale_x_continuous(breaks = c(0,25,50,75,100,200))
###Chloride & SepsisLabel #######
ggplot(data, aes(x = Chloride, y = factor(SepsisLabel))) + 
  ggridges::geom_density_ridges() #+
  # scale_x_continuous(breaks = c(0,25,50,75,100,200))





######### 3. data description ##########
### missing value
library(naniar)
vis_miss(data[,35:41],warn_large_data=F) #change the columns

# data %>%
#   select(Gender, Unit1, Unit2, SepsisLabel) %>%
#   gather(var, value, Gender:SepsisLabel) %>%
#   ggplot(aes(value)) +
#   geom_bar() +
#   facet_wrap(~ var, scales = "free")


### how many septic patient
# SepticPatient=1790 (8.80%)
checkSepticPatient <- function(){
  septic <- 0
  for (i in 1:20336){
    if (1 %in% a[[i]]$SepsisLabel == TRUE){
      septic = septic+1
    }
  }
  #print(septic)
  return(septic)
}
print(checkSepticPatient())

### SepticObservation = 104964 (13.28%)
checkSepticObservation <- function(){
  septic <- 0
  for (i in 1:20336){
    if (1 %in% a[[i]]$SepsisLabel == TRUE){
      septic = septic+length(a[[i]]$SepsisLabel)
    }
  }
  #print(septic)
  return(septic)
}
print(checkSepticObservation())

### SepticObservationPositive = 17136 (16.33%)
checkSepticObservationPositive <- function(){
  septic <- 0
  for (i in 1:20336){
    if (1 %in% a[[i]]$SepsisLabel == TRUE){
      #print(i)
      septic = septic + (as.data.frame(table(a[[i]]$SepsisLabel)))[((as.data.frame(table(a[[i]]$SepsisLabel)))$Var1 ==1),2]
      #print(septic)
    }
  }
  #print(septic)
  return(septic)
}
print(checkSepticObservationPositive())