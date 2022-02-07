#################################################
# 
# Name:           Psychopy dataset
# Programmer:     Thomas Quettier
# Date:           18/11/2021
# Description:    Put together several csv files
#
#################################################


# Library & dataset
rm(list=ls()) # remove all objects
# neaded packages
library(dplyr)
library(tidyverse)

# find nb participant
folder_dir<-c("03.original_data")
nbsub<-sapply(folder_dir,function(dir){length(list.files(dir,pattern='csv'))})
effective<-c(1:nbsub)

# loading data ----
dataset <- read.csv(paste(folder_dir,"/Pt_1.csv",sep = ""), sep=";", header=TRUE,stringsAsFactors = FALSE)
dataset$resp.keys<-str_remove_all(dataset$resp.keys, "[\\[|\\] ']")
dataset$resp.rt<-str_remove_all(dataset$resp.rt, "[\\[|\\] ']")



for (i in 2:length(effective)){
  data2<-read.csv(paste(folder_dir,"/Pt_",effective[i],".csv",sep = ""), sep=";", header=TRUE,stringsAsFactors = FALSE)
  
  data2$resp.keys<-str_remove_all(data2$resp.keys, "[\\[|\\] ']")
  data2$resp.rt<-str_remove_all(data2$resp.rt, "[\\[|\\] ']")
  dataset<-rbind(dataset,data2)
}

#write.csv2(dataset, file = "03.original_data/Pt_.csv")
save(dataset,file="04.data_preprocessing/dataset.RData")

#################################################
# 
# END
#
#################################################