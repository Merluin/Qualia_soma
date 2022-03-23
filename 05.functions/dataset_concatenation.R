#################################################
# 
# Name:           Psychopy dataset
# Programmer:     Thomas Quettier
# Date:           18/11/2021
# Description:    Put together several csv files
#
#################################################
dataset_concatenation()<- function(name)
{
  
# clean environment
rm(list=ls()) 
# packages
library(dplyr)
library(tidyverse)
source("05.functions/clean_practice.R")

# find nb of file
folder_dir<-c("03.original_data/")
foldername<-list.files(folder_dir,pattern='csv')
nbsub<-sapply(folder_dir,function(folder_dir){length(foldername)})
effective<-c(1:nbsub)

# loading file 1 ----
dataset <- read.csv(paste0(folder_dir,foldername[1]), sep=";", header=TRUE,stringsAsFactors = FALSE)

# clean data if practice columns
dataset<-clean_practice(dataset)

# clean cells from extra characters like [] 
dataset$resp.keys<-str_remove_all(dataset$resp.keys, "[\\[|\\] ']")
dataset$resp.rt<-str_remove_all(dataset$resp.rt, "[\\[|\\] ']")


# loading next files ----
for (i in 2:length(effective)){
  temp<-read.csv(paste0(folder_dir,foldername[i]), sep=";", header=TRUE,stringsAsFactors = FALSE)
  
  # clean data if practice columns
  temp<-clean_practice(temp)
  # clean cells from extra characters like [] 
  temp$resp.keys<-str_remove_all(temp$resp.keys, "[\\[|\\] ']")
  temp$resp.rt<-str_remove_all(temp$resp.rt, "[\\[|\\] ']")
  
  #concatenate all file  
  dataset<-rbind(dataset,temp)
}

#write.csv2(dataset, file = "03.original_data/Pt_.csv")
save(dataset,file=paste0("04.data_preprocessing/",name,".RData"))


} #end function  

#################################################
# 
# END
#
#################################################