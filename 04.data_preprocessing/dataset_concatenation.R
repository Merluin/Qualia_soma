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

if(colnames(dataset[1]) == "stim_left" ){dataset <-dataset%>%
  select(-stim_left,-stim_right,-trials_2.thisRepN,-trials_2.thisTrialN,-trials_2.thisN,-trials_2.thisIndex,
         -both_txt_loop.thisRepN,-both_txt_loop.thisTrialN,-both_txt_loop.thisN,-both_txt_loop.thisIndex,
         -both_stim_loop.thisRepN,-both_stim_loop.thisTrialN,-both_stim_loop.thisN,-both_stim_loop.thisIndex,
         -training_trials_loop.thisRepN,-training_trials_loop.thisTrialN,-training_trials_loop.thisN,
         -training_trials_loop.thisIndex,-position.right,-position.left,-txt_right.started,-txt_right.stopped,-txt_left.started,
         -txt_left.stopped,-left.started,-left.stopped,-right.started,-right.stopped,-kb_trial_training.keys,
         -kb_trial_training.rt,-kb_trial_training.started,-kb_trial_training.stopped,-kb_extra_training.keys,
         -kb_extra_training.rt,-kb_extra_training.started,-kb_extra_training.stopped)
}

dataset$resp.keys<-str_remove_all(dataset$resp.keys, "[\\[|\\] ']")
dataset$resp.rt<-str_remove_all(dataset$resp.rt, "[\\[|\\] ']")



for (i in 2:length(effective)){
  data2<-read.csv(paste(folder_dir,"/Pt_",effective[i],".csv",sep = ""), sep=";", header=TRUE,stringsAsFactors = FALSE)
  if(colnames(data2[1]) == "stim_left" ){data2 <-data2%>%
    select(-stim_left,-stim_right,-trials_2.thisRepN,-trials_2.thisTrialN,-trials_2.thisN,-trials_2.thisIndex,
           -both_txt_loop.thisRepN,-both_txt_loop.thisTrialN,-both_txt_loop.thisN,-both_txt_loop.thisIndex,
           -both_stim_loop.thisRepN,-both_stim_loop.thisTrialN,-both_stim_loop.thisN,-both_stim_loop.thisIndex,
           -training_trials_loop.thisRepN,-training_trials_loop.thisTrialN,-training_trials_loop.thisN,
           -training_trials_loop.thisIndex,-position.right,-position.left,-txt_right.started,-txt_right.stopped,-txt_left.started,
           -txt_left.stopped,-left.started,-left.stopped,-right.started,-right.stopped,-kb_trial_training.keys,
           -kb_trial_training.rt,-kb_trial_training.started,-kb_trial_training.stopped,-kb_extra_training.keys,
           -kb_extra_training.rt,-kb_extra_training.started,-kb_extra_training.stopped)
  }
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