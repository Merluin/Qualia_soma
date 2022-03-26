
###########################################################################
#
#  Experiment:  Qualia_soma
#  Programmer:   QUETTIER THOMAS
#  Date:        01/2022
#  Description: Generate the dataset from psychopy
#
#   
###########################################################################

rm(list=ls()) # remove all objects

# Packages ----------------------------------------------------------------

library(tidyverse)
library(anytime)
library(readr)
library(dplyr)


# Functions ---------------------------------------------------------------
source("05.functions/dataset_concatenation.R")
source("05.functions/keypress.R")
source("05.functions/questionnaires.R")
source("05.functions/stim_amplitude.R")

# loading data ----
datasetname<-"dataset"
dataset_concatenation(datasetname)
load(paste0("04.data_preprocessing/",datasetname,".RData") )


############################## Begin DATASET ##############################


# Subjects General Information---------------------------------------------
 Info<-dataset%>%
   select(participant, session, date, psychopyVersion, frameRate,right,amplitude_5hz,amplitude_31hz,Hz,
          stim, resp.keys,resp.rt, kb_valuation.keys,kb_valuation.rt,
          valence_loop.thisRepN,
          arousal_loop.thisRepN,
          bn_loop_trials.thisN,
          nh_loop_trials.thisN,
          nb_loop_trials.thisN,
          hn_loop_trials.thisN)%>%
   'colnames<-'(c("subject" ,"session","date","psychopy","frameRate" ,"file.rx","amplitude.5hz","amplitude.31hz","Hz",
                  "stimulation","br.keys","br.rt","val.keys","val.rt","valence","arousal","hn","nh","bn","nb"))%>%
    mutate(#amplitude.5hz = mean(amplitude.5hz,na.rm = TRUE),
           #amplitude.31hz = mean(amplitude.31hz,na.rm = TRUE),
           trial = coalesce(hn,nh,bn,nb),
           trial = trial +1,
          hn = ifelse(hn >= 0 , "hn", ''),
          nh = ifelse(nh >= 0 , "nh", ''),
          bn = ifelse(bn >= 0 , "bn", ''),
          nb = ifelse(nb >= 0 , "nb", ''),
          valence = ifelse(valence >= 0 , "valence", ''),
          arousal = ifelse(arousal >= 0 , "arousal", ''),
          block = coalesce(hn,nh,bn,nb,valence,arousal),
          condition= case_when(block == "hn" ~ "emotion",
                               block == "nh" ~ "emotion",
                               block == "bn" ~ "control",
                               block == "nb" ~ "control",
                               block == "valence" ~ "valuation",
                               block == "arousal" ~ "valuation"))%>%
    drop_na(stimulation)%>%
  select(-valence,-arousal,-hn,-nh,-bn,-nb)
  
### Rivalry ---------------------------------------------------------------
# dataset only key resp
key<- Info%>%
  filter(is.na(val.rt))%>%
  select(-val.keys, -val.rt)

keysize<-key$br.keys
keysize<-gsub("[^a-zA-Z]", "", keysize)
keysizemax<-max(nchar(keysize))
keypress(keysizemax) # create RESP, RT and DUR string for column names.


# dataset of key responses
keys <- key%>%
  select(block,br.keys)%>%
  separate(br.keys,RESP,fill = "right", sep = ",")%>%
  data.frame()%>%
  mutate_at(
    vars(all_of(RESP)),
    funs(case_when(. == "k"  ~ "mixed",
                   . == "j" & block == "nb" ~ "neutral",
                   . == "j" & block == "bn" ~ "baffi",
                   . == "j" & block == "nh" ~ "neutral",
                   . == "j" & block == "hn" ~ "happy",
                   . == "l" & block == "nb" ~ "baffi",
                   . == "l" & block == "bn" ~ "neutral",
                   . == "l" & block == "nh" ~ "happy",
                   . == "l" & block == "hn" ~ "neutral")))

# dataset of key responses rt
rt <- key%>%
  select(br.rt)%>%
            separate(br.rt,RT,fill = "right", sep = ",")%>%
  mutate_if(is.character,as.numeric)%>%
  mutate_if(is.numeric, function(x){round(x*1000)}) # covert sec to ms

# dataset of duration 
x <- rt%>%
  mutate_at(vars(all_of(RT)), ~replace(., is.na(.), 30000))%>%
  mutate(last = 30000)

# Keys Calculation
x <- x[,-1] - unlist(x) # unlist() for making difference with different length

# Renaming Columns
colnames(x) <- DUR

# Adding Onset Rivarly culumn 
x <- x %>%
  add_column(rt$KeyPress1RT, .before = TRUE) %>%
  rename("dur_onset" = "rt$KeyPress1RT")
#x$tot <- rowSums(x) # per verifica

# Dataframe 
qualia_riv <- cbind(key%>%
                      select(-br.keys, -br.rt, -block),keys,rt, x)

# Cleaning 
qualia_rt <- qualia_riv %>%
  select( -RESP, -DUR)%>%
  gather(key = keys, value = rt, RT)

qualia_resp <- qualia_riv %>%
  select( -RT, -DUR)%>%
  gather(key = keys, value = resp, RESP)

qualia_dur <- qualia_riv %>%
  select( -RESP, -RT)%>%
  gather(key = keys, value = dur, DUR)

x<-cbind(qualia_resp,qualia_dur$dur,qualia_rt$rt)
qualia_long <- x%>%
  'colnames<-'(c("subject","session","date" ,"psychopy" ,"frameRate" , "file.rx" ,
                 "amplitude.5hz","amplitude.31hz","Hz",
                 "stimulation", "trial" ,"condition","block",  "dur_onset"          
                 , "key","emotion" , "dur","rt"  ))%>%
  mutate(key = parse_number(key),
         rt=as.numeric(rt),
         subject=as.numeric(subject),
         last.key = ifelse(is.na(rt),"yes","no"))%>%
  'row.names<-'(c(1:length(x[,1])))%>%
  arrange(subject,block,condition)

a<-which(qualia_long$last.key=="yes") # identify the trunked (end trial) response
b<-a-1
qualia_long$last.key[b]<-"yes"
xp_resp<-qualia_riv
rivalry_dataset<-qualia_long

### ONSET dataset ----------------------------------------------------------------

ORT<-rivalry_dataset%>%
  filter(key==1)%>%
  select(subject,condition,Hz,rt,emotion)%>%
  'colnames<-'(c("subject","condition","Hz", "onset", "initial_percept"))

z<-qualia_riv%>% # identify max nb of key press before press happy or neutral as IP 
  select(RESP)
z[z!="mixed"]<-"xx"
z<-z[z[,1]=="mixed",]
rep=0
while(!is.na(z[1,1])){
  z<-z[z[,1]=="mixed",]
  z<-z[,-1]
  rep<-rep+1
}
rep<-rep

resp<-rivalry_dataset%>%
  filter(key==1)%>%
  select(emotion,rt)

for(i in 2:rep){ #replace mixed by firrt IP and correct time for ORT
  nextresp<-rivalry_dataset%>%
    filter(key==i)%>%select(emotion,rt)
  
  mix<-which(resp$emotion=="mixed")
  
  resp$emotion[mix]<-nextresp$emotion[mix]
  resp$rt[mix]<- resp$rt[mix]+nextresp$rt[mix]
  
}

onset_dataset<-cbind(ORT[,1:3],resp)%>%
  'colnames<-'(c("subject","condition", "stimulation", "initial_percept", "onset"))%>%
  drop_na(initial_percept)

### Stimuli Valuation--------------------------------------------------------

# Valenza
valuation_dataset <- Info%>%
  filter(condition == "valuation")%>%
  mutate(file.rx = case_when(file.rx == "images/MAM11BFI.png"~ "baffi",
                              file.rx == "images/MAM11HAS.png"~ "happy",
                              file.rx == "images/MAM11NES.png"~ "neutral"),
         val.keys = case_when(block == "valence" & val.keys == 1 ~ -3,
                              block == "valence" & val.keys == 2 ~ -2,
                              block == "valence" & val.keys == 3 ~ -1,
                              block == "valence" & val.keys == 4 ~ 0,
                              block == "valence" & val.keys == 5 ~ 1,
                              block == "valence" & val.keys == 6 ~ 2,
                              block == "valence" & val.keys == 7 ~ 3,
                              block == "arousal" & val.keys == 1 ~ 1,
                              block == "arousal" & val.keys == 2 ~ 2,
                              block == "arousal" & val.keys == 3 ~ 3,
                              block == "arousal" & val.keys == 4 ~ 4,
                              block == "arousal" & val.keys == 5 ~ 5,
                              block == "arousal" & val.keys == 6 ~ 6,
                              block == "arousal" & val.keys == 7 ~ 7))%>%
  arrange(file.rx,Hz)%>%
  group_by(file.rx,subject,Hz,block) %>% #add condition
  summarise_at(vars(val.keys), list(mean))%>%
  spread(block,val.keys)%>%
  select(subject,file.rx,Hz,valence,arousal)%>%
  'colnames<-'(c("subject" ,"emotion","frequency","valence","arousal"))

  


### Questionnaires ---------------------------------------------------------------

subjectorder<-c( "1", "2","3")

#Questionnaires<-questionnaires("04.data_preprocessing/questionnaires.csv",subjectorder)

### Save -------------------------------------------------------------------

save(onset_dataset,
     rivalry_dataset,
     valuation_dataset,
     # Questionnaires,
     xp_resp,
     file = "04.data_preprocessing/qualia_soma.RData")

###########################################################################
#                                   END                                   #
###########################################################################





