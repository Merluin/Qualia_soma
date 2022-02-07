###########################################################################
###########################################################################
#
#  Experiment:  Qualiamotion
#  Programmer:  GAMBAROTTA Filippo, QUETTIER THOMAS
#  Date:        07/2019
#  Description: Responces from E-PRIME
#  Notes : A first step is to merge subject by E-Merge, then open the file and remove trial columns and export in .XLS. The next step is to open the E-merge file with Ecxel and export in .CSV.
#   
###########################################################################
#                              SCRIPT QUALIA                              #
###########################################################################
# Workspace ---------------------------------------------------------------

rm(list=ls())

if (!require(rstudioapi)) {
  install.packages("rstudioapi")
}
wd <- getActiveDocumentContext()$path
setwd(dirname(wd))

rm(list=ls())

# Packages ----------------------------------------------------------------

library(tidyverse)
library(anytime)


# Functions ---------------------------------------------------------------


source("Functions/keypress.R")


# Importing Data ----------------------------------------------------------
load("Data/qualia.RData")

qualia <- read.csv("Data/qualia.csv", sep = ";")
qualia <- xp

# Selecting Subjects ------------------------------------------------------

#subj <-  # Female to delete

qualia<-qualia %>%
  filter(ID > 0)

# Info Sub----
 Info<-qualia[seq(1,length(qualia[,1]),128),] %>%
  select(Subject,ID,Sex,Handedness, Chopstick, Session,FeliceNeutro.Session., Lenti, SessionDate) %>%
  mutate(Session = case_when(Chopstick=="YES"  ~ "free",
                             Chopstick=="NO"  ~ "blocked")) %>%
  mutate(Chopstick = case_when(Chopstick=="YES"  ~ "blocked",
                               Chopstick=="NO"  ~ "free")) %>%
  mutate(FeliceNeutro.Session. = case_when(FeliceNeutro.Session.=="F"  ~ "happy",
                                           FeliceNeutro.Session.=="N"  ~ "neutral")) %>%
  mutate(Lenti = case_when(Lenti=="VR"  ~ "green",
                           Lenti=="RV"  ~ "red"))

#%>%
  #mutate(SessionDate=gsub('NA', '',paste0(anydate(SessionDate[!is.na(SessionDate)]),as.Date(SessionDate[!is.na(SessionDate)],'%m/%d/%y'))))
# Splitting Dataframe -----------------------------------------------------

### Valenza
  
qualia_valenza <- qualia %>%
  filter(Procedure.Trial. == "ValenzaTrialproc") %>%
  select(Subject,ID,Remove,Chopstick,TAG,TAG1, COLORE1, ValImage.RESP)

### Arousal

qualia_arousal <- qualia %>%
  filter(Procedure.Trial. == "ArousalTrialproc") %>%
  select(Subject,ID,Remove,Chopstick,TAG,TAG1, COLORE1, ArouImage.RESP)

### Rivalry

#### Keypress Function         

keypress(29) # create strings for selecting response easier

qualia_riv <- qualia %>%
  filter(Procedure.Trial. == "FeliceNeutroProc" | Procedure.Trial. == "NeutroFeliceProc" ) %>%
  select(Subject,ID,Remove, Session, Chopstick, Handedness, Lenti, Sex, TAG, b, m, ColoreB, ColoreM, RESP, RT) %>%
  arrange(Subject)


# Keys Duration -----------------------------------------------------------

x <- qualia_riv # copy for calculating

x[is.na(x)] <- 15000 # all NA value to 15000

x <- x[,RT] # selecting only times

x$key30 <- rep(15000, length(x$KeyPress29RT)) # new column for last key

## Keys Calculation

x <- x[,-1] - unlist(x) # unlist() for making difference with different length

## Renaming Columns

colnames(x) <- DUR

## Adding Onset Rivarly culumn 

x <- x %>%
  add_column(qualia_riv$KeyPress1RT, .before = TRUE) %>%
  rename("dur_onset" = "qualia_riv$KeyPress1RT")


# Final Dataframe ---------------------------------------------------------

qualia_riv <- cbind(qualia_riv, x)
         
         
############################## TRIAL SUM KEYS #############################
# Sum of individual keys ---------------------------------------------------

## Matrix Creation

qualia_dur <- qualia_riv %>%
  select(DUR) %>%
  as.matrix()

qualia_resp <- qualia_riv %>%
  select(RESP) %>%
  as.matrix()

## Selecting which position for every key

b<-which(qualia_resp == "b")
m<-which(qualia_resp == "m")
n<-which(qualia_resp == "n")

## 3 New matrix for calculating

qualia_dur_b<-qualia_dur
qualia_dur_m<-qualia_dur
qualia_dur_n<-qualia_dur

## Put NA in unwanted keys

qualia_dur_b[c(m,n)] <- NA
qualia_dur_m[c(b,n)] <- NA
qualia_dur_n[c(b,m)] <- NA

## Sum and adding columns to general dataframe

qualia_riv$tot_b<-apply(qualia_dur_b, 1,sum, na.rm=T)
qualia_riv$tot_m<-apply(qualia_dur_m, 1,sum, na.rm=T)
qualia_riv$tot_n<-apply(qualia_dur_n, 1,sum, na.rm=T)

## Put NA insted 0
qualia_dur_b[qualia_dur_b==0]<-NA
qualia_dur_m[qualia_dur_m==0]<-NA
qualia_dur_n[qualia_dur_n==0]<-NA

freq_b<-qualia_dur_b-(qualia_dur_b-1)
freq_m<-qualia_dur_m-(qualia_dur_m-1)
freq_n<-qualia_dur_n-(qualia_dur_n-1)

qualia_riv$freq_b<-apply(freq_b, 1,sum, na.rm=T)
qualia_riv$freq_m<-apply(freq_m, 1,sum, na.rm=T)
qualia_riv$freq_n<-apply(freq_n, 1,sum, na.rm=T)


############################## FINAL DATASET ##############################
# Cleaning ----------------------------------------------------------------
RESP2<-RESP[-1]
qualia_riv2 <- qualia_riv %>%
  select(-RT, -RESP2, -DUR)

# Converting TAG in only numbers

qualia_riv2$stimuli <- qualia_riv$TAG %>%
  as.character() %>%
  parse_number() %>%
  as.factor()

# Initial percept-----
for(i in 1: length(qualia_riv2$KeyPress1RESP))
if(qualia_riv2$KeyPress1RESP[i] == "b"){
  qualia_riv2$KeyPress1RESP[i]<-qualia_riv2$b[i]
} else if (qualia_riv2$KeyPress1RESP[i]== "n"){
  qualia_riv2$KeyPress1RESP[i]<-qualia_riv2$m[i]
} else {
  qualia_riv2$KeyPress1RESP[i]<-"MISTO"
}




# Associating keys and emotion --------------------------------------------
 
qualia_long <- qualia_riv2 %>%
  gather(key = keys, value = time, c(tot_b, tot_n, tot_m)) %>%
  mutate(emozione = case_when(b=="FELICE" & keys == "tot_b" ~ "felice",
                              m=="FELICE" & keys == "tot_m" ~ "felice",
                              b=="NEUTRO" & keys == "tot_b" ~ "neutro",
                              m=="NEUTRO" & keys == "tot_m" ~ "neutro",
                              (b=="NEUTRO" | b=="FELICE") & keys == "tot_n" ~ "misto")) %>%
  mutate(direction = case_when(stimuli == "1" | stimuli == "2" ~ "up",
                               stimuli == "3" | stimuli == "4" ~ "down"))

# Onset -----
onset<-qualia_long %>%
  select(Subject,ID,Remove, Chopstick, direction, KeyPress1RESP, dur_onset) %>%
  mutate(KeyPress1RESP = case_when(KeyPress1RESP=="FELICE" ~ "happy",
                                   KeyPress1RESP=="MISTO" ~ "mixed",
                                   KeyPress1RESP=="NEUTRO" ~ "neutral")) %>%
           mutate(Chopstick = case_when(Chopstick=="YES" ~ "chopstick",
                                        Chopstick=="NO" ~ "without")) %>%
  group_by(Subject,ID,Remove, Chopstick, direction, KeyPress1RESP) %>%
  summarise_at(vars(dur_onset), list(mean)) %>%
  as.data.frame()
onset$cond<-paste0(onset$KeyPress1RESP,".",onset$Chopstick,".",onset$direction,".onset")
onset<-onset%>%   select(Subject,ID,Remove,  cond, dur_onset) %>%
spread(key = cond, value = dur_onset)

# Mean per conditions ------------------------------------------------------

qualia_long_mean <- qualia_long %>%
  select(Subject,ID,Remove, Session, Chopstick, direction, stimuli, keys, time, emozione, dur_onset) %>%
  group_by(Subject,ID,Remove, Chopstick, direction, emozione) %>%
  summarise_at(vars(time), list(mean)) %>%
  as.data.frame() %>%
  spread(key = emozione, value = time)


# Only Useful Variables ---------------------------------------------------


qualia_long_mean<-qualia_long_mean %>%
  select(Subject,ID,Remove, Chopstick, direction, felice, neutro, misto) %>%
  arrange(Subject,Chopstick, direction)


# Alpers Index ------------------------------------------------------------

qualia_long_mean$alpers <- (qualia_long_mean$felice - qualia_long_mean$neutro) / (qualia_long_mean$felice + qualia_long_mean$neutro)

# Valuation ----
# recode Valence
qualia_valenza$ValImage.RESP[qualia_valenza$ValImage.RESP=="q"] <- c(-3)
qualia_valenza$ValImage.RESP[qualia_valenza$ValImage.RESP=="w"] <- c(-2)
qualia_valenza$ValImage.RESP[qualia_valenza$ValImage.RESP=="e"] <- c(-1)
qualia_valenza$ValImage.RESP[qualia_valenza$ValImage.RESP=="r"] <-  c(0)
qualia_valenza$ValImage.RESP[qualia_valenza$ValImage.RESP=="t"] <-  c(1)
qualia_valenza$ValImage.RESP[qualia_valenza$ValImage.RESP=="y"] <-  c(2)
qualia_valenza$ValImage.RESP[qualia_valenza$ValImage.RESP=="u"] <-  c(3)
qualia_valenza$ValImage.RESP<-as.numeric(as.character(qualia_valenza$ValImage.RESP))
qualia_valenza$TAG1[qualia_valenza$TAG1=="FELICE"] <-  "happy"
qualia_valenza$TAG1[qualia_valenza$TAG1=="NEUTRO"] <-  "neutral"
qualia_valenza$COLORE1[qualia_valenza$COLORE1=="ROSSO"] <-  "red"
qualia_valenza$COLORE1[qualia_valenza$COLORE1=="VERDE"] <-  "green"
qualia_valenza$Chopstick[qualia_valenza$Chopstick=="YES"] <-  "yes"
qualia_valenza$Chopstick[qualia_valenza$Chopstick=="NO"] <-  "no"
# new order Valence
qualia_valenza<-qualia_valenza[order(qualia_valenza$Subject,qualia_valenza$TAG),]
# Add Sex and Direction columns
qualia_valenza$Sex<-c(rep("female",2),rep("male",2))
qualia_valenza$Direction<-c(rep("up",4),rep("down",4))
# new order Arousal
qualia_arousal<-qualia_arousal[order(qualia_arousal$Subject,qualia_arousal$TAG),]
Valuation<-data.frame(qualia_valenza[,c(1:7,9:10)],qualia_valenza$ValImage.RESP,qualia_arousal$ArouImage.RESP)
colnames(Valuation)<-c("Subject", "ID", "Remove", "Chopstick","Stimulus","Emotion", "Colour","Sex", "Direction", "Valence","Arousal")

# subtitution value for F1 F3
Valuation$Valence<-as.numeric(as.character(Valuation$Valence))

Valuation$Arousal[Valuation$Subject<20 &Valuation$Stimulus== "R_F1.png"]<-round(mean(Valuation$Arousal[Valuation$Subject>20 &Valuation$Stimulus== "R_F1.png"]),0)
Valuation$Arousal[Valuation$Subject<20 &Valuation$Stimulus== "R_F3.png"]<-round(mean(Valuation$Arousal[Valuation$Subject>20 &Valuation$Stimulus== "R_F3.png"]),0)
Valuation$Valence[Valuation$Subject>20 &Valuation$Stimulus== "R_F1.png"]<-round(mean(Valuation$Valence[Valuation$Subject>20 &Valuation$Stimulus== "R_F1.png"]),0)
Valuation$Valence[Valuation$Subject>20 &Valuation$Stimulus== "R_F3.png"]<-round(mean(Valuation$Valence[Valuation$Subject>20 &Valuation$Stimulus== "R_F3.png"]),0)






# missing value ----
missing.resp<-qualia_riv[is.na(qualia_riv$dur_onset),]




# Save -----

save(qualia_long,
     qualia_long_mean, 
     Valuation, 
     missing.resp,
     Info,
     onset,
     file = "qualia.RData")

###########################################################################
#                                   END                                   #
###########################################################################





