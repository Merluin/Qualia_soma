clean_practice <- function(x){
#################################################
# 
# Name:           Psychopy dataset
# Programmer:     Thomas Quettier
# Date:           23/03/2022
# Description:    clean extra columns (practice) from Qualiasoma experiment (psychopy3)
#
#################################################

  if(colnames(x[1]) == "stim_left" ){x <-x%>%
    select(-stim_left,-stim_right,-trials_2.thisRepN,-trials_2.thisTrialN,-trials_2.thisN,-trials_2.thisIndex,
           -both_txt_loop.thisRepN,-both_txt_loop.thisTrialN,-both_txt_loop.thisN,-both_txt_loop.thisIndex,
           -both_stim_loop.thisRepN,-both_stim_loop.thisTrialN,-both_stim_loop.thisN,-both_stim_loop.thisIndex,
           -training_trials_loop.thisRepN,-training_trials_loop.thisTrialN,-training_trials_loop.thisN,
           -training_trials_loop.thisIndex,-position.right,-position.left,-txt_right.started,-txt_right.stopped,-txt_left.started,
           -txt_left.stopped,-left.started,-left.stopped,-right.started,-right.stopped,-kb_trial_training.keys,
           -kb_trial_training.rt,-kb_trial_training.started,-kb_trial_training.stopped,-kb_extra_training.keys,
           -kb_extra_training.rt,-kb_extra_training.started,-kb_extra_training.stopped)
  }
  
  return(x)

###########################################################################
#                                   END                                   #
###########################################################################
}