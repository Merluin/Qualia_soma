stim_amplitude <- function(x){
  #################################################
  # 
  # Name:           Psychopy dataset
  # Programmer:     Thomas Quettier
  # Date:           23/03/2022
  # Description:    clean extra columns (practice) from Qualiasoma experiment (psychopy3)
  #
  #################################################
  
  hz5 <- x%>%
    select(amplitude_5hz)%>%
    filter(!is.na(amplitude_5hz))%>%
    tail(n=1)
  hz31 <- x%>%
    select(amplitude_31hz)%>%
    filter(!is.na(amplitude_31hz))%>%
    tail(n=1)

x<- x%>%
  mutate(amplitude_5hz =  hz5[1,1],
         amplitude_31hz = hz31[1,1])
  
  return(x)
  
  ###########################################################################
  #                                   END                                   #
  ###########################################################################
}