barct <- function(x,name_plot){
  
  y<-x%>%
    group_by(percept,condition,frequency) %>%
    summarise_at(vars(duration), list(mean))
  plot(y)
p<- ggplot(y,aes(y=duration,x=percept, fill = as.factor(frequency)) )
p + geom_bar(stat="identity", position = "dodge2")
p + facet_grid(. ~ condition)
p + theme_classic()
p + theme(text=element_text(size=16,  family="Helvetica"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.text.y = element_text(size = 20))
p
  
 
  #ggsave(paste0("07.figures/",name_plot,".tiff"), units="in", width=5, height=4, dpi=200, compression = 'lzw')
  

}