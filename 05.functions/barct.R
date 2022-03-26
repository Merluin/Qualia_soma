barct <- function(x,name_plot){
  
  x%>%
    group_by(percept,condition,frequency) %>%
    summarise_at(vars(duration), list(mean))%>%
    ggplot(aes(y=duration,x=percept, fill = frequency) )+
    geom_bar(stat="identity", position = "dodge2")+
    facet_grid(. ~ condition)+
    theme_classic()+
    theme(text=element_text(size=16,  family="Helvetica"),
          panel.background = element_blank(),
          axis.line = element_line(colour = "black"),
          strip.text.y = element_text(size = 20))
  ggsave(paste0("07.figures/",name_plot,".tiff"), units="in", width=5, height=4, dpi=200, compression = 'lzw')
  
}