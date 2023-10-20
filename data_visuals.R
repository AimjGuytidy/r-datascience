# Preliminaries

rm(list = ls()) # removing all objects to start with a clean slate
library("utils")
library("tidyverse")
library("haven")

getwd() # make sure I am in the right directory

Light_grey <- c("#FFFFDD") #Light grey for the background
Blue <- c("#279EFF") #Blue
Light_blue <- c("#9DC3E5") #Light blue
Dark_blue <- c("#0C356A") #Dark blue
Green <- c("#016A70") #Green
yellow_green <- c("#D2DE32") #yellow green
Dark_green <- c("#A2C579") #Dark green
dark_greenish <- c("#164B60")
whitish <- c("#F9F9F9")

merged_scores <- read_dta("03_clean/merged_scores_b1.dta")
districts <- unique(merged_scores$district)

for (i in districts){
  ggplot(data = merged_scores[merged_scores$district==i,])+
    geom_line(aes(x = seq(1,nrow(merged_scores[merged_scores$district==i,])),
                  y=teacher_score,group=1,colour="HeadTeacher Score"),linewidth = 1.2,alpha=.6)+
    geom_point(aes(x = seq(1,nrow(merged_scores[merged_scores$district==i,])),
                   y=teacher_score,group=1,color=Dark_blue))+
    geom_line(aes(x = seq(1,nrow(merged_scores[merged_scores$district==i,])),
                  y=imihigo_score,group=1,colour="Imihigo Score"),linewidth = 1.2,alpha=.6)+
    geom_point(aes(x = seq(1,nrow(merged_scores[merged_scores$district==i,])),
                   y=imihigo_score,group=1,color=Dark_blue))+
    scale_color_manual(name = paste(i,"District"), values = c("HeadTeacher Score" = Green,
                                                             "Imihigo Score" = Blue)) + 
    ylim(60,100)+
    #ylab("Total marks submitted")+
    #ggtitle(title_prep)+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      plot.background = element_rect(fill = c(whitish)),
      panel.background = element_rect(fill = c(whitish)),
      #remove y axis labels
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),#remove y axis ticks
      # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color=Light_blue ) 
    ) +
    
    theme(axis.text.x = element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank())+ 
    theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                      vjust = 0.5, face = 'bold'))+
    theme(axis.text.y = element_text(colour = 'black', size = 10),
          axis.title.y = element_blank())
  
  

  
  ggsave(paste("04_output/",i,".png",sep = ""),width = 2500,height = 2500,units=c("px"))
  
}

ggplot(data = merged_scores)+
  geom_point(aes(x = imihigo_score,y=teacher_score))