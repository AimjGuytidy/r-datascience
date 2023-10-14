library(tidyverse)
library(scales)

df <- tibble(y = c(520039,461837,533904,18085740,20265163,44439062),
             x = factor(c("Term 1, 2021/22","Term 2, 2021/22","Term 3, 2021/22",
                     "Term 1, 2022/23","Term 2, 2022/23","Term 3, 2022/23"),
                     levels = c("Term 1, 2021/22","Term 2, 2021/22","Term 3, 2021/22",
                                "Term 1, 2022/23","Term 2, 2022/23","Term 3, 2022/23")))
typeof(df$y) 
typeof(df$x)

Light_grey <- c("#FFFFDD") #Light grey for the background
Blue <- c("#279EFF") #Blue
Light_blue <- c("#9DC3E5") #Light blue
Dark_blue <- c("#0C356A") #Dark blue
Green <- c("#016A70") #Green
yellow_green <- c("#D2DE32") #yellow green
Dark_green <- c("#A2C579") #Dark green
dark_greenish <- c("#164B60")

title_prep <- "Across all types of mark, the total submitted is as follows:\n"
ggplot(data = df, aes(x = x, y = y,group = 1 )) +
  geom_line(color=dark_greenish,size = 1.2,alpha=.6)+
  geom_point(color=Dark_blue)+
  geom_text(aes(label=comma(y)), vjust=-0.5, color=Dark_blue,
                        position = position_dodge(0), size=3.5, fontface = "bold") + 
  ylab("Total marks submitted")+
  ggtitle(title_prep)+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c(Light_grey)),
    panel.background = element_rect(fill = c(Light_grey)),
    #remove y axis labels
    axis.ticks.y = element_blank(),
    axis.ticks.x = element_blank(),#remove y axis ticks
    # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( size=.1, color=Light_blue ) 
  ) +
  
  theme(axis.text.x = element_text(colour = 'black', angle = -70,
                                   size = 10, hjust = -0.1, vjust = 
                                     0.5),axis.title.x=element_blank())+ 
  theme(strip.text.y = element_text(size = 11, hjust = 0.5,
                                    vjust = 0.5, face = 'bold'))+
  theme(axis.text.y = element_text(colour = 'black', size = 10),
        axis.title.y = element_text(size = 12,
                                    hjust = 0.5, vjust = 0.2))+
  scale_y_continuous(labels = scales::label_comma())

ggsave("visuals/marks_sub.png")
  