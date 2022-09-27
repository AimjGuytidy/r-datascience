library(tidyverse)
temp <- read_csv("data/mean-temperature-of-rwanda_ggplot.csv")
View(temp)
ggplot(data = temp)+
  geom_line(aes(year,temeperature),color= "red")+
  #geom_point(aes(year,temeperature),color="blue")+
  ylab("Temperature")+
  labs(color="white")+
  theme(plot.background = element_rect(fill="blue"),
        panel.background = element_rect(fill = "yellow"),
        axis.title = element_text(colour = "green"))+
  geom_text(aes(year,temeperature,label=temeperature),
            size=1.5,color="black")
