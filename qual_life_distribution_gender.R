library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)
library(haven)
library(foreign)
library(labelled)
library(rio)
library(psych)
library(ggplot2)
library(stringr)
library(forcats)
library(data.table)
library(scales)
Light_grey <- c("#F2F2F2") #Light grey for the background
Blue <- c("#097ABC") #Blue
Light_blue <- c("#9DC3E5") #Light blue
Dark_blue <- c("#1b2f55") #Dark blue
Green <- c("#8ccc98") #Green
Dark_grey <- c("#7F7F7F") #Dark grey
Dark_green <- c("#49711E") #Dark green

#Visualizing L5.1.2b to get the overall view#####
mcf_data <- read_sav("data/mcf_data_new_reporting_tool.sav")
mcf_data_l5_t <- mcf_data

keyword_label<-c("D14",	"D16",	"D18",	"D20",	"D22",	"D24",	"D26",	"D28",	"D30",	"D32",	"D34",	"D36")
variables_for_l531_a <- mcf_data_l5_t%>%look_for(keyword_label)
variables_for_l531_a <-variables_for_l531_a[,"variable"]
var_df <- as.data.frame(variables_for_l531_a)

for (i in 1:length(keyword_label) ){
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==1]<-0
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==2]<-1
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==3]<-2
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==4]<-3
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==5]<-4
}

# change the values from 1-3 to 0-2 
keyword_label_b<-c("D15",	"D17",	"D19",	"D21",	"D23",	"D25",	"D27",	"D29",	"D31",	"D33",	"D35",	"D37")
variables_for_l531_b <- mcf_data_l5_t%>%look_for(keyword_label_b)
variables_for_l531_b <-variables_for_l531_b[,"variable"]
var_df_b <- as.data.frame(variables_for_l531_b)

for (i in 1:length(keyword_label_b) ){
  mcf_data_l5_t[,var_df_b[i,]][mcf_data_l5_t[,var_df_b[i,]]==1]<-0
  mcf_data_l5_t[,var_df_b[i,]][mcf_data_l5_t[,var_df_b[i,]]==2]<-1
  mcf_data_l5_t[,var_df_b[i,]][mcf_data_l5_t[,var_df_b[i,]]==3]<-2
}

# step 1: summing values from variables covering subquestion a and indicator l5.3.1
#var_df_filter <- grep("_access$", var_df$variable,value=TRUE, ignore.case =T)
mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(sum_quality_life=rowSums(select(.,grep("_access$", var_df$variable,value=TRUE, ignore.case =T)
  ),na.rm = TRUE))

# step 2: averaging services improvement (subquestion b related)

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(avg_improv_quality_life=rowMeans(select(.,var_df_b$variable),na.rm = TRUE))

#step 3: computing the product of step 1 and step 2

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prod_quality_life=avg_improv_quality_life*sum_quality_life)

#step 4: adjusting the index to 100 from step 3

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(perc_quality_life=(prod_quality_life*100)/96)


# Quality of life distribution
mcf_data_l5_t %>%
  filter(gender==1) %>%
    ggplot(aes(x=perc_quality_life)) +
      stat_bin(binwidth = 1,aes(y=..count..*100/sum(..count..)),fill=Dark_blue,color = "#000000") +
      ylim(c(0,20))+
      stat_bin(geom="text", aes(label=round(..count..*100/sum(..count..),2)),
               vjust=-.5,
               size = 3.3)+
      scale_y_continuous(breaks = seq(from = 0, to = 100, by = 10),limits = c(0,100))+
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(
        plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank(),
    #remove x axis ticks
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
    #remove y axis labels
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()#remove y axis ticks
      )



annotations <- data.frame(
  x = c(round(min(
    mcf_data_l5_t$perc_quality_life
  ), 2), round(
    mean(mcf_data_l5_t$perc_quality_life), 2
  ), round(max(
    mcf_data_l5_t$perc_quality_life
  ), 2)),
  y = c(0.0025, 0.0375, 0.0025),
  label = c("Min:", "Mean:", "Max:")
)

ggplot(mcf_data_l5_t, aes(perc_quality_life)) +
  geom_histogram(
    aes(y = ..density..),
    color = "#000000",
    fill = Dark_blue,
    binwidth = 2
  ) +
  geom_density(color = "#000000",
               fill = Light_grey,
               alpha = 0.6) +
  geom_text(
    data = annotations,
    aes(
      x = x,
      y = y,
      label = paste(label, x)
    ),
    size = 2,
    fontface = "bold"
  ) 




#------------------------------------------------------------------------------


mcf_data_l5_t_male <- mcf_data_l5_t %>%
  filter(gender == 1)

male_wtmean <- mcf_data_l5_t_male %>%
  group_by() %>%
  summarise(meano = weighted.mean(perc_quality_life, weights, na.rm = TRUE))

annotations_male <- data.frame(
  x = c(round(min(
    mcf_data_l5_t_male$perc_quality_life
  ), 2), round(
    mean(male_wtmean$meano), 2
  ), round(max(
    mcf_data_l5_t_male$perc_quality_life
  ), 2)),
  y = c(0.0025, 0.0375, 0.0025),
  label = c("Min:", "Mean:", "Max:")
)

ggplot(mcf_data_l5_t_male, aes(perc_quality_life)) +
  geom_histogram(
    aes(y = ..density..),
    color = "#000000",
    fill = Light_blue,
    binwidth = 2
  ) +
  geom_density(color = "#000000",
               fill = Light_grey,
               alpha = 0.6) +
  geom_text(
    data = annotations_male,
    aes(
      x = x,
      y = y,
      label = paste(label, x)
    ),
    size = 2,
    fontface = "bold"
  )+
  xlab("Quality of life index") +
  labs(title = "Distribution of quality of life amongst male youth") +
  scale_y_continuous(labels = scales::label_percent( scale = 100))+
  theme(plot.title = element_text(hjust = 0.2)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank()
  )

#------------------------------------------------------------------------------

mcf_data_l5_t_female <- mcf_data_l5_t %>%
  filter(gender == 2)

female_wtmean <- mcf_data_l5_t_female %>%
  group_by() %>%
  summarise(meano = weighted.mean(perc_quality_life, weights, na.rm = TRUE))

annotations <- data.frame(
  x = c(round(min(
    mcf_data_l5_t$perc_quality_life
  ), 2), round(
    mean(mcf_data_l5_t$perc_quality_life), 2
  ), round(max(
    mcf_data_l5_t$perc_quality_life
  ), 2)),
  y = c(0.0025, 0.0375, 0.0025),
  label = c("Min:", "Mean:", "Max:")
)

ggplot(mcf_data_l5_t, aes(perc_quality_life)) +
  geom_histogram(
    aes(y = ..density..),
    color = "#000000",
    fill = Dark_blue,
    binwidth = 2
  ) +
  geom_density(color = "#000000",
               fill = Light_grey,
               alpha = 0.6) +
  geom_text(
    data = annotations,
    aes(
      x = x,
      y = y,
      label = paste(label, x)
    ),
    size = 2,
    fontface = "bold"
  )
