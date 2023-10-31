########################
# HT PHONE SURVEY#
#######################
# HT#
####################
rm(list = ls()) # removing all objects to start with a clean slate
library(foreign)
library(haven)
library(tidyverse)
library(VIM)
library(outliers)
library(ggplot2)
library(scales) 
library(grid)
library(RColorBrewer)
library(psych)
#install.packages("sjlabelled")
library(testit)
library(matrixStats)
library(rio)
library(labelled)
library(openxlsx)
library(data.table)
library(sjlabelled)


setwd("C:/Users/HP/Box/IPA_RWA_Project_STARS/07_Data/26_HT_Teacher_Phone_Survey")

df <- read_dta("03_clean/final_dataset/Teacher_Headteacher_Survey_Round_clean_final.dta")
df_camis <- select(df,caseid,starts_with("camis_2"))
df_camis_2 <- filter(df_camis,!is.na(camis_2))
temp <- unlist(var_label(df_camis_2))
temp["camis_2_1_1"] <- "1. Couldn't access account"
temp["camis_2_1_17"] <- "17. Lack of infranstructures such as electricity"
temp <- gsub('[0-9]+', '', temp)
temp <- gsub('\\.', '', temp)
df_camis_long <- df_camis_2%>%
  rename(all_of(setNames(names(temp), temp))) %>% 
  pivot_longer(cols = -caseid,names_to = "challenge",values_to = "status")
df_camis_long <- filter(df_camis_long,challenge!="C Did you face any challenges while entering data on CA-MIS?")
df_camis_long <- filter(df_camis_long,status!=0)

ggplot(data = df_camis_long)+
  geom_bar(aes(x = challenge))+
  coord_flip()+
  ylab("Percent of teachers agreeing")+
  xlab("Challenges")+
  ggtitle("Challenges in entering CAMIS data (among treatment and control teachers)")+
  ylim(0,80)+
  geom_text(aes(y = count(challenge), x = challenge,label=count(challenge)), vjust=-0.5, color="darkblue",
            position = position_dodge(0), size=3.5, fontface = "bold")
ggsave("camis_plot.png",units = "px",width = 1062,height = 633)

df_camis_text <- df_camis_long |>
  group_by(challenge)|>
  summarise(total = sum(status))
