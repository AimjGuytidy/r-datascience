library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(haven)
library(foreign)
dataset0 <- read_dta("data/MCF Baseline Survey Study V1.dta")
dataset_clean<-dataset0%>%
       separate(emply_status_calc,sep = "_",into = c("emp","geo_ent"))%>%
       separate(geo_ent,sep = 2,into = c("geo_entity","sex"))


#homework#
#########

employed_youth<-dataset_clean %>%
                  filter(employed==1&gender==2&geo_entity=="ru")


household_heads <- dataset_clean %>%
                      filter(hhrelation==1)

#getwd()
#write_dta(employed_youth,"file1.dta")
#write.dta(employed_youth,"file1.dta")

#data manipulation#
###################
ass_var <- dataset0%>%select("uniqueid","enumerator","resp_name_calc","resp_phone","phonoth_calc","source_water_oth", "reduce_exp_oth", "coping_strat_oth", "hinv_help_oth", "resp_conditions_oth")
view(as.data.frame.table(table(ass_var$source_water_oth)))
view(ass_var)
view(select(dataset0,"inco_total"))
#select(dataset0,)
library(ggplot2)
ggplot(select(dataset0,"inco_total"))+
  geom_boxplot(aes(log(inco_total)))+
  coord_flip()
