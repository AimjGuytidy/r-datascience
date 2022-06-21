library(tidyverse)
library(readr)
library(readxl)
library(dplyr)
library(haven)

dataset0 <- read_dta("R session/MCF Baseline Survey Study V1.dta")
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
