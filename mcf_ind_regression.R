#Load required packages
library(foreign) #for reading .dta files
library(dplyr) #for the filter function
library(questionr) #for the weight function
library(tidyverse)
library(haven)
library(openxlsx)
library(readr)
library(tidyr)
library(rio)
library(expss)
library(labelled)
library(weights)
library(anesrake)
library(stringr)
library(zoo)
library(sjmisc)
library()
#Load in data
mcf_data<-read_dta("data/mcf_clean_parfait.dta")
mcf_data<-mcf_data%>%
            select(where(is.numeric))
#sum(!is.na(mcf_data$new_improv_self_employment))
mcf_data<-mcf_data%>%filter(type_employ==2)%>%
  select(-c(uniqueid:note_sec_a))
view(cor(mcf_data))
