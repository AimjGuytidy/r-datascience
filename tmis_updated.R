rm(list = ls())

library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)
#install.packages("here")
library(here)

# set working directory
setwd(paste0("C:/Users/HP/Box/",
             "IPA_RWA_Project_STARS/07_Data/",
             "31_TMIS_analysis"))


# get data

tmis_df <- read_excel("02_raw_data/tmis_data_october_2023.xlsx")
