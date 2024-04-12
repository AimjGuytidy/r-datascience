rm(list = ls())
# Import repetition survey data 

# libraries 
library(here)
library(tidyverse)
library(openxlsx)
library(rio)
library(labelled)
library(haven)

# Box Path

if (Sys.getenv("USERNAME") == "HP" && Sys.getenv("COMPUTERNAME") == "RW-5CG2404KFQ") {
  setwd(paste("C:/","Users","HP", "Box", "IPA_RWA_Project_STARS","07_Data",
              "34_Repetition_schools",sep = "/"))
  
} else {
  stop("Define machine-specific Box Path.")
}

# combining categorized data
rep_surv_1 <- read.xlsx("01_raw/Francoise_categ.xlsx")
rep_surv_2 <- read.xlsx("01_raw/Olga_categ.xlsx")
rep_surv_3 <- read.xlsx("01_raw/Parfait_categ.xlsx")

rep_surv_comb <- rbind(rep_surv_2, rep_surv_1, rep_surv_3)

# 