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

# Lower case all categories to have uniformity

rep_surv_comb[,"Guidelines.Categories"] <- str_trim(str_to_lower(rep_surv_comb[,"Guidelines.Categories"]))
rep_surv_comb[,"Criteria.Categories"] <- str_trim(str_to_lower(rep_surv_comb[,"Criteria.Categories"]))

# correct mistakes made in categories

rep_surv_comb[which(rep_surv_comb$Guidelines.Categories == "repetion status"),
              "Guidelines.Categories"]  <- "repetition status"

# check the available categories
