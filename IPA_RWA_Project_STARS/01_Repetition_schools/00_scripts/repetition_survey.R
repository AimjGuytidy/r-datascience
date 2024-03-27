# Import repetition survey data 

# libraries 
library(here)
library(tidyverse)
library(openxlsx)

# Box Path

if (Sys.getenv("USERNAME") == "HP" && Sys.getenv("COMPUTERNAME") == "RW-5CG2404KFQ") {
  box_root <- file.path("C:","Users","HP", "Box", "IPA_RWA_Project_STARS")
  
} else {
    stop("Define machine-specific Box Path.")
}

box_rep_survey_data <- file.path(box_root, "07_Data", "34_Repetition_schools",
                                 "01_raw")
#dir.create(file.path(here(),"IPA_RWA_Project_STARS","01_Repetition_schools",
#                     "01_raw"),recursive = TRUE)

# Copy excel data

file.copy(file.path(box_rep_survey_data, "repetition_schools.xlsx"),
          here("IPA_RWA_Project_STARS","01_Repetition_schools","01_raw"))


box_rep_survey <- read.xlsx(here("IPA_RWA_Project_STARS","01_Repetition_schools",
                                 "01_raw","repetition_schools.xlsx"))


