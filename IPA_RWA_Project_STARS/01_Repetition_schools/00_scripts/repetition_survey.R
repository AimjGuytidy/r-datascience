# Import repetition survey data 

# libraries 
library(here)
library(tidyverse)
library(openxlsx)
library(rio)
library(labelled)

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


# assigning values to NAs

for (i in colnames(box_rep_survey)) {
  vec <- which(is.na(box_rep_survey[,i]))
 for (j in vec) {
    box_rep_survey[j,i] <- box_rep_survey[j-1,i]
    }
}

# remove whitespace
box_rep_survey[,"GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION"] <- str_trim(box_rep_survey[,"GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION"])

box_rep_survey[,"CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION"] <- str_trim(box_rep_survey[,"CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION"])

# remove special characters
box_rep_survey[,"GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION"] <- gsub("[[:punct:]]", " ",box_rep_survey[,"GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION"])

box_rep_survey[,"CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION"] <- gsub("[[:punct:]]", " ",box_rep_survey[,"CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION"])


# check for duplicated rows

View(box_rep_survey %>%
       group_by(ID) %>%               
       mutate(counted = n(),dup = ifelse(counted == 1,0,row_number())) %>%
       filter(dup>0)%>%
       characterize())

View(box_rep_survey %>%
       group_by(GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION) %>%               
       mutate(counted = n(),dup = ifelse(counted == 1,0,row_number())) %>%
       filter(dup>0)%>%
       characterize())

View(box_rep_survey %>%
       group_by(`CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION`) %>%              
       mutate(counted = n(),dup = ifelse(counted == 1,0,row_number())) %>%
       filter(dup>0)%>%
       characterize())


# Check for unique options for GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION

view(count(box_rep_survey,GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION))

# Check for unique options for CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION



