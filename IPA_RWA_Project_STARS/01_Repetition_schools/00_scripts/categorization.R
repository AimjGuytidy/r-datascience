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

# check the available categories #
##################################

# checking entries with NAs for categories (for guideline categories)

temp_guidelines <-
  filter(
    rep_surv_comb,
    is.na(Guidelines.Categories) &
      grepl("-", GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION, fixed = T) &
      !grepl("^-.*", GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION, fixed = F)
  )
temp_guidelines [, "Guidelines.Categories"] <-
  ifelse(
    grepl(
      "-",
      temp_guidelines$GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION,
      fixed = T
    ),
    "specific student",
    "data"
  )


# checking entries with NAs for categories (for criteria categories)

temp_criterias <-
  filter(
    rep_surv_comb,
    is.na(Criteria.Categories)&
      grepl("(?i)sen$", `CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION`))

temp_criterias [, "Criteria.Categories"] <-
  ifelse(
    grepl(
      "(?i)sen$",
      temp_criterias$`CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION`
    ),
    "special social case",
    NA_character_
  )

# join
temp_guide_filtered <- temp_guidelines |>
  select(ID:Guidelines.Categories)


rep_surv_comb <- rep_surv_comb |>
    left_join(
      temp_guide_filtered,
      by = join_by(
        ID,
        `WHAT'S.THE.NAME.OF.YOUR.SCHOOL`,
        SDMS.CODE,
        DISTRICT,
        SCHOOL.STATUS,
        SCHOOL.CATEGORY,
        GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION
      )
    ) |>
    mutate(
      Guidelines.Categories = coalesce(Guidelines.Categories.x, Guidelines.Categories.y)
    )

