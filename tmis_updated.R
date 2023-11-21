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


# get data ####

tmis_df <- read_excel("02_raw_data/tmis_data_october_2023.xlsx")

# Duplicates check ####

View(tmis_df %>%
       group_by(employeeid) %>%               ## grouped by employeeid
       mutate(counted = n(),dup = ifelse(counted == 1,0,row_number())) %>%
       filter(dup>0)) # there are no duplicates based on the employeeid

# check missing age rows ####

sum(is.na(tmis_df[["year_birth"]])) # we have six rows with missing birth dates

# Categorize ages in 5s ####

tmis_filter <- tmis_df|>
  select(employeeid,gender,civilStatus,year_birth,position,qualificationLevel,
         role,schoolName:districtName)|>
  mutate(age = as.integer(format(Sys.Date(), "%Y")) - as.integer(year_birth),
         age_categ = if_else(age >= 70, "70 and above",
                                if_else(age >= 65, "65-69",
                                        if_else(age >= 60, "60-64",
                                                if_else(age >= 55, "55-59",
                                                        if_else(age >= 50, "50-54",
                                                                if_else(age >= 45, "45-49",
                                                                        if_else(age >= 40, "40-44",
                                                                                if_else(age >= 35, "35-39",
                                                                                        if_else(age >= 30, "30-34",
                                                                                                if_else(age >= 24, "24-29",
                                                                                                        "18-23")))))))))))
# Cross reference age categories with gender ####

# filter staff that is considered as teaching staff
tmis_teacher <- filter(tmis_filter,
                       role %in% c("Teacher", "DOD", "DOS", "Head Teacher"))

tmis_age_gender <- count(tmis_teacher,gender,age_categ)

tmis_age_gender|>
  filter(!is.na(age_categ)) |>
  rename(age_count = n) |>
  mutate(age_count = if_else(gender=="Female",-age_count,age_count)) |>
  ggplot(aes(x = age_categ, y = age_count, fill = gender)) +
    geom_bar(stat = "identity", width = .98) +
    scale_y_continuous(breaks = waiver(), labels = waiver()) +
    coord_flip() +
    #theme_gray() +
    labs(title="Teacher Population October 2023") +
    scale_fill_manual(name = NULL, values = c("Female" = "#FF9130",
                                              "Male" = "#3876BF"))+
    theme(plot.title = element_text(hjust = .5),
          axis.ticks = element_blank())+
    theme(
      plot.background = element_rect(fill = c("#F2F2F2")),
      panel.background = element_rect(fill = c("#F2F2F2")),
      panel.grid = element_blank(),
      #remove x axis ticks
      axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #remove x axis labels
      axis.ticks.x = element_blank(),  #remove x axis ticks
      axis.text.y = element_text(size=10, face="bold", colour = "black"),
      legend.box = "horizontal",
      legend.position = "bottom"
    )

dt_age_gender <- as.data.table(tmis_age_gender)
dt_age_gender <- dt_age_gender[!is.na(dt_age_gender$age_categ),]
cross_age_gender <- dcast(dt_age_gender,gender~age_categ,value.var = "n")

write.xlsx(cross_age_gender,
           "04_reporting/01_tables/updated/cross_ref_age_gender.xlsx",
           asTable = T)

# cross reference age categories with teaching level ####
