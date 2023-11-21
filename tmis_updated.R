rm(list = ls())

library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)
library(haven)
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

dt_age_level <- count(tmis_teacher,teachingCategoryName,age_categ)
dt_age_level <- as.data.table(dt_age_level)
dt_age_level <- dt_age_level[!is.na(dt_age_level$age_categ),]
cross_age_level <- dcast(dt_age_level,
                         teachingCategoryName~age_categ,value.var = "n")

write.xlsx(cross_age_level,
           "04_reporting/01_tables/updated/cross_ref_age_level.xlsx",
           asTable = T)

# cross reference age categories with positions

# we need to save "tmis_filter" to dta format and categorize positions using STATA
write_dta(tmis_filter,"03_clean_data/tmis_filter.dta")

# after assigning labels to position now we can start our categorization
tmis_label <- read_dta("03_clean_data/tmis_filter.dta")
sum(is.na(tmis_label$class)) # there is no missing class value
tmis_label <- tmis_label |>
  mutate(class = as.character(class),
    subject = case_when(
    class %in% c(
      "7",
      "8",
      "11",
      "12",
      "14",
      "15",
      "32",
      "39",
      "40",
      "41",
      "49",
      "50",
      "51",
      "52",
      "53",
      "54",
      "55",
      "56",
      "66",
      "83",
      "85",
      "95",
      "96",
      "97",
      "98",
      "99",
      "100",
      "101",
      "102",
      "103",
      "104",
      "107",
      "108",
      "109",
      "110",
      "111",
      "118",
      "121"
    ) ~ "STEM",
    class %in% c(
      "23",
      "24",
      "26",
      "29",
      "30",
      "33",
      "34",
      "35",
      "37",
      "38",
      "43",
      "59",
      "60",
      "61",
      "62",
      "63",
      "66",
      "67",
      "69",
      "70",
      "71",
      "72",
      "73",
      "83",
      "84",
      "85",
      "86",
      "87",
      "88",
      "90",
      "91",
      "93",
      "125"
    ) ~ "Languages",
    class %in% c(
      "9",
      "18",
      "21",
      "22",
      "19",
      "24",
      "27",
      "29",
      "30",
      "31",
      "54",
      "55",
      "58",
      "64",
      "65",
      "66",
      "73",
      "74",
      "75",
      "76",
      "77",
      "78",
      "79",
      "80",
      "81",
      "82",
      "92",
      "99",
      "100",
      "102",
      "109",
      "110",
      "112",
      "113",
      "114",
      "116",
      "119"
    ) ~ "Humanities",
    class %in% c("4", "6", "10", "117", "123") ~ "Finance",
    class %in% c("25","68","126","127","128") ~ "Education",
    class %in% c("84","115") ~ "Special Needs Educ",
    class %in% c("116","105","57") ~ "Arts",
    class == "94" ~ "Lower Primary",
    class == "106" ~ "Nursery School",
    class == "44" ~ "Normal Primary"
  ))

# cross reference age categories with leadership roles

dt_age_role <- count(tmis_filter,age_categ,role)
dt_age_role <- as.data.table(dt_age_role)
dt_age_role <- dt_age_role[!is.na(dt_age_role$age_categ),]