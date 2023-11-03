#######################
# TMIS ANALYSIS#
####################
rm(list = ls()) # removing all objects to start with a clean slate
library(foreign)
library(haven)
library(tidyverse)
library(VIM)
library(outliers)
library(ggplot2)
library(scales) 
library(grid)
library(RColorBrewer)
library(psych)
#install.packages("sjlabelled")
library(testit)
library(matrixStats)
library(rio)
library(labelled)
library(readxl)
library(data.table)
library(sjlabelled)
library(plotly)

df <- read_xlsx("data/tmis_data_october_2023.xlsx")

df_filter <- df|>
  select(employeeid,lastName,firstName,gender,civilStatus,year_birth,position,qualificationLevel,
         role,schoolName:districtName)|>
  unite("Name",lastName:firstName,sep = " ") |>
  mutate(age = as.integer(format(Sys.Date(), "%Y")) - as.integer(year_birth),
         age_brackets = if_else(age >= 66, "66 and above",
                        if_else(age >= 61, "61-65",
                        if_else(age >= 56, "56-60",
                        if_else(age >= 51, "51-55",
                        if_else(age >= 46, "46-50",
                        if_else(age >= 41, "41-45",
                        if_else(age >= 36, "36-40",
                        if_else(age >= 31, "31-35",
                        if_else(age >= 26, "26-30",
                        if_else(age >= 21, "21-25",
                                "17-20")))))))))))
# Duplicates check
View(df_filter %>%
       group_by(employeeid) %>%               ## grouped by staff_id
       mutate(counted = n(),dup = ifelse(counted == 1,0,row_number())) %>%
       filter(dup>0)) # there are no duplicates based on the employeeid

# let's look at the maximum age of the teachers
max(df_filter$age,na.rm = T) # the maximum age is 68
sum(is.na(df_filter$age)) # 6 teachers have missing age values

# visualizing Age Distribution of Teaching Staff in Rwanda
brks <- c(seq(-15000, 15000, by = 500))
lbls = c(seq(15, 0, -5), seq(5, 15, 5))
(p <- df_filter |>
  group_by(age_brackets,gender) |>
  mutate(age_count = n()) |>
  ungroup() |>
  select(age_brackets,age_count,gender) |>
  group_by(age_brackets,gender,age_count) |>
  mutate(counted = n(),dup = ifelse(counted == 1,0,row_number())) %>%
  filter(dup==1,!is.na(age_brackets))|>
  ungroup() |>
  select(-dup) |>
  mutate(age_count = if_else(gender=="Female",-age_count,age_count)) |>
  ggplot(aes(x = age_brackets, y = age_count, fill = gender)) +
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
  ))
setwd("C:/Users/HP/Box/IPA_RWA_Project_STARS/07_Data/31_TMIS_analysis/04_reporting/")
# total teachers by gender
teacher_count <- dplyr::count(dplyr::filter(df_filter,!is.na(age)),gender) |>
  rename(Total = n)
write.xlsx(teacher_count,"01_tables//teacher_count.xlsx",asTable = T)

# Mean, max and min age of teachers by gender
teacher_age <- df_filter |>
  #dplyr::filter(!is.na(age)) |>
  group_by(gender) |>
  summarise(Mean = round(mean(age,na.rm = T)),Min = min(age,na.rm = T),
            Max = max(age, na.rm = T))
write.xlsx(teacher_age,"01_tables//teacher_age.xlsx",asTable = T)

# Age and gender by education level
teacher_educ0 <- df_filter |>
  #dplyr::filter(!is.na(age)) |>
  group_by(teachingCategoryName,gender) |>
  summarise(count = n()) |>
  ungroup() |>
  pivot_wider(id_cols = teachingCategoryName,
              names_from = gender, values_from = count)

teacher_educ1 <- df_filter |>
  group_by(teachingCategoryName) |>
  summarise(`Average age` = round(mean(age, na.rm = T)))

teacher_educ <- teacher_educ1 |>
  left_join(teacher_educ0,by="teachingCategoryName") |>
  mutate(Total = Female + Male) 
teacher_educ[nrow(teacher_educ)+1,"teachingCategoryName"] <- "Total"
teacher_educ[nrow(teacher_educ),"Female"] <- sum(teacher_educ[,"Female"],na.rm = T)
teacher_educ[nrow(teacher_educ),"Male"] <- sum(teacher_educ[,"Male"],na.rm = T)
teacher_educ[nrow(teacher_educ),"Total"] <- sum(teacher_educ[,"Total"],na.rm = T)
write.xlsx(teacher_educ,"01_tables//teacher_educ.xlsx",asTable = T)

# Teachers' retirement by education level
df_age <- df_filter |>
  select(Name,gender,teachingCategoryName,year_birth,age) 

for (years in 2024:2030){
  varname <- as.character(years)
  df_age <- df_age |>
    dplyr::mutate(!! varname := as.integer(years) - as.integer(year_birth))
}

df_age_long <- df_age |>
  rename(`2023` = age) |>
  pivot_longer(cols = -c(Name:year_birth),names_to = "Year", values_to = "Age")
df_retirement <- dplyr::filter(df_age_long,Age >= 65)

retirement <- dplyr::count(df_retirement,Year,teachingCategoryName)|>
  ungroup() |>
  mutate(Year = as.integer(Year)) 
retirement_bind <- retirement |>
  group_by(Year) |>
  mutate(Total = sum(n,na.rm = T))|>
  select(Year,Total) |>
  rename(n=Total) |>
  group_by(Year) |>
  mutate(count = n(),dup = ifelse(count==1,1,row_number())) |>
  filter(dup == 1) |>
  mutate(dup = NULL,count = NULL)

ret_binded <- rbind(retirement,retirement_bind) |>
  mutate(teachingCategoryName = if_else(is.na(teachingCategoryName),"Total",
                                        teachingCategoryName),
         Total = NULL) |>
  rename(Total = n)

resolution(retirement$n)

ggplot(data = retirement,aes(x = Year,y=n)) +
  geom_bar(aes(x = Year,y=n,fill = teachingCategoryName), 
           stat="identity", position = "dodge")+
  geom_text(aes(label=n,group = teachingCategoryName),
            position = position_dodge(1))+
  scale_fill_manual(name = NULL, values = c("PRE_PRIMARY" = "#FF9130",
                                            "PRIMARY" = "#3876BF",
                                            "SECONDARY" = "black"))+
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank())+
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),  #remove x axis ticks
    axis.text.y = element_text(size=10, face="bold", colour = "black"),
    legend.box = "horizontal",
    legend.position = "bottom"
  ) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2000)) 

  