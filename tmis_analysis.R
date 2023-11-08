#######################
# TMIS ANALYSIS#
####################
rm(list = ls()) # removing all objects to start with a clean slate
library(foreign)
library(haven)
library(tidyverse)
library(scales) 
library(rio)
library(labelled)
library(readxl)
library(data.table)
library(sjlabelled)
library(openxlsx)

setwd("C:/Users/HP/source/repos/r-datascience")
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

x <- 2023:2030
y <- ret_binded[ret_binded$teachingCategoryName=="Total","Total"]$Total 
mod <- lm(y~x)
coef(mod)

resolution(ret_binded$Total)
Year <- 2023:2030

ggplot(data = ret_binded,aes(x = Year,y=Total)) +
  geom_bar(aes(x = Year,y=Total,fill = teachingCategoryName), 
           stat="identity", position = "dodge")+
  geom_text(aes(label=Total,group = teachingCategoryName),
            position = position_dodge(.9), size = 3, hjust = .6, 
            vjust = -.3,fontface="bold",color="#232D3F")+
  ggtitle("Projected Retirement 2023-2030")+
  scale_fill_manual(name = NULL, values = c("PRE_PRIMARY" = "#5272F2",
                                            "PRIMARY" = "#B4B4B3",
                                            "SECONDARY" = "#0174BE",
                                            "Total" = "#4F709C"))+
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
    axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom"
  ) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2500))  +
  scale_x_continuous(name = NULL, labels = as.character(Year), breaks = Year)+ 
  geom_abline(slope = 318.369,intercept = -644273.750,col="#4F709C") 

ret_arranged<-ret_binded |>
       arrange(Year)
write.xlsx(ret_arranged,"01_tables//retirement_trend.xlsx",asTable = T)

# Retirement by qualification and gender
ret_qual <- df_filter |>
  filter(age >= 65) |>
  select(gender,qualificationLevel) |>
  group_by(qualificationLevel,gender) |>
  summarise(Total = n())
write.xlsx(ret_qual,"01_tables//retirement_qualification.xlsx",asTable = T)

ret_qual[nrow(ret_qual)+1,"gender"] <- "Female"
ret_qual[nrow(ret_qual),"Total"] <- 0
ret_qual[nrow(ret_qual),"qualificationLevel"] <- "A1"

resolution(ret_qual$Total)

ggplot(data = ret_qual,aes(x = qualificationLevel,y=Total)) +
  geom_bar(aes(x = qualificationLevel,y=Total,fill = gender), 
           stat="identity", position = "dodge")+
  geom_text(aes(label=Total,group = gender),
            position = position_dodge(1), size = 3, hjust = .6, 
            vjust = -.3,fontface="bold",color="#232D3F")+
  ggtitle("Retirement Age by Qualification")+
  scale_fill_manual(name = NULL, values = c("Female" = "#FF9130",
                                            "Male" = "#3876BF"))+
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
    axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom"
  ) 

# New age brackets####
######################

setwd("C:/Users/HP/Box/IPA_RWA_Project_STARS/07_Data/31_TMIS_analysis/04_reporting/")



df_age_brackets <- df |>
  unite("Name",lastName:firstName,sep = " ") |>
  select(Name,gender,teachingCategoryName,year_birth) 

for (years in 2023:2030){
  varname <- as.character(years)
  df_age_brackets <- df_age_brackets |>
    dplyr::mutate(!! varname := as.integer(years) - as.integer(year_birth))
}

df_age_bracket_long <- df_age_brackets |>
  pivot_longer(
    cols = -c(Name:year_birth),
    names_to = "Year",
    values_to = "Age"
  ) |>
  mutate(Year = as.integer(Year))

write.xlsx(df_age_bracket_long,
           "01_tables//retirement_prediction.xlsx",
           asTable = T)

# adding age brackets

df_age_filter <- df_age_bracket_long |>
  mutate(Age_brackets = if_else(Age >= 70, "70 and above",
                                if_else(
                                  Age >= 65, "65-70",
                                  if_else(Age >= 60, "60-64",
                                          if_else(
                                            Age >= 56, "56-59",
                                            if_else(Age >= 50, "50-55", "Below 50")
                                          ))
                                )))

# Aggregation based on age

df_age_2023 <- dplyr::count(dplyr::filter(df_age_filter,Age>=50,Year == 2023),
             teachingCategoryName, Age_brackets) |>
  pivot_wider(id_cols = teachingCategoryName, names_from = Age_brackets,
              values_from = n)

write.xlsx(df_age_2023,
           "01_tables//retirement_count_2023.xlsx",
           asTable = T)
df_age_all <- dplyr::count(dplyr::filter(df_age_filter,Age>=50),
                           Year,teachingCategoryName, Age_brackets) |>
  pivot_wider(id_cols = c(Year,teachingCategoryName), names_from = Age_brackets,
              values_from = n)

write.xlsx(df_age_all,
           "01_tables//retirement_count_all.xlsx",
           asTable = T)

# Retirement analysis considering teachers' qualifications####
# Teachers are those occupying these roles: Teachers, DoD, Dos and headteachers
##############################################################

setwd("C:/Users/HP/Box/IPA_RWA_Project_STARS/07_Data/31_TMIS_analysis/04_reporting/")



df_age_brackets_teachers <- df |>
  select(employeeid,gender,year_birth,qualificationLevel,teachingCategoryName,
         role,schoolName,sectorName,districtName) 

for (years in 2023:2030){
  varname <- as.character(years)
  df_age_brackets_teachers <- df_age_brackets_teachers |>
    dplyr::mutate(!! varname := as.integer(years) - as.integer(year_birth))
}

df_age_brackets_teachers_long <- df_age_brackets_teachers |>
  pivot_longer(
    cols = -c(employeeid:districtName),
    names_to = "Year",
    values_to = "Age"
  ) |>
  mutate(Year = as.integer(Year))

#write.xlsx(df_age_brackets_teachers_long,
#           "01_tables//retirement_data.xlsx",
#           asTable = T)

# Data analysis by considering teachers only ####
#################################################

# Retirement by qualification and gender and teachers
ret_qual_teacher <- df_age_brackets_teachers_long |>
  filter(Age >= 65,
         Year == 2023,
         role %in% c("Teacher", "DOD", "DOS", "Head Teacher")) |>
  select(gender, qualificationLevel) |>
  group_by(qualificationLevel, gender) |>
  summarise(Total = n())
write.xlsx(ret_qual_teacher,
           "01_tables//retirement_qualification_teachers.xlsx",
           asTable = T)

ret_qual_teacher[nrow(ret_qual_teacher) + 1, "gender"] <- "Female"
ret_qual_teacher[nrow(ret_qual_teacher), "Total"] <- 0
ret_qual_teacher[nrow(ret_qual_teacher), "qualificationLevel"] <-
  "A1"

resolution(ret_qual_teacher$Total)

ggplot(data = ret_qual_teacher, aes(x = qualificationLevel, y = Total)) +
  geom_bar(
    aes(x = qualificationLevel, y = Total, fill = gender),
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    aes(label = Total, group = gender),
    position = position_dodge(1),
    size = 3,
    hjust = .6,
    vjust = -.3,
    fontface = "bold",
    color = "#232D3F"
  ) +
  ggtitle("Retirement Age by Qualification") +
  scale_fill_manual(name = NULL,
                    values = c("Female" = "#FF9130",
                               "Male" = "#3876BF")) +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    #remove x axis ticks
    axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom"
  )


retirement_teacher <-
  dplyr::count(
    dplyr::filter(df_age_brackets_teachers_long,
                  Age >= 65, role == "Teacher"),
    Year,
    teachingCategoryName
  ) |>
  ungroup()

retirement_bind_teacher <- df_age_brackets_teachers_long |>
  dplyr::filter(Age >= 65, role == "Teacher") |>
  group_by(Year) |>
  mutate(Total = n()) |>
  select(Year, Total) |>
  rename(n = Total) |>
  group_by(Year) |>
  mutate(count = n(), dup = ifelse(count == 1, 1, row_number())) |>
  filter(dup == 1) |>
  mutate(dup = NULL, count = NULL)

ret_binded_teacher <-
  rbind(
    retirement_teacher,
    mutate(retirement_bind_teacher,
           teachingCategoryName = NA_character_)
  ) |>
  mutate(
    teachingCategoryName = if_else(is.na(teachingCategoryName), "Total",
                                   teachingCategoryName),
    Total = NULL
  ) |>
  rename(Total = n) |>
  arrange(Year)

x <- 2023:2030
y <-
  ret_binded_teacher[ret_binded_teacher$teachingCategoryName == "Total", "Total"]$Total
mod <- lm(y ~ x)
mod.coef <- coef(mod)

res <- resolution(ret_binded_teacher$Total)
Year <- 2023:2030

ggplot(data = ret_binded_teacher, aes(x = Year, y = Total)) +
  geom_bar(
    aes(x = Year, y = Total, fill = teachingCategoryName),
    stat = "identity",
    position = "dodge"
  ) +
  geom_text(
    aes(label = Total, group = teachingCategoryName),
    position = position_dodge(0.9),
    size = 3,
    hjust = .6,
    vjust = -.3,
    fontface = "bold",
    color = "#232D3F"
  ) +
  ggtitle("Projected Retirement 2023-2030") +
  scale_fill_manual(
    name = NULL,
    values = c(
      "PRE_PRIMARY" = "#5272F2",
      "PRIMARY" = "#B4B4B3",
      "SECONDARY" = "#0174BE",
      "Total" = "#4F709C"
    )
  ) +
  theme(plot.title = element_text(hjust = .5),
        axis.ticks = element_blank()) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    #remove x axis ticks
    axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom"
  ) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2500))  +
  scale_x_continuous(name = NULL,
                     labels = as.character(Year),
                     breaks = Year) +
  geom_abline(slope = mod.coef[["x"]],
              intercept = mod.coef[["(Intercept)"]],
              col = "#4F709C")

write.xlsx(ret_binded_teacher,
           "01_tables//retirement_trend_teacher.xlsx",
           asTable = T)



# Age brackets creation and filtering####

df_age_filter_teacher <- df_age_brackets_teachers_long |>
  mutate(Age_brackets = if_else(Age >= 70, "70 and above",
                                if_else(
                                  Age >= 65, "65-70",
                                  if_else(Age >= 60, "60-64",
                                          if_else(
                                            Age >= 56, "56-59",
                                            if_else(Age >= 50, "50-55", "Below 50")
                                          ))
                                )))

df_age_teachers <-
  dplyr::count(
    dplyr::filter(df_age_filter_teacher, Age >= 50,
                  role == "Teacher"),
    Year,
    teachingCategoryName,
    Age_brackets
  ) |>
  pivot_wider(
    id_cols = c(Year, teachingCategoryName),
    names_from = Age_brackets,
    values_from = n
  )

write.xlsx(df_age_teachers,
           "01_tables//retirement_count_teachers.xlsx",
           asTable = T)
