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
library(openxlsx)
library(data.table)
library(sjlabelled)


setwd("C:/Users/HP/Box/IPA_RWA_Project_STARS/07_Data/30_endof_ term3_Marks/04_output")
df <- read_dta("Histo_mean_marks.dta")
df_grouped <- df |>
  select(-c(student_obtained_marks,mean_school,mean_sect))|>
  group_by(sector_name,district_code,school_name,grade_name,course_name)|>
  summarise(mean_marks = max(mean_grade_course_sect))
ggplot(data = df_grouped,aes(x = mean_marks, y = sector_name, fill = course_name))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = mean_marks), size = 3, hjust = 0.5, 
            vjust = 3, position = "stack")+
  facet_wrap(~grade_name)


df_grouped_grade <- df |>
  select(-c(student_obtained_marks,mean_school,mean_sect,district_code,
            school_name,grade_name))|>
  group_by(sector_name,course_name)|>
  summarise(mean_marks_grade = mean(mean_grade_course_sect,na.rm = T))|>
  ungroup()|>
  filter(course_name%in%c("Mathematics","Kinyarwanda","English"))


ggplot(data = df_grouped_grade,aes(x = mean_marks_grade, y = sector_name, fill = course_name))+
  geom_bar(position="stack", stat="identity")+
  geom_text(aes(label = round(mean_marks_grade)), size = 3.8, hjust = 8, 
            vjust = -0, position = "stack")
ggplot()+
  geom_histogram(data = df_grouped[df_grouped$course_name=="English",],
                 aes(x = mean_marks),fill = "#379237",alpha = .6)+
  geom_histogram(data = df_grouped[df_grouped$course_name=="Kinyarwanda",],
                 aes(x = mean_marks),fill = "#F875AA",alpha = .6)+
  geom_histogram(data = df_grouped[df_grouped$course_name=="Mathematics",],
                 aes(x = mean_marks),fill = "#00A9FF",alpha = .6)



ggplot()+
  geom_histogram(data = df[df$course_name=="English",],
                 aes(x = mean_grade_course_sect,
                     fill = "English"),alpha = .9)+
  geom_histogram(data = df[df$course_name=="Kinyarwanda",],
                 aes(x = mean_grade_course_sect,
                     fill = "Kinyarwanda"),alpha = .9)+
  geom_histogram(data = df[df$course_name=="Mathematics",],
                 aes(x = mean_grade_course_sect,
                     fill = "Mathematics"),alpha = .9)+
  scale_fill_manual(name = "Subjects", values = c("English" = "#379237",
                                                  "Kinyarwanda" = "#F875AA",
                                                  "Mathematics" = "#00A9FF"))+
  ylab("Count")+
  xlab("Marks")+
  ggtitle("Distribution of Marks, by subject")+
  theme_light()

# The correct distribution histogram
ggplot(data = filter(df,course_name%in%c("Mathematics","Kinyarwanda","English")))+
  geom_histogram(aes(x = mean_grade_course_sect,
                     fill = course_name),position = "stack",alpha = .7)+
  ylab("Count")+
  xlab("Marks")+
  ggtitle("Distribution of Marks, by subject")+
  theme_light()
# instead of count we use proportions
ggplot(data = filter(df,course_name%in%c("Mathematics","Kinyarwanda","English")))+
  geom_histogram(aes(x = mean_grade_course_sect,y = after_stat(density),
                     fill = course_name),position = "stack",alpha = .7)+
  ylab("Count")+
  xlab("Marks")+
  ggtitle("Distribution of Marks, by subject")+
  theme_light()
