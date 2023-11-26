rm(list = ls())

library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)
library(haven)
#install.packages("openxlsx")
library(here)

# set working directory ####
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

# visualization

res <- resolution(dt_age_level$n)
Age <- dt_age_level$age_categ

ggplot(data = dt_age_level, aes(x = age_categ, y = n,group = teachingCategoryName)) +
  geom_bar(
    aes(fill = teachingCategoryName),
    stat = "identity",
    position = "dodge"
  ) +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = n),
            position = position_dodge(.9),
            size = 2.5,
            vjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teaching level by age groups") +
  # scale_fill_manual(
  #   name = NULL,
  #   values = c(
  #     "PRE_PRIMARY" = "#5272F2",
  #     "PRIMARY" = "#B4B4B3",
  #     "SECONDARY" = "#0174BE",
  #     "Total" = "#4F709C"
  #   )
  # )  +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = rel(.8))
  ) +
  theme(
    plot.background = element_rect(fill = c("#ECE5C7")),
    panel.background = element_rect(fill = c("#ECE5C7")),
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
    legend.position = "bottom",
    legend.background = element_rect(fill = c("#ECE5C7")),
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))+ 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
ggsave("04_reporting/02_visuals/level_age.png",
       units = "px",width = 2000,height = 1000,dpi = 100,
       device = "png")

# cross reference age categories with positions ####

# we need to save "tmis_filter" to dta format and categorize positions using STATA
write_dta(tmis_filter,"03_clean_data/tmis_filter.dta")

# after assigning labels to position now we can start our categorization
tmis_label <- read_dta("03_clean_data/tmis_filter.dta")
sum(is.na(tmis_label$class)) # there is no missing class value
tmis_label <- tmis_label |>
  mutate(class = as.character(class),
    subject = case_when(
    class %in% c(
      "3",
      "4",
      "5",
      "6",
      "7",
      "8",
      "9",
      "12",
      "13",
      "14",
      "15",
      "16",
      "17",
      "54",
      "55",
      "56",
      "57",
      "58",
      "59",
      "60",
      "61",
      "71",
      "72",
      "74",
      "86",
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
      "105",
      "106",
      "107",
      "108",
      "109",
      "110",
      "120",
      "121",
      "122",
      "123",
      "124",
      "125",
      "126",
      "127",
      "128",
      "134",
      "135",
      "136"
    ) ~ "STEM",
    class %in% c(
      "26",
      "27",
      "28",
      "29",
      "30",
      "31",
      "32",
      "33",
      "34",
      "35",
      "44",
      "45",
      "46",
      "47",
      "48",
      "49",
      "50",
      "51",
      "52",
      "53",
      "75",
      "76",
      "77",
      "78",
      "79",
      "80",
      "81",
      "82",
      "83",
      "84",
      "85",
      "87",
      "91",
      "92",
      "93"
    ) ~ "Languages",
    class %in% c(
      "18",
      "19",
      "65",
      "68",
      "69",
      "70",
      "114",
      "115",
      "116",
      "130",
      "131",
      "132",
      "133",
      "140",
      "141",
      "143"
    ) ~ "Humanities",
    class %in% c("24", 
                 "25", 
                 "36",
                 "37",
                 "38",
                 "39",
                 "40",
                 "41",
                 "66",
                 "67") ~ "Finance",
    class %in% c("43") ~ "Education",
    class %in% c("42","73","142") ~ "Special Needs Educ",
    class %in% c("10","11") ~ "Bursar",
    class %in% c("1","2") ~ "Accountant Secretary",
    class %in% c("137","138","139") ~ "Secretary",
    class %in% c("94","129") & teachingCategoryName == "PRE_PRIMARY" ~ "Nursery",
    class %in% c("94","129") & teachingCategoryName == "PRIMARY" ~ "Primary",
    class %in% c("20","21","22","23") ~ "Deputy Headteacher",
    class %in% c("88","89","90") ~ "Librarian",
    class %in% c("62","63","64") ~ "Headteacher",
    class %in% c("111","112","113") ~ "Matron",
    class %in% c("117","118","119") ~ "Patron"
  ))
dt_age_position <- count(tmis_label,age_categ,subject)
dt_age_position <- as.data.table(dt_age_position)
dt_age_position <- dt_age_position[dt_age_position$age_categ!="",]
cross_age_position <- dcast(dt_age_position,
                         subject~age_categ,value.var = "n")

write.xlsx(cross_age_position,
           "04_reporting/01_tables/updated/cross_age_position.xlsx",
           asTable = T)
# view(count(tmis_label,position,class,teachingCategoryName))

# Visualization

dt_age_position_vis <- copy(dt_age_position)
dt_age_position_vis[,Total_count := ifelse(n > 700,n,NA_integer_)]
dt_age_position_vis[,subject := str_wrap(subject,12)]
res <- resolution(dt_age_position_vis$Total_count)
text_color <- "#000000"
ggplot(data = dt_age_position_vis, aes(x = subject, 
                                       y = n, group = n)) +
  geom_bar(aes(fill = age_categ),
           stat = "identity",
           position = "stack") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = ifelse(n>700,n,NA)),
            position = position_stack(vjust = .5),
            size = 2.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teachers' Age by Subject/position") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.y = element_text(face="bold",color = "#245953", size = rel(.8))
  ) +
  theme(
    plot.background = element_rect(fill = c("#ECE5C7")),
    panel.background = element_rect(fill = c("#ECE5C7")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    #remove x axis ticks
    axis.text.x = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("#ECE5C7")),
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))+
  coord_flip()
ggsave("04_reporting/02_visuals/position_age.png",
       units = "px",width = 2000,height = 1000,dpi = 100,
       device = "png")

# visualization of only teaching position
'%notin%' <- function(x,y)!('%in%'(x,y))
dt_teaching_position <-
  dt_age_position[dt_age_position$subject %notin% c("Secretary",
                                                    "Librarian",
                                                    "Deputy Headteacher",
                                                    "Accountant Secretary"),]
ggplot(data = dt_teaching_position, aes(x = subject, 
                                       y = n, group = n)) +
  geom_bar(aes(fill = age_categ),
           stat = "identity",
           position = "stack") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = ifelse(n>700,n,NA)),
            position = position_stack(vjust = .5),
            size = 2.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teachers' Age by Subject") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.y = element_text(face="bold",color = "#245953", size = rel(.8))
  ) +
  theme(
    plot.background = element_rect(fill = c("#ECE5C7")),
    panel.background = element_rect(fill = c("#ECE5C7")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    #remove x axis ticks
    axis.text.x = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("#ECE5C7")),
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))+
  coord_flip()
ggsave("04_reporting/02_visuals/teacher_subject_age.png",
       units = "px",width = 2000,height = 1000,dpi = 100,
       device = "png")

# cross reference age categories with leadership roles ####

dt_age_role <- count(tmis_filter,age_categ,role)
dt_age_role <- as.data.table(dt_age_role)
dt_age_role <- dt_age_role[!is.na(dt_age_role$age_categ),]
cross_age_role <- dcast(dt_age_role,
                            role~age_categ,value.var = "n")

write.xlsx(cross_age_role,
           "04_reporting/01_tables/updated/cross_age_role.xlsx",
           asTable = T)

# visuals

ggplot(data = dt_age_role, aes(x = role, 
                               y = n, group = n)) +
  geom_bar(aes(fill = age_categ),
           stat = "identity",
           position = "dodge") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = ifelse(n>=100,n,NA)),
            position = position_dodge(.9),
            size = 1.8,
            hjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("School Staff's Age by Position") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.y = element_text(face="bold",color = "#245953", size = rel(.8))
  ) +
  theme(
    plot.background = element_rect(fill = c("#ECE5C7")),
    panel.background = element_rect(fill = c("#ECE5C7")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    #remove x axis ticks
    axis.text.x = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("#ECE5C7")),
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))+
  coord_flip()

ggsave("04_reporting/02_visuals/staff_age.png",
       units = "px",width = 2000,height = 1000,dpi = 100,
       device = "png")

# Teachers' age visualisation

dt_age_teacher <- dt_age_role[dt_age_role$role=="Teacher",]

ggplot(data = dt_age_teacher, aes(x = age_categ, 
                               y = n, group = n)) +
  geom_bar(stat = "identity",fill = "#3B9AE1") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = n),
            position = position_dodge(.9),
            size = 2.8,
            vjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teachers' Age") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = rel(1))
  ) +
  theme(
    plot.background = element_rect(fill = c("#ECE5C7")),
    panel.background = element_rect(fill = c("#ECE5C7")),
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
    legend.position = "bottom",
    legend.background = element_rect(fill = c("#ECE5C7")),
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))+ 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))

ggsave("04_reporting/02_visuals/teacher_age.png",
       units = "px",width = 2000,height = 1000,dpi = 100,
       device = "png")



# cross reference age categories with qualifications ####

dt_age_qualification <-
  count(tmis_filter, age_categ, qualificationLevel)
dt_age_qualification <- as.data.table(dt_age_qualification)
dt_age_qualification <-
  dt_age_qualification[!is.na(dt_age_qualification$age_categ), ]
cross_age_qualification <- dcast(dt_age_qualification,
                        qualificationLevel ~ age_categ, value.var = "n")

write.xlsx(cross_age_qualification,
           "04_reporting/01_tables/updated/cross_age_qualification.xlsx",
           asTable = T)

# visuals

ggplot(data = dt_age_qualification, aes(x = age_categ, 
                                        y = n, group = qualificationLevel)) +
  geom_bar(aes(fill = qualificationLevel),
           stat = "identity",
           position = "dodge") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = n),
            position = position_dodge(.9),
            size = 2.5,
            vjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teacher's Age by Education Attainment") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = rel(.8))
  ) +
  theme(
    plot.background = element_rect(fill = c("#ECE5C7")),
    panel.background = element_rect(fill = c("#ECE5C7")),
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
    legend.position = "bottom",
    legend.background = element_rect(fill = c("#ECE5C7")),
    legend.title = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 1))+ 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))


ggsave("04_reporting/02_visuals/age_qualification.png",
       units = "px",width = 2000,height = 1000,dpi = 100,
       device = "png")


# Teachers' age projection (2024-2030) ####

tmis_age <- tmis_df |>
  select(employeeid,gender,year_birth,position,qualificationLevel,teachingCategoryName,
         role,schoolName,sectorName,districtName)

for (years in 2023:2030){
  varname <- as.character(years)
  tmis_age <- tmis_age |>
    dplyr::mutate(!! varname := as.integer(years) - as.integer(year_birth))
}

tmis_age_long <- tmis_age |>
  pivot_longer(
    cols = -c(employeeid:districtName),
    names_to = "Year",
    values_to = "Age"
  ) |>
  mutate(Year = as.integer(Year))

# Identify French teachers

tmis_age_long <- tmis_age_long |>
  mutate(french_teacher = ifelse(str_detect(position,"(?i)french"),1,0),
         kiswahili_teacher = ifelse(str_detect(position,"(?i)kiswahili"),1,0),
         music_teacher = ifelse(str_detect(position,"(?i)music"),1,0),
         accounting_teacher = ifelse(str_detect(position,"(?i)accounting"),1,0),
         entr_teacher = ifelse(str_detect(position,"(?i)entrepreneurship"),1,0))

# save dataset

write.xlsx(tmis_age_long,
          "04_reporting/01_tables/updated/tmis_age_projection.xlsx",
          asTable = T)

# create datasets for subjects with teachers with retirement age #### 

columns_filter <- c("french_teacher", "kiswahili_teacher", "music_teacher",
                    "accounting_teacher","entr_teacher")

for (columns in columns_filter) {
  assign(columns,count(filter(tmis_age_long,Age>=65,get(columns)!=0),Year,
                       teachingCategoryName,
                       get(columns),name = "Total"))
  assign(columns,select(get(columns),Year,teachingCategoryName,Total))
  write.xlsx(get(columns),
             paste0("04_reporting/01_tables/updated/languages/",columns,".xlsx"),
             asTable = T)
}



tmis_primary <-
  count(
    filter(
      tmis_age_long,
      teachingCategoryName %in% c("PRE_PRIMARY", "PRIMARY"),
      Age >= 65
    ),
    Year,
    name = "Total"
  )
write.xlsx(tmis_primary,
           "04_reporting/01_tables/updated/tmis_primary.xlsx",
           asTable = T)




# Teaching Positions from Primary ####

primary_secondary_position <- tmis_label |>
  mutate(subject = ifelse(subject %in% c("Arts",
                                         "General studies and communication"),
                          "Humanities",
                          ifelse(subject %in% c("Lower Primary",
                                                "Normal Primary"),"Primary",
                                 subject)))

# save dataset (Primary)
primary_position <- count(filter(primary_secondary_position,
                                 teachingCategoryName=="PRIMARY"),subject)