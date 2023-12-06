rm(list = ls(all.names = T))
# Import necessary libraries ####
library(tidyverse)
library(data.table)
library(readxl)
library(openxlsx)
library(haven)
#install.packages("openxlsx")
library(scales)

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
         age_categ = if_else(age >= 70, "70+",
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

# filter staff that is considered as teaching staff and non-teaching staff that 
# will be impacted with age related policies
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
    scale_fill_manual(name = NULL, values = c("Female" = "maroon",
                                              "Male" = "#3876BF"))+
    theme(plot.title = element_text(hjust = .5),
          axis.ticks = element_blank())+
    theme(
      plot.background = element_rect(fill = c("white")),
      panel.background = element_rect(fill = c("white")),
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

ggsave(
  "04_reporting/02_visuals/teacher_gender.png",
  units = "px",
  width = 1000,
  height = 1000,
  dpi = 150,
  device = "png"
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
  geom_text(aes(label = scales::comma(n)),
            position = position_dodge(.9),
            size = 4,
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
    axis.text.x = element_text(face="bold",color = "#245953", size = 15)
  ) +
  theme(
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
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
    legend.background = element_rect(fill = c("white")),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow = 1))+ 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
ggsave("04_reporting/02_visuals/level_age1.png",
       units = "px",width = 2500,height = 1700,dpi = 180,
       device = "png")

# cross reference age categories with positions and labelling (tmis_label) ####

# we need to save "tmis_filter" to dta format and categorize positions using STATA
#write_dta(tmis_filter,"03_clean_data/tmis_filter.dta")
# after saving the dataset, we wrote a STATA script to assign labels to different 
# categories of the position variable. the script can be found here: 
# Box\IPA_RWA_Project_STARS\07_Data\31_TMIS_analysis\01_scripts\tmis_position_assign_labels.do

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
                 "67") ~ "Economics",
    class %in% c("42","43") ~ "Education",
    class %in% c("73","142") ~ "Special Needs Educ",
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
ggplot(data = dt_age_position_vis, aes(x = age_categ, 
                                       y = n)) +
  geom_bar(aes(fill = age_categ),
           stat = "identity",
           position = "stack") +
  ggtitle("Teachers' Age by Subject/Position") +
  scale_fill_brewer(palette = "GnBu") +
  geom_text(aes(label = scales::comma(n)),
            position = position_stack(.9),
            vjust = -1,
            size = 3.4,
            color = "#000000",
            fontface = "bold")+
  #coord_flip()+
  facet_wrap(~subject,scales = "free")+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(angle = -45, vjust = -.8, hjust=0.5,
                                   face="bold",color = "#245953", size = 10),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = .5),
        plot.background = element_rect(fill = c("white")),
        panel.background = element_rect(fill = c("white")),
        panel.grid = element_blank(),
        legend.position="none"
  )+ 
  scale_y_continuous(expand = expansion(mult = c(0, .1))) +
  guides(fill = guide_legend(nrow = 1))

ggsave("04_reporting/02_visuals/position_age_wrap1.png",
       units = "px",width = 3100,height = 2500,dpi = 200,
       device = "png")

# visualization of only teaching position
'%notin%' <- function(x,y)!('%in%'(x,y))
dt_teaching_position <-
  dt_age_position[dt_age_position$subject %notin% c("Secretary",
                                                    "Librarian",
                                                    "Deputy Headteacher",
                                                    "Accountant Secretary",
                                                    "Bursar",
                                                    "Patron",
                                                    "Matron"),]
#####
# ggplot(data = dt_teaching_position, aes(x = subject, 
#                                        y = n, group = n)) +
#   geom_bar(aes(fill = age_categ),
#            stat = "identity",
#            position = "stack") +
#   scale_fill_brewer(palette = "Blues") +
#   geom_text(aes(label = ifelse(n>700,n,NA)),
#             position = position_stack(vjust = .5),
#             size = 2.5,
#             color = "#000000",
#             fontface = "bold") +
#   ggtitle("Teachers' Age by Subject") +
#   theme(
#     plot.title = element_text(hjust = .5),
#     axis.ticks = element_blank(),
#     axis.text.y = element_text(face="bold",color = "#245953", size = rel(.8))
#   ) +
#   theme(
#     plot.background = element_rect(fill = c("#ECE5C7")),
#     panel.background = element_rect(fill = c("#ECE5C7")),
#     panel.grid = element_blank(),
#     #remove x axis ticks
#     #axis.text.x = element_blank(),
#     axis.title.x = element_blank(),
#     axis.title.y = element_blank(),
#     #remove x axis labels
#     axis.ticks.x = element_blank(),
#     #remove x axis ticks
#     axis.text.x = element_blank(),
#     legend.box = "horizontal",
#     legend.position = "bottom",
#     legend.background = element_rect(fill = c("#ECE5C7")),
#     legend.title = element_blank()
#   ) +
#   guides(fill = guide_legend(nrow = 1))+
#   coord_flip()
#####
teaching_position_grouped <- dt_teaching_position |>
  group_by(subject) |>
  summarize(total_count = sum(n))
ggplot(data = teaching_position_grouped, aes(x = subject, 
                                             y = total_count,group = total_count)) +
  geom_bar(fill = "#3876BF",
           stat = "identity") +
  ggtitle("Teachers' Count by Subject") +
  scale_fill_brewer(palette = "GnBu") +
  geom_text(aes(label = scales::comma(total_count)),
            position = position_stack(.9),
            vjust = -1.9,
            size = 4.4,
            color = "#000000",
            fontface = "bold")+
  theme(axis.title = element_blank(),
        axis.text.x = element_text(face="bold",color = "#245953", size = 11),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = .5),
        plot.background = element_rect(fill = c("white")),
        panel.background = element_rect(fill = c("white")),
        panel.grid = element_blank(),
  )+ 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))
ggsave("04_reporting/02_visuals/teacher_count_age.png",
       units = "px",width = 2900,height = 1800,dpi = 200,
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
            size = 2.5,
            hjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("School Staff's Age by Position") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.y = element_text(face="bold",color = "#245953", size = rel(1))
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
  geom_text(aes(label = scales::comma(n)),
            position = position_dodge(.9),
            size = 4.4,
            vjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teachers' Age") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 15)
  ) +
  theme(
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
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

ggsave("04_reporting/02_visuals/teacher_age1.png",
       units = "px",width = 2000,height = 1000,dpi = 150,
       device = "png")

# Staff's age visualisation (ex Teachers)

dt_age_staff <- dt_age_role[dt_age_role$role!="Teacher",]
dt_age_staff <- dt_age_staff |>
  mutate(role = str_wrap(role,width = 10))

ggplot(data = filter(
  dt_age_staff,
  age_categ %in% c("50-54",
                   "55-59",
                   "60-64",
                   "65-69")
),
aes(x = role,
    y = n, group = n)) +
  geom_bar(aes(fill = age_categ),
           stat = "identity",
           position = "dodge") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(
    aes(label = n),
    position = position_dodge(.9),
    size = 4.4,
    hjust = .5,
    vjust = -0.2,
    color = "#000000",
    fontface = "bold"
  ) +
  ggtitle("School Staff's Age by Position") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(
      face = "bold",
      color = "#245953",
      size = 15
    )
  ) +
  theme(
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.y = element_blank(),
    #remove x axis ticks
    axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("white")),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow = 1))+ 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))


ggsave("04_reporting/02_visuals/staff_exteacher_age1.png",
       units = "px",width = 2000,height = 1000,dpi = 150,
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
  geom_text(aes(label = scales::comma(n)),
            position = position_dodge(.9),
            size = 4,
            vjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teacher's Age by Qualification-to-teach levels") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 15)
  ) +
  theme(
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
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
    legend.background = element_rect(fill = c("white")),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow = 1))+ 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))


ggsave("04_reporting/02_visuals/age_qualification_teach.png",
       units = "px",width = 2400,height = 1700,dpi = 180,
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
  mutate(
    french_teacher = ifelse(str_detect(position, "(?i)Lower primary"), "French", NA),
    kiswahili_teacher = ifelse(str_detect(position, "(?i)kiswahili"), "kiswahili", NA),
    music_teacher = ifelse(str_detect(position, "(?i)music"), "Music", NA),
    accounting_teacher = ifelse(str_detect(position, "(?i)accounting"), "Accounting", NA),
    entr_teacher = ifelse(
      str_detect(position, "(?i)entrepreneurship"),
      "Entrepreneurship",
      NA
    ),
    DOS = ifelse(teachingCategoryName == "SECONDARY" &
                   role == "DOS", "DOS", NA),
    DOD = ifelse(teachingCategoryName == "SECONDARY" &
                   role == "DOD", "DOD", NA),
    HT_DOS = ifelse(
      teachingCategoryName == "SECONDARY" &
        role %in% c("Head Teacher", "DOS"),
      "HT and DOS",
      NA
    )
  )
# view(filter(tmis_age_long,str_detect(position,"(?i)Lower primary")))

# save dataset

write.xlsx(tmis_age_long,
          "04_reporting/01_tables/updated/tmis_age_projection1.xlsx",
          asTable = T)

# create datasets for subjects with teachers with retirement age #### 

columns_filter <- c("french_teacher", "kiswahili_teacher", "music_teacher",
                    "accounting_teacher","entr_teacher","DOS","DOD","HT_DOS")

for (columns in columns_filter) {
  assign(columns,count(filter(tmis_age_long,Age>=50,get(columns)!=0),Year,
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


# Teaching Positions from Primary and Secondary ####

# Primary school

primary_position <-
  count(
    filter(tmis_label, teachingCategoryName == "PRIMARY"),
    subject,
    sort = T,
    name = "Total"
  )

# save dataset (Primary)
write.xlsx(
  primary_position,
  "04_reporting/01_tables/updated/primary_position.xlsx",
  asTable = T
)

primary_position_role <-
  count(
    filter(tmis_label, teachingCategoryName == "PRIMARY"),
    subject,
    role,
    sort = T,
    name = "Total"
  )

write.xlsx(
  primary_position_role,
  "04_reporting/01_tables/updated/primary_position_role.xlsx",
  asTable = T
)

# Secondary School

secondary_position <-
  count(
    filter(tmis_label, teachingCategoryName == "SECONDARY"),
    subject,
    sort = T,
    name = "Total"
  )

# save dataset (Primary)
write.xlsx(
  secondary_position,
  "04_reporting/01_tables/updated/secondary_position.xlsx",
  asTable = T
)

secondary_position_role <-
  count(
    filter(tmis_label, teachingCategoryName == "SECONDARY"),
    subject,
    role,
    sort = T,
    name = "Total"
  )

write.xlsx(
  secondary_position_role,
  "04_reporting/01_tables/updated/secondary_position_role.xlsx",
  asTable = T
)


# Retirement by Position (tmis_ret_subj) ####

# join tmis_label with tmis_age_long to append the subject variable
tmis_ret_subj <- tmis_age_long |>
  left_join(select(tmis_label,employeeid,subject))
tmis_ret_subj_filter <- filter(tmis_ret_subj,Age >= 50,
                               subject %in% c("Headteacher","Languages",
                                              "Primary","STEM"))
# save dataset (Primary)
write.xlsx(
  tmis_ret_subj_filter,
  "04_reporting/01_tables/updated/tmis_ret_subj_filter1.xlsx",
  asTable = T
)

ret_subj_filter <- tmis_ret_subj_filter |>
  mutate(Age_brackets = if_else(Age >= 70, "70+",
                                if_else(
                                  Age >= 65, "65-69",
                                  if_else(Age >= 60, "60-64",
                                          if_else(
                                            Age >= 55, "55-59",
                                            if_else(Age >= 50, "50-54", "Below 50")
                                          ))
                                )))|>
  group_by(Year,subject,Age_brackets)|>
  mutate(total_count = n())|>
  ungroup()|>
  filter(Age>=65)

ggplot(data = count(ret_subj_filter,Year,subject),
       aes(x = Year,y = n,group=subject)) +
  geom_bar(stat = "identity",position = "dodge",aes(fill=subject)) + 
  geom_text(aes(label = n),
            position = position_dodge(.9),
            size = 4,
            vjust=-.5,
            color = "#000000",
            fontface = "bold")+
  scale_fill_brewer(palette = "Blues")+
  scale_x_continuous(breaks=seq(2023,2030,1))+ 
  scale_y_continuous(expand = expansion(mult = c(0, .1)))+
  ggtitle("Retirement by Position")+
  theme(
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    plot.title = element_text(hjust = 0.5),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    #axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 15),
    # axis.text.y = element_text(face="bold",color = "#245953", size = 20),
    #remove x axis ticks
    #axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("white")),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("04_reporting/02_visuals/retirement_position_65.png",
       units = "px",width = 2000,height = 1000,dpi = 150,
       device = "png")


# Age distribution by Role ####
tmis_age_brackets <- tmis_age_long |>
  mutate(Age_brackets = if_else(Age >= 70, "70+",
                                if_else(
                                  Age >= 65, "65-69",
                                  if_else(Age >= 60, "60-64",
                                          if_else(
                                            Age >= 55, "55-59",
                                            if_else(Age >= 50, "50-54", "Below 50")
                                          ))
                                )))
age_role <-
  count(filter(tmis_age_brackets,Age>=50),
        role,
        Age_brackets,
        sort = T,
        name = "Total") |>
  pivot_wider(id_cols = role,
              names_from = Age_brackets,
              values_from = Total)

# save dataset (Primary)
write.xlsx(
  age_role,
  "04_reporting/01_tables/updated/age_role.xlsx",
  asTable = T
)

write.xlsx(tmis_age_brackets,
           "04_reporting/01_tables/updated/tmis_age_brackets_position1.xlsx",
           asTable = T)


tmis_wider_projection <- filter(tmis_age_brackets, Age >= 50) |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    entr_teacher,
    french_teacher,
    kiswahili_teacher,
    music_teacher,
    accounting_teacher,
    DOS,
    HT_DOS,
    DOD,
    name = "Total count"
  )

write.xlsx(tmis_wider_projection,
           "04_reporting/01_tables/updated/tmis_wider_projection1.xlsx",
           asTable = T)


entr_temp<-filter(tmis_age_brackets, Age >= 50) |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    entr_teacher,
   name = "Total count"
  )|>
  filter(!is.na(entr_teacher))|>
  mutate(unit_cost = ifelse(Year==2023,55798,
                            ifelse(Year == 2024, 143148,
                                   ifelse(Year == 2025, 142512,
                                          ifelse(Year == 2026,146787,
                                                 ifelse(Year == 2027,151191,
                                                        ifelse(Year == 2028,155726,
                                                               ifelse(Year == 2029,160398,
                                                                      ifelse(Year == 2030,165210,NA))))))))) |>
  rename(trained_teacher=entr_teacher)

music_temp <- filter(tmis_age_brackets, Age >= 50) |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    music_teacher,
    name = "Total count"
  )|>
  filter(!is.na(music_teacher))|>
  mutate(unit_cost = ifelse(Year==2023,13841,
                            ifelse(Year == 2024, 197393,
                                   ifelse(Year == 2025, 203315,
                                          ifelse(Year == 2026,209415,
                                                 ifelse(Year == 2027,215697,
                                                        ifelse(Year == 2028,222168,
                                                               ifelse(Year == 2029,228833,
                                                                      ifelse(Year == 2030,235698,NA))))))))) |>
  rename(trained_teacher=music_teacher)

acc_temp <- filter(tmis_age_brackets, Age >= 50) |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    accounting_teacher,
    name = "Total count"
  )|>
  filter(!is.na(accounting_teacher))|>
  mutate(unit_cost = ifelse(Year==2023,54562,
                            ifelse(Year == 2024, 143148,
                                   ifelse(Year == 2025, 143471,
                                          ifelse(Year == 2026,147775,
                                                 ifelse(Year == 2027,152209,
                                                        ifelse(Year == 2028,156775,
                                                               ifelse(Year == 2029,161478,
                                                                      ifelse(Year == 2030,166323,NA))))))))) |>
  rename(trained_teacher=accounting_teacher)

ksw_temp <- filter(tmis_age_brackets, Age >= 50) |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    kiswahili_teacher,
    name = "Total count"
  )|>
  filter(!is.na(kiswahili_teacher))|>
  mutate(unit_cost = ifelse(Year==2023,32995,
                            ifelse(Year == 2024, 33985,
                                   ifelse(Year == 2025, 35004,
                                          ifelse(Year == 2026,36054,
                                                 ifelse(Year == 2027,37136,
                                                        ifelse(Year == 2028,38250,
                                                               ifelse(Year == 2029,39398,
                                                                      ifelse(Year == 2030,40579,NA))))))))) |>
  rename(trained_teacher=kiswahili_teacher)

french_temp <- filter(tmis_age_brackets, Age >= 50,
                      teachingCategoryName=="PRIMARY") |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    french_teacher,
    name = "Total count"
  )|>
  filter(!is.na(french_teacher))|>
  mutate(unit_cost = ifelse(Year==2023,15824,
                            ifelse(Year == 2024, 162930,
                                   ifelse(Year == 2025, 167457,
                                          ifelse(Year == 2026,172480,
                                                 ifelse(Year == 2027,177655,
                                                        ifelse(Year == 2028,182985,
                                                               ifelse(Year == 2029,188474,
                                                                      ifelse(Year == 2030,194128,NA))))))))) |>
  rename(trained_teacher=french_teacher) 

prim_temp  <- filter(tmis_age_brackets, Age >= 50,
                     teachingCategoryName%in%c("PRE_PRIMARY","PRIMARY")) |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    name = "Total count"
  )|>
  mutate(unit_cost = ifelse(Year==2023,16532.8,
                            ifelse(Year == 2024, 138643.9,
                                   ifelse(Year == 2025, 138643.26,
                                          ifelse(Year == 2026,138643.26,
                                                 ifelse(Year == 2027,138643.26,
                                                        ifelse(Year == 2028,138643.26,
                                                               ifelse(Year == 2029,138643.26,
                                                                      ifelse(Year == 2030,138643.26,NA)))))))),
         trained_teacher = "Pre_primary and Primary")

dos_temp <- filter(tmis_age_brackets, Age >= 50,
                   teachingCategoryName=="SECONDARY") |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    DOS,
    name = "Total count"
  )|>
  filter(!is.na(DOS))|>
  mutate(unit_cost = ifelse(Year==2023,31772,
                            ifelse(Year == 2024, 83000,
                                   ifelse(Year == 2025, 96817,
                                          ifelse(Year == 2026,112933,
                                                 ifelse(Year == 2027,131732,
                                                        ifelse(Year == 2028,153661,
                                                               ifelse(Year == 2029,179240,
                                                                      ifelse(Year == 2030,209077,NA))))))))) |>
  rename(trained_teacher=DOS)


dod_temp <- filter(tmis_age_brackets, Age >= 50,
                   teachingCategoryName=="SECONDARY") |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    DOD,
    name = "Total count"
  )|>
  filter(!is.na(DOD))|>
  mutate(unit_cost = ifelse(Year==2023,149672,
                            ifelse(Year == 2024, 94419,
                                   ifelse(Year == 2025, 87836,
                                          ifelse(Year == 2026,80560,
                                                 ifelse(Year == 2027,73887,
                                                        ifelse(Year == 2028,67767,
                                                               ifelse(Year == 2029,62154,
                                                                      ifelse(Year == 2030,57006,NA))))))))) |>
  rename(trained_teacher=DOD)


ht_dos_temp <- filter(tmis_age_brackets, Age >= 50,
                   teachingCategoryName=="SECONDARY") |>
  count(
    Year,
    teachingCategoryName,
    qualificationLevel,
    Age_brackets,
    HT_DOS,
    name = "Total count"
  )|>
  filter(!is.na(HT_DOS))|>
  mutate(unit_cost = ifelse(Year==2023,24814.4,
                            ifelse(Year == 2024, 185787.27,
                                   ifelse(Year == 2025, 185638.3,
                                          ifelse(Year == 2026,183950.7,
                                                 ifelse(Year == 2027,182278.4,
                                                        ifelse(Year == 2028,180621.35,
                                                               ifelse(Year == 2029,178979.3,
                                                                      ifelse(Year == 2030,177352.25,NA))))))))) |>
  rename(trained_teacher=HT_DOS)

projection_data <-
  rbind(entr_temp,
        french_temp,
        ksw_temp,
        acc_temp,
        prim_temp,
        dos_temp,
        dod_temp,
        ht_dos_temp,
        music_temp)

# constraining french and Entr to not exceed the number of teachers to be trained

projection_data <- projection_data |>
  rename(total_count = `Total count`) |>
  group_by(Year, trained_teacher) |>
  arrange(Year,
          trained_teacher,
          Age_brackets,
          qualificationLevel,
          teachingCategoryName) |>
  ungroup() |>
  mutate(
    total_count = ifelse(
      Age_brackets %in% c("50-54", "55-59") &
        trained_teacher == "Entrepreneurship" &
        Year > 2024,
      ifelse(
        Year == 2025 & qualificationLevel == "A1" &
          Age_brackets == "50-54",
        41,
        ifelse(
          Year == 2026 & qualificationLevel == "A1" &
            Age_brackets == "50-54",
          1,
          ifelse(
            Year == 2027 & qualificationLevel == "A0" &
              Age_brackets == "50-54",
            69,
            ifelse(
              Year == 2028 & qualificationLevel == "A0" &
                Age_brackets == "50-54",
              31,
              ifelse(
                Year == 2029 & qualificationLevel == "A0" &
                  Age_brackets == "50-54",
                22,
                ifelse(
                  Year == 2029 & qualificationLevel == "A1" &
                    Age_brackets == "50-54",
                  17,
                  ifelse(
                    Year == 2030 & qualificationLevel %in% c("A0", "A1") &
                      Age_brackets == "50-54",
                    0,
                    ifelse(
                      Year == 2030 & qualificationLevel == "A1" &
                        Age_brackets == "55-59",
                      42,
                      total_count
                    )
                  )
                )
              )
            )
          )
        )
      ),total_count),
    total_count = ifelse(
      trained_teacher == "French",
      ifelse(
        qualificationLevel == "A2" &
          Age_brackets %in% c("50-54","55-59"),
        0,
        ifelse(Year == 2027 & 
               Age_brackets == "60-64",
               total_count - 22,
               ifelse(Year == 2028 & 
                      Age_brackets == "60-64",
                      total_count - 180,
                      ifelse(Year == 2029 & 
                               Age_brackets == "60-64",
                             total_count - 344,
                             ifelse(Year == 2030 & 
                                      Age_brackets == "60-64",
                                    total_count - 576,total_count))))
      ),total_count),
    total_cost = total_count * unit_cost)|>
  group_by(Year, trained_teacher) |>
  mutate(total_year = sum(total_count,na.rm = T) ) |>
  ungroup()

view(filter(
  projection_data,
  trained_teacher %in% c("French", "Entrepreneurship")
))

write.xlsx(projection_data,
           "04_reporting/01_tables/updated/projection_data1.xlsx",
           asTable = T)



# Budget implication aggregating on teaching level ####
projection_data_level <- projection_data |>
  group_by(Year,teachingCategoryName)|>
  summarize(total_cost_level = sum(total_cost))

#visualization

ggplot(data = projection_data_level,
       aes(x = Year,y=total_cost_level,group = teachingCategoryName,
           fill=teachingCategoryName)) +
  geom_bar(position = "dodge",stat = "identity") +
  scale_fill_brewer(palette = "Blues")+
  scale_x_continuous(breaks=seq(2023,2030,1))+ 
  ylim(c(0,3100000000)) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = label_comma())+
  ggtitle("Budget implication by teaching level") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( linewidth =.1, color="black" ) ,
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )+
  theme(
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    plot.title = element_text(hjust = 0.5),
    #panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 20),
    axis.text.y = element_text(face="bold",color = "#245953", size = 20),
    #remove x axis ticks
    #axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("white")),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("04_reporting/02_visuals/retirement_position_50_new1.png",
       units = "px",width = 2000,height = 1000,dpi = 150,
       device = "png")





# Budget implication aggregating on training program ####
#visualization
options(scipen = 999)
projection_data_year <- projection_data|>
  group_by(Year,trained_teacher)|>
  summarize(total_trained_year = sum(total_cost,na.rm = T))
ggplot(data = projection_data_year,
       aes(x = Year,y=total_trained_year)) +
  geom_bar(position = "dodge",stat = "identity",fill = "#3876BF") +
  scale_fill_brewer(palette = "Blues")+
  scale_x_continuous(breaks=seq(2023,2030,1))+ 
  facet_wrap(~trained_teacher,scales = "free")+
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = label_comma())+
  ggtitle("Budget implication by Trainings") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    panel.grid.major.y = element_line( size=.1, color="black" ) ,
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    plot.title = element_text(hjust = 0.5),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 8),
    axis.text.y = element_text(face="bold",color = "#245953", size = 8),
    #remove x axis ticks
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("white")),
    legend.title = element_blank(),
    legend.text = element_text(size = 10)
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("04_reporting/02_visuals/budget_trainings_new1.png",
       units = "px",width = 2500,height = 1800,dpi = 170,
       device = "png")

# Budget implications by training and age bracket ####
options(scipen = 999)
cols_data <- unique(projection_data$trained_teacher)
projection_data_training <- projection_data |>
  group_by(Year,trained_teacher,Age_brackets)|>
  summarise(total_training = sum(total_cost,na.rm = T))
for (i in cols_data){
  print(i)
  ggplot(data = filter(projection_data_training,trained_teacher == i),
         aes(x = Year,y=total_training)) +
    geom_bar(position = "dodge",stat = "identity",fill = "#3876BF") +
    scale_fill_brewer(palette = "Blues")+
    scale_x_continuous(breaks=seq(2023,2030,1))+ 
    facet_wrap(~Age_brackets,scales = "free")+
    scale_y_continuous(expand = expansion(mult = c(0, .1)),
                       labels = label_comma())+
    ggtitle(paste("Budget implication by Trainings:",i)) +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      # explicitly set the horizontal lines (or they will disappear too)
      panel.grid.major.y = element_line( size=.1, color="black" ) ,
      panel.grid.minor.x = element_blank(),
      panel.grid.minor.y = element_blank()
    )+
    theme(
      plot.background = element_rect(fill = c("white")),
      panel.background = element_rect(fill = c("white")),
      plot.title = element_text(hjust = 0.5),
      #panel.grid = element_blank(),
      #remove x axis ticks
      #axis.text.x = element_blank(),
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      #remove x axis labels
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(face="bold",color = "#245953", size = 8),
      axis.text.y = element_text(face="bold",color = "#245953", size = 8),
      #remove x axis ticks
      #axis.text.y = element_blank(),
      legend.box = "horizontal",
      legend.position = "bottom",
      legend.background = element_rect(fill = c("white")),
      legend.title = element_blank(),
      legend.text = element_text(size = 10)
    ) +
    guides(fill = guide_legend(nrow = 1))
  
  ggsave(
    paste0(
      "04_reporting/02_visuals/trainings_visuals/",
      "budget_trainings_",
      gsub(" ", "", i, fixed = TRUE),
      "1.png"
    ),
    units = "px",
    width = 2600,
    height = 1800,
    dpi = 180,
    device = "png"
  )
}

#####
count(filter(tmis_df,role=="Teacher"),teachingCategoryName)
round(mean(filter(tmis_filter,role=="Teacher")$age,na.rm = T))
filter(tmis_filter,
       role %in% c("Teacher", "DOD", "DOS", "Head Teacher")) |>
  group_by(teachingCategoryName,gender) |>
  summarise(average_age = round(mean(age,na.rm = T)))


#####
tmis_teacher_long <- filter(tmis_age_long,
                            role %in% c("Teacher", "DOD", "DOS", "Head Teacher"),
                            Age >= 50)|>
  mutate(age_categ = ifelse(Age >= 70, "70+",
                             ifelse(Age >= 65, "65-69",
                                     ifelse(Age >= 60, "60-64",
                                             ifelse(Age >= 55, "55-59",
                                                     ifelse(Age >= 50, "50-54",NA))))))
cross_teacher_age <-
  count(filter(tmis_teacher_long,Year==2023), teachingCategoryName, 
        age_categ, name = "Total_count") |>
  rename(`Age Group`=age_categ) |>
  pivot_wider(id_cols = `Age Group`,names_from = teachingCategoryName,
              values_from = Total_count)

write.xlsx(
  cross_teacher_age,
  "04_reporting/01_tables/updated/cross_teacher_age.xlsx",
  asTable = T
)

# dataset to be sent so we can get employees salary packages ####
tmis_sub <- select(tmis_df, employeeid,teachingCategoryName,qualificationLevel,
                   schoolCode,schoolName,sectorName,districtName)
#write_dta(tmis_sub,"03_clean_data/tmis_subdata.dta")

# GROSS SALARIES budget analysis ####
tmis_salary <- read_excel(path = "02_raw_data/Teachers Salary 2023_october.xlsx")
tmis_salary <- tmis_salary |>
  separate_longer_delim(Level,delim = ".") |>
  mutate(col_names = ifelse(str_detect(Level,"[0-9]"),"level_1","level_2")) |>
  pivot_wider(names_from = col_names,values_from = Level) 


tmis_join <- rename(tmis_salary, employeeid = id) |>
  full_join(mutate(tmis_ret_subj, employeeid = as.character(employeeid)))

sum(is.na(tmis_join$GrossSalary))

view(count(filter(
  tmis_join, is.na(GrossSalary) |
    is.na(qualificationLevel)
), employeeid)) # there are 40 employees (teachers) who either belong in the  
# tmis_salary or tmis_ret_subj

tmis_join <- filter(tmis_join,!is.na(GrossSalary),!is.na(qualificationLevel))
sum(is.na(tmis_join$GrossSalary))

tmis_join <- tmis_join |>
  mutate(Age_categ = if_else(Age >= 70, "70+",
                             if_else(Age >= 65, "65-69",
                                     if_else(Age >= 60, "60-64",
                                             if_else(Age >= 55, "55-59",
                                                     if_else(Age >= 50, "50-54",
                                                             if_else(Age >= 45, "45-49",
                                                                     if_else(Age >= 40, "40-44",
                                                                             if_else(Age >= 35, "35-39",
                                                                                     if_else(Age >= 30, "30-34",
                                                                                             if_else(Age >= 24, "24-29",
                                                                                                     "18-23")))))))))))
  

# Salary by school staff role ####
teacher_salary_role <- tmis_join |>
  mutate(GrossSalary = as.numeric(GrossSalary)) |>
  group_by(role) |>
  summarise(total_salary_role = sum(GrossSalary,na.rm = T))

ggplot(teacher_salary_role,aes(x = str_wrap(role,width = 8),y = total_salary_role)) + 
  geom_bar(stat = "identity",fill = "#3876BF") +
  geom_text(aes(label = scales::comma(total_salary_role)),
            position = position_dodge(.9),
            size = 4,
            vjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Staff's Total salaries per  Role") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 15),
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    #remove x axis ticks
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))

ggsave("04_reporting/02_visuals/teacher_salary_role.png",
       units = "px",width = 4500,height = 3200,dpi = 270,
       device = "png")


# Salary by teaching level ####
teacher_salary_level <- tmis_join |>
  filter(role %in%c("Teacher", "DOD", "DOS", "Head Teacher")) |>
  mutate(GrossSalary = as.numeric(GrossSalary)) |>
  group_by(teachingCategoryName) |>
  summarise(total_salary_level = sum(GrossSalary,na.rm = T))

ggplot(teacher_salary_level,aes(x = teachingCategoryName,y = total_salary_level,
                                fill = teachingCategoryName)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = scales::comma(total_salary_level)),
            position = position_dodge(.9),
            size = 4.4,
            vjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teachers' Total salaries per Teaching Level") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 15),
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    #remove x axis ticks
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))

ggsave("04_reporting/02_visuals/teacher_salary_level.png",
       units = "px",width = 1200,height = 1800,dpi = 150,
       device = "png")

# salary by teachers' Age ####

teacher_salary_age <- tmis_join  |>
  filter(role %in%c("Teacher", "DOD", "DOS", "Head Teacher")) |>
  mutate(GrossSalary = as.numeric(GrossSalary)) |>
  group_by(Year,Age_categ) |>
  summarise(total_salary_age = sum(GrossSalary,na.rm = T))

ggplot(filter(teacher_salary_age,!is.na(Age_categ),Year == 2023),
       aes(x = Age_categ,y = total_salary_age)) + 
  geom_bar(stat = "identity",fill = "#3876BF") +
  scale_color_brewer(palette = "Blues") +
  geom_text(aes(label = scales::comma(total_salary_age)),
            position = position_dodge(.9),
            size = 4.4,
            vjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teachers' Total salaries per Age group") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 15),
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    #remove x axis ticks
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))

ggsave("04_reporting/02_visuals/teacher_salary_age.png",
       units = "px",width = 2800,height = 2000,dpi = 200,
       device = "png")

# Salary per qualification to teach level (A0, A1, A2) ####
teachers_salary_qualification <- tmis_join  |>
  filter(role %in%c("Teacher", "DOD", "DOS", "Head Teacher")) |>
  mutate(GrossSalary = as.numeric(GrossSalary)) |>
  group_by(Year,qualificationLevel) |>
  summarize(total_salary_qualification = sum(GrossSalary,na.rm = T))

ggplot(filter(teachers_salary_qualification,Year == 2023),
       aes(x = qualificationLevel,y = total_salary_qualification,
           fill = qualificationLevel)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette = "Blues") +
  geom_text(aes(label = scales::comma(total_salary_qualification)),
            position = position_dodge(.9),
            size = 4.4,
            vjust=-.5,
            color = "#000000",
            fontface = "bold") +
  ggtitle("Teachers' Total salaries per Qualification to Teach Level") +
  theme(
    plot.title = element_text(hjust = .5),
    axis.ticks = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 15),
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    #remove x axis ticks
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, .1)))

ggsave("04_reporting/02_visuals/teacher_salary_qualification.png",
       units = "px",width = 2000,height = 2000,dpi = 200,
       device = "png")

# salary per age and qualification to teach level ####

teachers_salary_age_qualification <- tmis_join  |>
  filter(role %in%c("Teacher", "DOD", "DOS", "Head Teacher")) |>
  mutate(GrossSalary = as.numeric(GrossSalary)) |>
  group_by(Year,Age_categ,qualificationLevel) |>
  summarize(total_salary_age_qualification = sum(GrossSalary,na.rm = T))

ggplot(filter(teachers_salary_age_qualification,!is.na(Age_categ),Year == 2023),
       aes(x = Age_categ,y = total_salary_age_qualification,
           group = qualificationLevel, fill = qualificationLevel)) + 
  geom_bar(stat = "identity",position = "dodge") +
  scale_fill_brewer(palette = "Blues") +
  ggtitle("Teachers' Total salaries per Age Category and Qualification to Teach Level")  + 
  ylim(c(0,3100000000)) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = label_comma())+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( linewidth =.1, color="black" ) ,
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    plot.title = element_text(hjust = 0.5),
    #panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 20),
    axis.text.y = element_text(face="bold",color = "#245953", size = 20),
    #remove x axis ticks
    #axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("white")),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("04_reporting/02_visuals/teacher_salary_age_qualification.png",
       units = "px",width = 2400,height = 2000,dpi = 200,
       device = "png")

# salary per age and teaching level ####

teachers_salary_age_level <- tmis_join  |>
  filter(role %in%c("Teacher", "DOD", "DOS", "Head Teacher")) |>
  mutate(GrossSalary = as.numeric(GrossSalary)) |>
  group_by(Year,Age_categ,teachingCategoryName) |>
  summarize(total_salary_age_level = sum(GrossSalary,na.rm = T))

ggplot(filter(teachers_salary_age_level,!is.na(Age_categ),Year == 2023),
       aes(x = Age_categ,y = total_salary_age_level,
           group = teachingCategoryName, fill = teachingCategoryName)) + 
  geom_bar(stat = "identity",position = "dodge") +
  scale_fill_brewer(palette = "Blues") +
  ggtitle("Teachers' Total salaries per Age Category and Teaching Level")  + 
  ylim(c(0,3500000000)) + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = label_comma())+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( linewidth =.1, color="black" ) ,
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    plot.title = element_text(hjust = 0.5),
    #panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 20),
    axis.text.y = element_text(face="bold",color = "#245953", size = 20),
    #remove x axis ticks
    #axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("white")),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow = 1))

ggsave("04_reporting/02_visuals/teacher_salary_age_level.png",
       units = "px",width = 2400,height = 2000,dpi = 200,
       device = "png")

# salary based on roman levels ####

teacher_salary_roman <- tmis_join  |>
  filter(role %in%c("Teacher", "DOD", "DOS", "Head Teacher")) |>
  mutate(GrossSalary = as.numeric(GrossSalary)) |>
  group_by(Year,level_2) |>
  summarise(total_salary_roman = sum(GrossSalary,na.rm = T))

ggplot(filter(teacher_salary_roman, Year == 2023),
       aes(x = level_2,y = total_salary_roman)) + 
  geom_bar(stat = "identity",fill = "#3876BF") +
  scale_color_brewer(palette = "Blues") +
  ggtitle("Teachers' Total Salaries per Seniority level") +
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( linewidth =.4, color="black" ) ,
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    plot.title = element_text(hjust = 0.5),
    #panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 15),
    axis.text.y = element_text(face="bold",color = "#245953", size = 15),
    #remove x axis ticks
    legend.position = "none"
  ) +
  scale_y_continuous(breaks = seq(0,15000000000,2000000000),
                     expand = expansion(mult = c(0, .1)),labels = label_comma())

ggsave("04_reporting/02_visuals/teacher_salary_roman.png",
       units = "px",width = 2800,height = 2000,dpi = 200,
       device = "png")

# descriptive relationship between salary and count by seniority levels ####
salary_count_roman <- filter(tmis_join,Year == 2023) |>
  mutate(GrossSalary = as.numeric(GrossSalary)) |>
  group_by(level_2) |>
  summarise(total_salaries = sum(GrossSalary,na.rm = T),
            total_count = n())
options(scipen = 999)
ggplot(salary_count_roman,aes(x = total_count, y = total_salaries)) +
  geom_point() + 
  geom_smooth(se = F,method = lm,color="#000000")+
  geom_text(aes(label= level_2),check_overlap = T,
            size = 4,vjust = 1.4, hjust = 1 )+
  xlab("Total count per level") +
  ylab("Total salary per level") +
  scale_y_continuous(labels = label_comma())+
  scale_x_continuous(breaks = seq(0,60000,5000),labels = label_comma())+
  theme_bw()

ggsave("04_reporting/02_visuals/teacher_salary_count_roman.png",
       units = "px",width = 7000,height = 4000,dpi = 370,
       device = "png")


# salary based on seniority levels disaggregated by qualification to teach level #### 

teacher_salary_qualification_roman <- tmis_join  |>
  filter(role %in%c("Teacher", "DOD", "DOS", "Head Teacher")) |>
  mutate(GrossSalary = as.numeric(GrossSalary)) |>
  group_by(Year,qualificationLevel,level_2) |>
  summarise(total_salary_qualification_roman = sum(GrossSalary,na.rm = T))

ggplot(filter(teacher_salary_qualification_roman, Year == 2023),
       aes(x = level_2,y = total_salary_qualification_roman)) + 
  geom_bar(stat = "identity",fill = "#3876BF") +
  scale_color_brewer(palette = "Blues") +
  facet_wrap(~qualificationLevel,ncol = 2,scales = "free") + 
  scale_y_continuous(expand = expansion(mult = c(0, .1)),
                     labels = label_comma())+
  ggtitle(" ")+
  theme( # remove the vertical grid lines
    panel.grid.major.x = element_blank() ,
    # explicitly set the horizontal lines (or they will disappear too)
    panel.grid.major.y = element_line( linewidth =.4, color="black" ) ,
    panel.grid.minor.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    plot.background = element_rect(fill = c("white")),
    panel.background = element_rect(fill = c("white")),
    plot.title = element_text(hjust = 0.5),
    #panel.grid = element_blank(),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(face="bold",color = "#245953", size = 15),
    axis.text.y = element_text(face="bold",color = "#245953", size = 15),
    #remove x axis ticks
    #axis.text.y = element_blank(),
    legend.box = "horizontal",
    legend.position = "bottom",
    legend.background = element_rect(fill = c("white")),
    legend.title = element_blank(),
    legend.text = element_text(size = 15)
  ) +
  guides(fill = guide_legend(nrow = 1))
ggsave("04_reporting/02_visuals/teacher_salary_qualification_roman.png",
       units = "px",width = 7000,height = 4000,dpi = 370,
       device = "png")



# 
