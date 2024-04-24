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

# join#
#######

# Guidelines
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


rep_surv_comb[which(rep_surv_comb$GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION == "KOMEZUSENGE ALPHONSE"),
              "Guidelines.Categories"]  <- "specific student"

rep_surv_comb[which(rep_surv_comb$GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION == "UWACU ANGE MARIE CHRISTELLA"),
              "Guidelines.Categories"]  <- "specific student"

rep_surv_comb[which(rep_surv_comb$GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION == "ISHIMWE ROSINE"),
              "Guidelines.Categories"]  <- "specific student"
rep_surv_comb[which(rep_surv_comb$GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION == "BYIRINGIRO NOAH"),
              "Guidelines.Categories"]  <- "specific student"

rep_surv_comb <- select(rep_surv_comb, -Guidelines.Categories.y,-Guidelines.Categories.x)


#Criteria

temp_criteria_filtered <- temp_criterias |>
  select(-GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION,-Guidelines.Categories)


rep_surv_comb <- rep_surv_comb |>
  left_join(
    temp_criteria_filtered,
    by = join_by(
      ID,
      `WHAT'S.THE.NAME.OF.YOUR.SCHOOL`,
      SDMS.CODE,
      DISTRICT,
      SCHOOL.STATUS,
      SCHOOL.CATEGORY,
      `CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION`
    )
  ) |>
  mutate(
    Criteria.Categories = coalesce(Criteria.Categories.x, Criteria.Categories.y)
  )

rep_surv_comb <- select(rep_surv_comb, -Criteria.Categories.x,-Criteria.Categories.y)
write_dta(
  rename(rep_surv_comb, school_name = `WHAT'S.THE.NAME.OF.YOUR.SCHOOL`,
         sdms_code = SDMS.CODE,
         school_status = SCHOOL.STATUS,
         school_categories = SCHOOL.CATEGORY,
         guidelines = `GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION`,
         guidelines_categories = Guidelines.Categories,
         criteria = `CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION`,
         criteria_categories = Criteria.Categories),
  "02_clean/repetition_data.dta"
)



# Data Visualization#
####################

# guidelines

rep_guide <-
  select(
    rep_surv_comb,
    ID:SDMS.CODE,
    GUIDELINES.TO.MAKE.DECISIONS.FOR.REPETITION,
    Guidelines.Categories
  )

rep_guide_dist <- rep_guide |> 
  distinct()

rep_guide_count <- count(rep_guide_dist,Guidelines.Categories)
rep_guide_count <-
  rename(rep_guide_count, `Guidelines Categories` = Guidelines.Categories,
         `Total Count` = n)

ggplot(filter(rep_guide_count,!is.na(`Guidelines Categories`)),
       aes(reorder(`Guidelines Categories`,`Total Count`),`Total Count`)) +
  geom_bar(stat = "identity",
           position = "dodge",fill = "#5BBCFF") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 2000))  +
  coord_flip() +
  geom_text(aes(label=`Total Count`),
            position = position_dodge(.9), size = 3, hjust = -.2, 
            vjust = -.1,fontface="bold",color="#232D3F") +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks = element_blank(),  #remove x axis ticks
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10)) +
  ggtitle("Guidelines to make decisions for repetition categories")
ggsave("03_output/guidelines_categories.png",width = 2500, height = 2500,units = "px")


# criteria

rep_crit <-
  select(
    rep_surv_comb,
    ID:SDMS.CODE,
    `CRITERIA.TO.DETERMINE.CHILD'S.ACADEMIC.MERIT.FOR.PROMOTION`,
    Criteria.Categories
  )

rep_crit_dist <- rep_crit |> 
  distinct()

rep_crit_count <- count(rep_crit_dist,Criteria.Categories)
rep_crit_count <-
  rename(rep_crit_count, `Criteria Categories` = Criteria.Categories,
         `Total Count` = n)

ggplot(filter(rep_crit_count,!is.na(`Criteria Categories`)),
       aes(reorder(`Criteria Categories`,`Total Count`),`Total Count`)) +
  geom_bar(stat = "identity",
           position = "dodge",fill = "#569DAA") + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 3000))  +
  coord_flip() +
  geom_text(aes(label=`Total Count`),
            position = position_dodge(.9), size = 3, hjust = -.2, 
            vjust = -.1,fontface="bold",color="#232D3F") +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = .5),
    #remove x axis ticks
    #axis.text.x = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove x axis labels
    axis.ticks = element_blank(),  #remove x axis ticks
    axis.text.x = element_blank(),
    axis.text.y = element_text(face = "bold", size = 10)) +
  ggtitle("Criteria to Determine Child's Academic Merit for Promotion categories")
ggsave("03_output/criteria_categories.png",width = 2500, height = 2500,units = "px")



rep_guide_count <- rep_guide_count |>
  mutate(percentage_count = round(`Total Count`*100/sum(`Total Count`),0))

rep_crit_count <- rep_crit_count |>
  mutate(percentage_count = round(`Total Count`*100/sum(`Total Count`),0))



# long to wide format guidelines

rep_surv_long <- read_dta("02_clean/repetition_data.dta")

rep_surv_long_guide <- rep_surv_long |>
  filter(guidelines_categories!="") |>
  select(-guidelines,-criteria,-criteria_categories) |>
  group_by(ID,guidelines_categories) |>               
  mutate(counted = n(),dup = ifelse(counted == 1,0,row_number())) |>
  filter(dup==0) |>
  ungroup()|>
  select(-counted,-dup)
rep_surv_long_guide <- rep_surv_long_guide |>
  mutate(guidelines_categories = gsub(" ","_",guidelines_categories)) |>
  mutate(guidelines_categories = str_c("guidelines",guidelines_categories,sep = "_"),
         guide_count = 1)|>
  pivot_wider(names_from = guidelines_categories,
              values_from = guide_count,
              values_fill = 0)



# long to wide format criteria

rep_surv_long_crit <- rep_surv_long |>
  filter(criteria_categories!="") |>
  select(-guidelines,-criteria,-guidelines_categories) |>
  group_by(ID,criteria_categories) |>               
  mutate(counted = n(),dup = ifelse(counted == 1,0,row_number())) |>
  filter(dup==0) |>
  ungroup()|>
  select(-counted,-dup)
rep_surv_long_crit <- rep_surv_long_crit |>
  mutate(criteria_categories = gsub(" ","_",criteria_categories)) |>
  mutate(criteria_categories = str_c("criteria",criteria_categories,sep = "_"),
         crit_count = 1)|>
  pivot_wider(names_from = criteria_categories,
              values_from = crit_count,
              values_fill = 0)

# Joining the datasets

rep_surv_wide <- rep_surv_long_guide |>
  left_join(
    rep_surv_long_crit,
    by = join_by(
      ID,
      school_name,
      sdms_code,
      DISTRICT,
      school_status,
      school_categories
    )
  ) |>
  mutate_if(is.numeric,coalesce,0)




























