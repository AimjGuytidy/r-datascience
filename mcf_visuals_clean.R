#load necessary libraries
library(dplyr)
library(officer)
library(ggplot2)
library(haven)
library(labelled)
library(rio)
library(stringr)
library(sjmisc)
library(tidyr)
library(lubridate)
library(readxl)
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)

Light_grey <- c("#F2F2F2") #Light grey for the background
Blue <- c("#097ABC") #Blue
Light_blue <- c("#9DC3E5") #Light blue
Dark_blue <- c("#1b2f55") #Dark blue
Green <- c("#8ccc98") #Green
Dark_grey <- c("#7F7F7F") #Dark grey
Dark_green <- c("#49711E") #Dark green

#load data
mcf_data <- read_dta(
  paste0(
    "G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/",
    "4. QUANT/1. Data collection/Data management/Clean datasets/Main clean dataset/",
    "Final dataset/MCF_Baseline_Main data - Clean.dta"
  )
)


# rearranging the columns
mcf_data <- mcf_data %>%
  move_columns(c(type_employ, main_activity), .after = give_chance) %>%
  move_columns(c(main_month, main_years), .before = inc_impro) %>%
  move_columns(c(last_year, year_before), .before = intro_prod) %>%
  move_columns(c(opera_month, opera_years), .before = job_accept)

variables <- var_label(mcf_data)

#Add stratum column
`%notin%` <- Negate(`%in%`)
mcf_data <- mcf_data %>%
  dplyr::mutate(
    stratum = case_when(
      type_employ == 1 ~ 1,
      type_employ == 2 ~ 2,
      current_educ %notin% c(1, NA) &
        is.na(type_employ) ~ 3,
      current_educ %in% c(1, NA) &
        is.na(type_employ) ~ 4
    )
  ) %>%
  set_value_labels(
    stratum = c(
      `Wage employed` = 1,
      `Self employed` = 2,
      `Students` = 3,
      ` Unemployed/Non job-seekers` = 4
    )
  )

#Checking the number in each stratum
stratum <- mcf_data %>%
  dplyr::group_by(stratum, geo_entity, gender) %>%
  dplyr::summarize(total = n()) %>%
  pivot_wider(names_from = c(gender),
              values_from = c(total)) %>%
  dplyr::rename(Male = `1`, Female = `2`) %>%
  dplyr::mutate(total = sum(Male, Female))

#Cell weighting
total_pop <- read_excel('data/total_pop.xlsx')
total_pop <- total_pop %>%
  dplyr::mutate(
    stratum = case_when(
      stratum == "Wage employed" ~ 1,
      stratum == "Self employed" ~ 2,
      stratum == "Students" ~ 3,
      stratum == "Unemployed/Non-job seeker" ~ 4
    ),
    geo_entity = case_when(geo_entity == "Rural" ~ 2,
                           geo_entity == "Urban" ~ 1)
  )
cell_weights <- left_join(total_pop, stratum) %>%
  dplyr::mutate(male_wt = `Total Male` / Male, female_wt = `Total Female` /
                  Female)
cell_weights <- cell_weights %>%
  dplyr::select(stratum, geo_entity, female_wt, male_wt)
colnames(cell_weights) <- c('stratum', 'geo_entity', 'Female', 'Male')
cell_weights <- cell_weights %>%
  pivot_longer(
    cols = c('Female', 'Male'),
    names_to = 'gender1',
    values_to = 'cell_weights'
  ) %>%
  dplyr::mutate(gender = case_when(gender1 == 'Male' ~ 1,
                                   gender1 == 'Female' ~ 2)) %>%
  dplyr::select(-c(gender1))

#Add the weight column by joins
mcf_data <-
  left_join(mcf_data, cell_weights, by = c("stratum", "geo_entity", "gender")) %>%
  move_columns(c(main_month, main_years), .before = inc_impro)

mcf_data <- mcf_data%>%
  mutate(age = year(today())-age)

# create an empty word document
survey_data <- read_docx()

# let's create a function to help us automate some operations

preproc_stack <- function(data_prep,i,title_prep="no title provided") {
  temp <- characterize(data_prep) %>%
    rename(temp1 = as.factor(i)) %>%
    group_by(temp1, gender) %>%
    summarise(n = sum(cell_weights)) %>%
    dplyr::mutate(n = n * 100 / sum(n)) %>%
    ungroup() %>%
    dplyr::mutate_all( ~ replace(., is.na(.), "Not applicable")) %>%
    dplyr::mutate(across(-n, function(x)
      gsub("^[a-z]+\\.\\s+", "", x)),
      temp1 = str_wrap(temp1, width = 10),
      n = as.integer(n))
  if (nrow(temp) > 6){
    temp <- characterize(data_prep) %>%
      rename(temp1 = as.factor(i)) %>%
      group_by(temp1, gender) %>%
      summarise(n = sum(cell_weights)) %>%
      dplyr::mutate(n = n * 100 / sum(n)) %>%
      ungroup() %>%
      dplyr::mutate_all( ~ replace(., is.na(.), "Not applicable")) %>%
      dplyr::mutate(across(-n, function(x)
        gsub("^[a-z]+\\.\\s+", "", x)),
        temp1 = str_wrap(temp1, width = 28),
        n = as.integer(n))
    
    temp3<-
      temp %>%
      ggplot(mapping = aes(temp1, n)) +
      geom_bar(stat = "identity", position = "fill", aes(fill =
                                                           gender)) +
      xlab("") +
      ylab("") +
      geom_text(
        aes(label = paste0(n, "%")),
        position = position_fill(vjust = 0.5),
        size = 2,
        color = "white"
      ) +
      ggtitle(title_prep)+
      coord_flip()+
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(
        plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank(),
        #remove x axis ticks
        axis.text.x = element_blank(),
        #remove y axis labels
        axis.ticks.x = element_blank()  #remove y axis ticks
      ) +
      scale_fill_manual(values = c(Blue, Light_blue),
                        aesthetics = "fill")+
      scale_y_discrete(guide = guide_axis(n.dodge=2))
    survey_data <-
      body_add_gg(survey_data, value = temp3, style = "centered",height = 10)
    return(temp3)
    
  }else{
    
    temp3<-
      temp %>%
      ggplot(mapping = aes(temp1, n)) +
      geom_bar(stat = "identity", position = "fill", aes(fill =
                                                           gender)) +
      xlab("") +
      ylab("") +
      geom_text(
        aes(label = paste0(n, "%")),
        position = position_fill(vjust = 0.5),
        size = 3,
        color = "white"
      ) +
      ggtitle(title_prep) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(
        plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank(),
        #remove x axis ticks
        axis.text.y = element_blank(),
        #remove y axis labels
        axis.ticks.y = element_blank()  #remove y axis ticks
      ) +
      scale_fill_manual(values = c(Blue, Light_blue),
                        aesthetics = "fill")
    survey_data <-
      body_add_gg(survey_data, value = temp3, style = "centered")
    return(temp3)
    
  }
}

preproc_dodge <- function(data_prep,i,title_prep="no title provided") {
  temp <- characterize(data_prep) %>%
    rename(temp1 = as.factor(i)) %>%
    count(temp1, gender, wt = cell_weights) %>%
    dplyr::mutate_all(~ replace(., is.na(.), "Not applicable")) %>%
    dplyr::mutate(
      across(-n, function(x)
        gsub("^[a-z]+\\.\\s+", "", x)),
      temp1 = str_wrap(temp1, width = 10),
      n = as.integer(n),
      n = n / sum(n)
    )
  if (nrow(temp)>6){
    temp <- characterize(data_prep) %>%
      rename(temp1 = as.factor(i)) %>%
      count(temp1, gender, wt = cell_weights) %>%
      dplyr::mutate_all(~ replace(., is.na(.), "Not applicable")) %>%
      dplyr::mutate(
        across(-n, function(x)
          gsub("^[a-z]+\\.\\s+", "", x)),
        temp1 = str_wrap(temp1, width = 28),
        n = as.integer(n),
        n = n / sum(n)
      )
    temp3<-
      (temp %>%
         ggplot(mapping = aes(temp1, n)) +
         geom_bar(stat = "identity", position = "dodge", aes(fill = gender)) +
         xlab("") +
         ylab("") +
         ggtitle(title_prep) +
         coord_flip()+
         theme(plot.title = element_text(hjust = 0.5)) +
         theme(
           plot.background = element_rect(fill = c("#F2F2F2")),
           panel.background = element_rect(fill = c("#F2F2F2")),
           panel.grid = element_blank()
         ) +
         scale_fill_manual(values = c(Blue, Light_blue),
                           aesthetics = "fill") +
         scale_y_continuous(
           limits = c(0, (max(temp$n,na.rm = TRUE) + .1)),
           labels = scales::percent_format()
         ))
    survey_data <-
      body_add_gg(survey_data, value = temp3, style = "centered",height = 10)
    return(temp3)
  }else{
    temp3<-
      (temp %>%
         ggplot(mapping = aes(temp1, n)) +
         geom_bar(stat = "identity", position = "dodge", aes(fill = gender)) +
         xlab("") +
         ylab("") +
         ggtitle(title_prep) +
         theme(plot.title = element_text(hjust = 0.5)) +
         theme(
           plot.background = element_rect(fill = c("#F2F2F2")),
           panel.background = element_rect(fill = c("#F2F2F2")),
           panel.grid = element_blank()
         ) +
         scale_fill_manual(values = c(Blue, Light_blue),
                           aesthetics = "fill") +
         scale_y_continuous(
           limits = c(0, (max(temp$n,na.rm = TRUE) + .1)),
           labels = scales::percent_format()
         ))
    survey_data <-
      body_add_gg(survey_data, value = temp3, style = "centered")
    return(temp3)
  }
}


# visuals for gender

# create visuals for gender
gendr <- characterize(mcf_data) %>%
  group_by(gender) %>%
  summarise(n = sum(cell_weights)) %>%
  dplyr::mutate(n = n * 100 / sum(n)) %>%
  ungroup()%>%
  dplyr::mutate(across(-n, function(x)
    gsub("^[a-z]+\\.\\s+", "", x)),
    gender = str_wrap(gender, width = 28),
    n = as.integer(n),
    val1 = c("gender","gender"))
  



(
  gender_plot <- gendr %>%
    ggplot(mapping = aes(val1, n)) +
    geom_bar(stat = "identity", position = "fill", aes(fill = gender)) +
    xlab("") +
    ylab("") +
    geom_text(
      aes(label = paste0(round(n), "%")),
      position = position_fill(vjust = 0.5),
      size = 3,
      color = "white"
    ) +
    ggtitle("Gender") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      plot.background = element_rect(fill = c("#F2F2F2")),
      panel.background = element_rect(fill = c("#F2F2F2")),
      panel.grid = element_blank(),
      #remove x axis ticks
      axis.text.y = element_blank(),
      axis.text.x = element_blank(),
      #remove y axis labels
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank()#remove y axis ticks
    ) +
    scale_fill_manual(values = c(Blue, Light_blue),
                      aesthetics = "fill")
)
survey_data <-
  body_add_gg(survey_data, value = gender, style = "centered")

# Age visuals

age_fill<-preproc_stack(mcf_data,"age",title_prep = "Age Distribution")
age_dodge<-preproc_dodge(mcf_data,"age",title_prep = "Age Distribution")

# Marital status visuals

mart_stat_fill <- preproc_stack(mcf_data,"mart_status",title_prep = "Marital Status")
mart_stat_dodge <- preproc_dodge(mcf_data,"mart_status",title_prep = "Marital Status")

# Head of household relationship visuals

hhrelation_fill <- preproc_stack(mcf_data,"hhrelation",title_prep = "Head of household relationship")
hhrelation_dodge <- preproc_dodge(mcf_data,"hhrelation",title_prep = "Head of household relationship")

# create visuals for Gender of the household head

hhgender_fill <- preproc_stack(mcf_data,"hh_gender",title_prep = "Gender of Head of households")
hhgender_dodge <- preproc_dodge(mcf_data,"hh_gender",title_prep = "Gender of Head of households")

# create visuals for Refugees

refugees_fill <- preproc_stack(mcf_data,"refuge",title_prep = "Refugee Status")
refugees_dodge <-preproc_dodge(mcf_data,"refuge",title_prep = "Refugee Status")


# respondents conditions visuals

mcf_cond <- characterize(mcf_data)%>%
  select(contains("resp_conditions_"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_cond%>%
                   select(contains("resp_conditions_")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_cond,i,title_prep = paste0("Respondents Conditions:",i)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_cond,i,title_prep = paste0("Respondents Conditions:",i)))
}

# create visuals for the seriousness of the conditions that respondents have

diff_stack <-preproc_stack(mcf_data,"difficulty_1",title_prep = "Difficulty due to conditions")
diff_dodge <- preproc_dodge(mcf_data,"difficulty_1",title_prep = "Difficulty due to conditions")
