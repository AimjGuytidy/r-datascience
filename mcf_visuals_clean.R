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
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()#remove y axis ticks
      ) +
      scale_fill_manual(values = c(Blue, Light_blue),
                        aesthetics = "fill")+
      #scale_y_discrete(guide = guide_axis(n.dodge=2))+
      scale_y_continuous(expand=expansion(mult=c(0,0.1)))
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
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()#remove y axis ticks
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
  body_add_gg(survey_data, value = gender_plot, style = "centered")

# Age visuals
age_dodge<-preproc_dodge(mcf_data,"age",title_prep = "Age Distribution")

age_fill<-preproc_stack(mcf_data,"age",title_prep = "Age Distribution")

# Marital status visuals
mart_stat_dodge <- preproc_dodge(mcf_data,"mart_status",title_prep = "Marital Status")

mart_stat_fill <- preproc_stack(mcf_data,"mart_status",title_prep = "Marital Status")

# Head of household relationship visuals
hhrelation_dodge <- preproc_dodge(mcf_data,"hhrelation",title_prep = "Head of household relationship")

hhrelation_fill <- preproc_stack(mcf_data,"hhrelation",title_prep = "Head of household relationship")

# create visuals for Gender of the household head
hhgender_dodge <- preproc_dodge(mcf_data,"hh_gender",title_prep = "Gender of Head of households")

hhgender_fill <- preproc_stack(mcf_data,"hh_gender",title_prep = "Gender of Head of households")

# create visuals for Refugees
refugees_dodge <-preproc_dodge(mcf_data,"refuge",title_prep = "Refugee Status")

refugees_fill <- preproc_stack(mcf_data,"refuge",title_prep = "Refugee Status")


# respondents conditions visuals

mcf_cond <- characterize(mcf_data)%>%
  select(contains("resp_conditions_"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_cond%>%
                   select(contains("resp_conditions_")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_cond,i,title_prep = paste0("Respondents Conditions:",var_label(mcf_cond[c(i)]))))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_cond,i,title_prep = paste0("Respondents Conditions:",var_label(mcf_cond[c(i)]))))
}

# create visuals for the seriousness of the conditions that respondents have
diff_dodge <- preproc_dodge(mcf_data,"difficulty_1",title_prep = "Difficulty due to conditions")

diff_stack <-preproc_stack(mcf_data,"difficulty_1",title_prep = "Difficulty due to conditions")

#official doc
offdoc_dodge <-preproc_dodge(mcf_data,"officialdoc",title_prep = str_wrap("If yes to any of the above, do you have an official ID saying that you have the disability?" , width = 42))

offdoc_fill <- preproc_stack(mcf_data,"officialdoc",title_prep = str_wrap("If yes to any of the above, do you have an official ID saying that you have the disability?" , width = 42))

#mastercadprog
mastercard_dodge <-preproc_dodge(mcf_data,"mastcard_progr",title_prep = str_wrap("To your knowledge, have you ever participated in a programme collaborating with the Mastercard Foundation?" , width = 42))

mastercard_fill <- preproc_stack(mcf_data,"mastcard_progr",title_prep = str_wrap("To your knowledge, have you ever participated in a programme collaborating with the Mastercard Foundation?" , width = 42))

# implementation partners

mcf_impl <- characterize(mcf_data)%>%
  select(contains("progr_"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_impl%>%
                   select(contains("progr_")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_impl,i,title_prep = paste0("Implementation Partner:",var_label(mcf_impl[c(i)]))))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_impl,i,title_prep = paste0("Implementation Partner:",var_label(mcf_impl[c(i)]))))
}

# create visuals for employment status
employ_dodge <-preproc_dodge(mcf_data,"employment",title_prep = str_wrap("Did you do any work for wage, salary, commissions, tips, or any other pay, in cash or in-kind, even if only for one hour? (Includes persons with regular, casual, short-term or part-time intermittent and seasonal jobs, apprentices)?" , width = 42))

employ_fill <- preproc_stack(mcf_data,"employment",title_prep = str_wrap("Did you do any work for wage, salary, commissions, tips, or any other pay, in cash or in-kind, even if only for one hour? (Includes persons with regular, casual, short-term or part-time intermittent and seasonal jobs, apprentices)?" , width = 42))

# create visuals for Farming status
ownfarm_dodge <-preproc_dodge(mcf_data,"own_farming",title_prep = str_wrap("Did you do any farm work such as growing crops, raising or tending animals, fishing, forestry, etc.?" , width = 42))

ownfarm_fill <- preproc_stack(mcf_data,"own_farming",title_prep = str_wrap("Did you do any farm work such as growing crops, raising or tending animals, fishing, forestry, etc.?" , width = 42))

# create visuals for Farming sales status
farmsale_dodge <-preproc_dodge(mcf_data,"sell_goods",title_prep = str_wrap("If yes to any of the above, did you sell or barter any part of the goods obtained from this work?" , width = 42))

farmsale_fill <- preproc_stack(mcf_data,"sell_goods",title_prep = str_wrap("If yes to any of the above, did you sell or barter any part of the goods obtained from this work?" , width = 42))



# Section B: Non-employment

# apply variable
gr_apply_dodge <- preproc_dodge(mcf_data,"apply",title_prep = str_wrap("Over the past 3 months, have you applied/searched for work that provides wage, salary, commissions, tips, or any other pay, in cash or in-kind?", width = 42)) 

gr_apply_fill <- preproc_stack(mcf_data,"apply",title_prep = str_wrap("Over the past 3 months, have you applied/searched for work that provides wage, salary, commissions, tips, or any other pay, in cash or in-kind?", width = 42))


# Nojob variable
gr_nojob_dodge <- preproc_dodge(mcf_data,"nojob",title_prep = str_wrap("If yes, why do you think you didn’t get the job?", width = 42))

gr_nojob_fill <- preproc_stack(mcf_data,"nojob",title_prep = str_wrap("If yes, why do you think you didn’t get the job?", width = 42))



# Noseek variable
gr_noseek_dodge <- preproc_dodge(mcf_data,"noseek",title_prep = str_wrap("If you have not sought paid work, what is the reason you did not seek work?", width = 42))

gr_noseek_fill <- preproc_stack(mcf_data,"noseek",title_prep = str_wrap("If you have not sought paid work, what is the reason you did not seek work?", width = 42))


#Expect seek variable
gr_expect_dodge <- preproc_dodge(mcf_data,"expect_seek",title_prep = str_wrap("If you did not seek work for any reason below option d., do you expect to seek paid work within the next 6 months? ", width = 42))

gr_expect_fill <- preproc_stack(mcf_data,"expect_seek",title_prep = str_wrap("If you did not seek work for any reason below option d., do you expect to seek paid work within the next 6 months? ", width = 42))



# no expect seek categories 

mcf_noexpect <- characterize(mcf_data)%>%
  select(contains("noexpect_seek_"),gender,geo_entity,cell_weights,stratum)%>%
  select(-noexpect_seek_oth)
for (i in colnames(mcf_noexpect%>%
                   select(contains("noexpect_seek_")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_noexpect,i,title_prep = paste0("If no, why not?:",var_label(mcf_noexpect[c(i)]))))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_noexpect,i,title_prep = paste0("If no, why not?:",var_label(mcf_noexpect[c(i)]))))
}

mcf_noexpect_other <- characterize(mcf_data)%>%
  select(noexpect_seek_oth,cell_weights,stratum,gender,geo_entity)%>%
  filter(noexpect_seek_oth!="")
mcf_noexpect_other <- mcf_noexpect_other%>%
  count(noexpect_seek_oth,wt=cell_weights)

gr_noexpect_other<-mcf_noexpect_other%>%
  ggplot(aes(noexpect_seek_oth,n))+
  geom_bar(stat = "identity",position = "dodge",fill=Blue) +
  xlab("") +
  ylab("") +
  ggtitle("") +
  coord_flip()+
  geom_text(
    aes(label = paste0(round(n))),
    position = position_fill(),
    hjust = -0.9,
    size = 4,
    color = "white"
  ) +
  
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = c(Blue, Light_blue),
                    aesthetics = "fill")
survey_data <-
  body_add_gg(survey_data, value = gr_noexpect_other, style = "centered")

# Section C: Education and skills
#readletter variable
gr_readletter_dodge <- preproc_dodge(mcf_data,"readletter",title_prep = str_wrap("Can you read a letter or a simple note?", width = 42))

gr_readletter_fill <- preproc_stack(mcf_data,"readletter",title_prep = str_wrap("Can you read a letter or a simple note?", width = 42))

#writeletter variable
gr_writeletter_dodge <- preproc_dodge(mcf_data,"writeletter",title_prep = str_wrap("If yes, can you write a letter or a simple note?", width = 42))

gr_writeletter_fill <- preproc_stack(mcf_data,"writeletter",title_prep = str_wrap("If yes, can you write a letter or a simple note?", width = 42))

#adult_readwrite variable
gr_adult_readwrite_dodge <- preproc_dodge(mcf_data,"adult_readwrite",title_prep = str_wrap("If no to both of the preceding questions, can any other adult in your household read or write?", width = 42))

gr_adult_readwrite_fill <- preproc_stack(mcf_data,"adult_readwrite",title_prep = str_wrap("If no to both of the preceding questions, can any other adult in your household read or write?", width = 42))

#culculation variable
gr_culculation_dodge <- preproc_dodge(mcf_data,"culculation",title_prep = str_wrap("Can you perform a written calculation?", width = 42))

gr_culculation_fill <- preproc_stack(mcf_data,"culculation",title_prep = str_wrap("Can you perform a written calculation?", width = 42))

#language variable
mcf_lang <- characterize(mcf_data)%>%
  select(matches("language_[0-9]$"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_lang%>%
                   select(matches("language_[0-9]$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_lang,i,title_prep = paste0("I speak:",var_label(mcf_lang[c(i)]))))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_lang,i,title_prep = paste0("I speak:",var_label(mcf_lang[c(i)]))))
}
#language_ability variable
mcf_language_ability <- characterize(mcf_data)%>%
  select(contains("language_ability_"),gender,geo_entity,cell_weights,stratum)
mcf_language_ability$language_ability_1 <- set_variable_labels(mcf_language_ability$language_ability_1,.labels=c(x1="English"))
mcf_language_ability$language_ability_2 <- set_variable_labels(mcf_language_ability$language_ability_2,.labels=c(x1="French"))
mcf_language_ability$language_ability_3 <- set_variable_labels(mcf_language_ability$language_ability_3,.labels=c(x1="Kiswahili"))


for (i in colnames(mcf_language_ability%>%
                   select(contains("language_ability_")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_language_ability,i,title_prep = paste0("How do you use your language ability?:",var_label(mcf_language_ability[c(i)]))))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_language_ability,i,title_prep = paste0("How do you use your language ability?:",var_label(mcf_language_ability[c(i)]))))
}
#education variable
gr_education_dodge <- preproc_dodge(mcf_data,"education",title_prep = str_wrap("What is the highest level of education that you have completed? ", width = 42))

gr_education_fill <- preproc_stack(mcf_data,"education",title_prep = str_wrap("What is the highest level of education that you have completed?", width = 42))

#diploma variable
gr_diploma_dodge <- preproc_dodge(mcf_data,"diploma",title_prep = str_wrap("If e and above, in which field of education is your diploma/certificate/degree?", width = 42))

gr_diploma_fill <- preproc_stack(mcf_data,"diploma",title_prep = str_wrap("If e and above, in which field of education is your diploma/certificate/degree?", width = 42))

#educ_quality variable
gr_educ_quality_dodge <- preproc_dodge(mcf_data,"educ_quality",title_prep = str_wrap("How would you rate the quality of education received?", width = 42))

gr_educ_quality_fill <- preproc_stack(mcf_data,"educ_quality",title_prep = str_wrap("How would you rate the quality of education received?", width = 42))


#educ_knowledge variable
gr_educ_knowledge_dodge <- preproc_dodge(mcf_data,"educ_knowledge",title_prep = str_wrap("To what extent do you think your formal education have given you the type of knowledge and skills needed to get good work opportunities?", width = 42))

gr_educ_knowledge_fill <- preproc_stack(mcf_data,"educ_knowledge",title_prep = str_wrap("To what extent do you think your formal education have given you the type of knowledge and skills needed to get good work opportunities?", width = 42))

#educ_relavent variable
gr_educ_relavent_dodge <- preproc_dodge(mcf_data,"educ_relavent",title_prep = str_wrap("The education I received was relevant to my educational aspirations", width = 42))

gr_educ_relavent_fill <- preproc_stack(mcf_data,"educ_relavent",title_prep = str_wrap("The education I received was relevant to my educational aspirations", width = 42))

#educ_child variable
gr_educ_child_dodge <- preproc_dodge(mcf_data,"educ_child",title_prep = str_wrap("What level of education do you want for your children?", width = 42))

gr_educ_child_fill <- preproc_stack(mcf_data,"educ_child",title_prep = str_wrap("What level of education do you want for your children?", width = 42))

#trainings variable

#training_career variable
gr_training_career_dodge <- preproc_dodge(mcf_data,"training_career",title_prep = str_wrap("If any of the above did the training help you in your career prospects?", width = 42))

gr_training_career_fill <- preproc_stack(mcf_data,"training_career",title_prep = str_wrap("If any of the above did the training help you in your career prospects?", width = 42))

#online_course variable
gr_online_course_dodge <- preproc_dodge(mcf_data,"online_course",title_prep = str_wrap("Have you ever taken an online course?", width = 42))

gr_online_course_fill <- preproc_stack(mcf_data,"online_course",title_prep = str_wrap("Have you ever taken an online course?", width = 42))

#online_helpful variable
gr_online_helpful_dodge <- preproc_dodge(mcf_data,"online_helpful",title_prep = str_wrap("If yes, was it helpful for your professional career? ", width = 42))

gr_online_helpful_fill <- preproc_stack(mcf_data,"online_helpful",title_prep = str_wrap("If yes, was it helpful for your professional career? ", width = 42))

#member_training variable

#curr_pursuing variable
gr_curr_pursuing_dodge <- preproc_dodge(mcf_data,"curr_pursuing",title_prep = str_wrap("Are you currently pursuing any education or training?", width = 42))

gr_curr_pursuing_fill <- preproc_stack(mcf_data,"curr_pursuing",title_prep = str_wrap("Are you currently pursuing any education or training?", width = 42))

#nocurrent_educ variable
gr_nocurrent_educ_dodge <- preproc_dodge(mcf_data,"nocurrent_educ",title_prep = str_wrap("What is the reason you are not pursuing additional education?", width = 42))

gr_nocurrent_educ_fill <- preproc_stack(mcf_data,"nocurrent_educ",title_prep = str_wrap("What is the reason you are not pursuing additional education?", width = 42))


#current_educ variable
gr_current_educ_dodge <- preproc_dodge(mcf_data,"current_educ",title_prep = str_wrap("What level of education are you pursuing?", width = 42))

gr_current_educ_fill <- preproc_stack(mcf_data,"current_educ",title_prep = str_wrap("What level of education are you pursuing?", width = 42))

#future_diploma variable
 

gr_future_diploma_dodge <- preproc_dodge(mcf_data,"future_diploma",title_prep = str_wrap("If e and above, in which field of education is your future diploma/certificate/degree?", width = 42))

gr_future_diploma_fill <- preproc_stack(mcf_data,"future_diploma",title_prep = str_wrap("If e and above, in which field of education is your future diploma/certificate/degree?", width = 42))

print(survey_data, target = "Visuals.docx")
