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
      gsub("^[a-z]+\\.\\s*", "", x)),
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
        gsub("^[a-z]+\\.\\s*", "", x)),
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
        gsub("^[a-z]+\\.\\s*", "", x)),
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
          gsub("^[a-z]+\\.\\s*", "", x)),
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
  select(matches("resp_conditions_[0-9]+$"),gender,geo_entity,cell_weights,stratum)%>%
  mutate(across(matches("resp_conditions_[0-9]+$"),~ifelse(.x==1,"Yes","No")))

for (i in colnames(mcf_cond%>%
                   select(matches("resp_conditions_[0-9]+$")))){
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
  select(matches("progr_[0-9]+$"),gender,geo_entity,cell_weights,stratum)
for (i in colnames(mcf_impl%>%
                   select(matches("progr_[0-9]+$")))){
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
  select(matches("noexpect_seek_[0-9]+$"),gender,geo_entity,cell_weights,stratum)
for (i in colnames(mcf_noexpect%>%
                   select(matches("noexpect_seek_[0-9]+$")))){
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
  select(matches("language_[0-9]+$"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_lang%>%
                   select(matches("language_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_lang,i,title_prep = paste0("I speak:",var_label(mcf_lang[c(i)]))))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_lang,i,title_prep = paste0("I speak:",var_label(mcf_lang[c(i)]))))
}
#language_ability variable
mcf_language_ability <- characterize(mcf_data)%>%
  select(matches("language_ability_[0-9]+$"),gender,geo_entity,cell_weights,stratum)%>%
  mutate(across(matches("language_ability_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_language_ability$language_ability_1 <- set_variable_labels(mcf_language_ability$language_ability_1,.labels=c(x1="English"))
mcf_language_ability$language_ability_2 <- set_variable_labels(mcf_language_ability$language_ability_2,.labels=c(x1="French"))
mcf_language_ability$language_ability_3 <- set_variable_labels(mcf_language_ability$language_ability_3,.labels=c(x1="Kiswahili"))


for (i in colnames(mcf_language_ability%>%
                   select(contains("language_ability_[0-9]+$")))){
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
mcf_training <- characterize(mcf_data)%>%
  select(matches("trainings_[0-9]$"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_training%>%
                   select(matches("trainings_[0-9]$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_training,i,title_prep = str_wrap(paste0("Have you ever participated in training toward any of the following skills?",var_label(mcf_training[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_training,i,title_prep = str_wrap(paste0("Have you ever participated in training toward any of the following skills?",var_label(mcf_training[c(i)])),width = 42)))
}
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
mcf_training <- characterize(mcf_data)%>%
  select(matches("member_training_[0-9]+$"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_training%>%
                   select(matches("member_training_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_training,i,title_prep = str_wrap(paste0("Have you ever participated in training toward any of the following skills?",var_label(mcf_training[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_training,i,title_prep = str_wrap(paste0("Have you ever participated in training toward any of the following skills?",var_label(mcf_training[c(i)])),width = 42)))
}
#curr_pursuing variable
gr_curr_pursuing_dodge <- preproc_dodge(mcf_data,"curr_pursuing",title_prep = str_wrap("Are you currently pursuing any education or training?", width = 42))

gr_curr_pursuing_fill <- preproc_stack(mcf_data,"curr_pursuing",title_prep = str_wrap("Are you currently pursuing any education or training?", width = 42))

#nocurrent_educ variable
gr_nocurrent_educ_dodge <- preproc_dodge(mcf_data,"nocurrent_educ",title_prep = str_wrap("What is the reason you are not pursuing additional education?", width = 42))

gr_nocurrent_educ_fill <- preproc_stack(mcf_data,"nocurrent_educ",title_prep = str_wrap("What is the reason you are not pursuing additional education?", width = 42))


#current_educ variable
gr_current_educ_dodge <- preproc_dodge(mcf_data,"current_educ",title_prep = str_wrap("What level of education are you pursuing?", width = 42))

gr_current_educ_fill <- preproc_stack(mcf_data,"current_educ",title_prep = str_wrap("What level of education are you pursuing?", width = 42))

#current_training variable

mcf_curtraining <- characterize(mcf_data)%>%
  select(matches("current_training_[0-9]+$"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_curtraining%>%
                   select(matches("current_training_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_curtraining,i,title_prep = str_wrap(paste0("Have you ever participated in training toward any of the following skills?",var_label(mcf_curtraining[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_curtraining,i,title_prep = str_wrap(paste0("Have you ever participated in training toward any of the following skills?",var_label(mcf_curtraining[c(i)])),width = 42)))
}
#future_diploma variable

gr_future_diploma_dodge <- preproc_dodge(mcf_data,"future_diploma",title_prep = str_wrap("If e and above, in which field of education is your future diploma/certificate/degree?", width = 42))

gr_future_diploma_fill <- preproc_stack(mcf_data,"future_diploma",title_prep = str_wrap("If e and above, in which field of education is your future diploma/certificate/degree?", width = 42))

# Section D: Access to services and assets

#extent_purchase variable

gr_extent_purchase_dodge <- preproc_dodge(mcf_data,"extent_purchase",title_prep = str_wrap("To what extent do you participate in decision making for purchasing assets and property?", width = 42))

gr_extent_purchase_fill <- preproc_stack(mcf_data,"extent_purchase",title_prep = str_wrap("To what extent do you participate in decision making for purchasing assets and property?", width = 42))

#ownasset variable

mcf_ownasset <- characterize(mcf_data)%>%
  select(matches("ownasset_[0-9]+$"),gender,geo_entity,cell_weights,stratum)%>%
  mutate(across(matches("ownasset_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_ownasset$ownasset_0 <- set_variable_labels(mcf_ownasset$ownasset_0,.labels=c(x1="None"))
mcf_ownasset$ownasset_1 <- set_variable_labels(mcf_ownasset$ownasset_1,.labels=c(x1="A house"))
mcf_ownasset$ownasset_2 <- set_variable_labels(mcf_ownasset$ownasset_2,.labels=c(x1="Agricultural land"))


for (i in colnames(mcf_ownasset%>%
                   select(matches("ownasset_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_ownasset,i,title_prep = str_wrap(paste0("Does your household currently own any of this:",var_label(mcf_ownasset[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_ownasset,i,title_prep = str_wrap(paste0("Does your household currently own any of this:",var_label(mcf_ownasset[c(i)])),width = 42)))
}

#lose_asset variable

mcf_losasset <- characterize(mcf_data)%>%
  select(matches("lose_assets_[0-9]+$"),gender,geo_entity,cell_weights,stratum)%>%
  mutate(across(matches("lose_assets_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_losasset$lose_assets_0 <- set_variable_labels(mcf_losasset$lose_assets_0,.labels=c(x1="None"))
mcf_losasset$lose_assets_1 <- set_variable_labels(mcf_losasset$lose_assets_1,.labels=c(x1="A house"))
mcf_losasset$lose_assets_2 <- set_variable_labels(mcf_losasset$lose_assets_2,.labels=c(x1="Agricultural land"))

for (i in colnames(mcf_losasset%>%
                   select(matches("lose_assets_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_losasset,i,title_prep = str_wrap(paste0("Did you lose any of the above over the past 12 months:",var_label(mcf_losasset[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_losasset,i,title_prep = str_wrap(paste0("Did you lose any of the above over the past 12 months:",var_label(mcf_losasset[c(i)])),width = 42)))
}

#livestock variable

mcf_livestock <- characterize(mcf_data)%>%
  select(matches("^livestocks_[0-9]+$"),gender,geo_entity,cell_weights,stratum)%>%
  mutate(across(matches("^livestocks_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_livestock$livestocks_0 <- set_variable_labels(mcf_livestock$livestocks_0,.labels=c(x1="None"))
mcf_livestock$livestocks_1 <- set_variable_labels(mcf_livestock$livestocks_1,.labels=c(x1="Chickens"))
mcf_livestock$livestocks_2 <- set_variable_labels(mcf_livestock$livestocks_2,.labels=c(x1="Goats"))
mcf_livestock$livestocks_3 <- set_variable_labels(mcf_livestock$livestocks_3,.labels=c(x1="Cows"))
mcf_livestock$livestocks_4 <- set_variable_labels(mcf_livestock$livestocks_4,.labels=c(x1="Pigs"))
mcf_livestock$livestocks_5 <- set_variable_labels(mcf_livestock$livestocks_5,.labels=c(x1="Rabbits"))
mcf_livestock$livestocks_6 <- set_variable_labels(mcf_livestock$livestocks_6,.labels=c(x1="Sheep"))
mcf_livestock$livestocks_7 <- set_variable_labels(mcf_livestock$livestocks_7,.labels=c(x1="Beehives"))

for (i in colnames(mcf_livestock%>%
                   select(matches("^livestocks_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_livestock,i,title_prep = str_wrap(paste0("Does your household currently own any of this:",var_label(mcf_livestock[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_livestock,i,title_prep = str_wrap(paste0("Does your household currently own any of this:",var_label(mcf_livestock[c(i)])),width = 42)))
}

#lose_livestock variable

mcf_lolivestock <- characterize(mcf_data)%>%
  select(matches("lose_livestocks_[0-9]+$"),gender,geo_entity,cell_weights,stratum)%>%
  mutate(across(matches("lose_livestocks_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_lolivestock$lose_livestocks_0 <- set_variable_labels(mcf_lolivestock$lose_livestocks_0,.labels=c(x1="None"))
mcf_lolivestock$lose_livestocks_1 <- set_variable_labels(mcf_lolivestock$lose_livestocks_1,.labels=c(x1="Chickens"))
mcf_lolivestock$lose_livestocks_2 <- set_variable_labels(mcf_lolivestock$lose_livestocks_2,.labels=c(x1="Goats"))
mcf_lolivestock$lose_livestocks_3 <- set_variable_labels(mcf_lolivestock$lose_livestocks_3,.labels=c(x1="Cows"))
mcf_lolivestock$lose_livestocks_4 <- set_variable_labels(mcf_lolivestock$lose_livestocks_4,.labels=c(x1="Pigs"))
mcf_lolivestock$lose_livestocks_5 <- set_variable_labels(mcf_lolivestock$lose_livestocks_5,.labels=c(x1="Rabbits"))
mcf_lolivestock$lose_livestocks_6 <- set_variable_labels(mcf_lolivestock$lose_livestocks_6,.labels=c(x1="Sheep"))
mcf_lolivestock$lose_livestocks_7 <- set_variable_labels(mcf_lolivestock$lose_livestocks_7,.labels=c(x1="Beehives"))

for (i in colnames(mcf_lolivestock%>%
                   select(matches("lose_livestocks_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_lolivestock,i,title_prep = str_wrap(paste0("Did you lose any of the above over the past 12 months:",var_label(mcf_lolivestock[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_lolivestock,i,title_prep = str_wrap(paste0("Did you lose any of the above over the past 12 months:",var_label(mcf_lolivestock[c(i)])),width = 42)))
}

#equiment variable

mcf_equiment <- characterize(mcf_data)%>%
  select(matches("^equiment_[0-9]+$"),gender,geo_entity,cell_weights,stratum)%>%
  mutate(across(matches("^equiment_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_equiment$equiment_0 <- set_variable_labels(mcf_equiment$equiment_0,.labels=c(x1="None"))
mcf_equiment$equiment_1 <- set_variable_labels(mcf_equiment$equiment_1,.labels=c(x1="Television"))
mcf_equiment$equiment_2 <- set_variable_labels(mcf_equiment$equiment_2,.labels=c(x1="Radio"))
mcf_equiment$equiment_3 <- set_variable_labels(mcf_equiment$equiment_3,.labels=c(x1="Watch"))
mcf_equiment$equiment_4 <- set_variable_labels(mcf_equiment$equiment_4,.labels=c(x1="Feature phone"))
mcf_equiment$equiment_5 <- set_variable_labels(mcf_equiment$equiment_5,.labels=c(x1="Smartphone"))
mcf_equiment$equiment_6 <- set_variable_labels(mcf_equiment$equiment_6,.labels=c(x1="A computer"))
mcf_equiment$equiment_7 <- set_variable_labels(mcf_equiment$equiment_7,.labels=c(x1="Car"))
mcf_equiment$equiment_8 <- set_variable_labels(mcf_equiment$equiment_8,.labels=c(x1="Bicycle"))

for (i in colnames(mcf_equiment%>%
                   select(matches("^equiment_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_equiment,i,title_prep = str_wrap(paste0("Does your household currently own any of this:",var_label(mcf_equiment[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_equiment,i,title_prep = str_wrap(paste0("Does your household currently own any of this:",var_label(mcf_equiment[c(i)])),width = 42)))
}

#lose_equiment variable

mcf_loequiment <- characterize(mcf_data)%>%
  select(matches("lose_equiment_[0-9]+$"),gender,geo_entity,cell_weights,stratum)%>%
  mutate(across(matches("lose_equiment_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_loequiment$lose_equiment_0 <- set_variable_labels(mcf_loequiment$lose_equiment_0,.labels=c(x1="None"))
mcf_loequiment$lose_equiment_1 <- set_variable_labels(mcf_loequiment$lose_equiment_1,.labels=c(x1="Television"))
mcf_loequiment$lose_equiment_2 <- set_variable_labels(mcf_loequiment$lose_equiment_2,.labels=c(x1="Radio"))
mcf_loequiment$lose_equiment_3 <- set_variable_labels(mcf_loequiment$lose_equiment_3,.labels=c(x1="Watch"))
mcf_loequiment$lose_equiment_4 <- set_variable_labels(mcf_loequiment$lose_equiment_4,.labels=c(x1="Feature phone"))
mcf_loequiment$lose_equiment_5 <- set_variable_labels(mcf_loequiment$lose_equiment_5,.labels=c(x1="Smartphone"))
mcf_loequiment$lose_equiment_6 <- set_variable_labels(mcf_loequiment$lose_equiment_6,.labels=c(x1="A computer"))
mcf_loequiment$lose_equiment_7 <- set_variable_labels(mcf_loequiment$lose_equiment_7,.labels=c(x1="Car"))
mcf_loequiment$lose_equiment_8 <- set_variable_labels(mcf_loequiment$lose_equiment_8,.labels=c(x1="Bicycle"))

for (i in colnames(mcf_loequiment%>%
                   select(matches("lose_equiment_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_loequiment,i,title_prep = str_wrap(paste0("Did you lose any of the above over the past 12 months:",var_label(mcf_loequiment[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_loequiment,i,title_prep = str_wrap(paste0("Did you lose any of the above over the past 12 months:",var_label(mcf_loequiment[c(i)])),width = 42)))
}

#source_water variable

gr_source_water_dodge <- preproc_dodge(mcf_data,"source_water",title_prep = str_wrap("What is the main source of drinking water for members of your household?", width = 42))

gr_source_water_fill <- preproc_stack(mcf_data,"source_water",title_prep = str_wrap("What is the main source of drinking water for members of your household?", width = 42))

#have_electricity variable

gr_have_electricity_dodge <- preproc_dodge(mcf_data,"have_electricity",title_prep = str_wrap("Does your household have electricity?", width = 42))

gr_have_electricity_fill <- preproc_stack(mcf_data,"have_electricity",title_prep = str_wrap("Does your household have electricity?", width = 42))

#toilet_facility variable

gr_toilet_facility_dodge <- preproc_dodge(mcf_data,"toilet_facility",title_prep = str_wrap("What kind of toilet facility do members of your household usually use?", width = 42))

gr_toilet_facility_fill <- preproc_stack(mcf_data,"toilet_facility",title_prep = str_wrap("What kind of toilet facility do members of your househotoilet_facility", width = 42))

#floor variable

gr_floor_dodge <- preproc_dodge(mcf_data,"floor",title_prep = str_wrap("What is the main material of the floor in your household?", width = 42))

gr_floor_fill <- preproc_stack(mcf_data,"floor",title_prep = str_wrap("What is the main material of the floor in your household?", width = 42))

#exterior_walls variable

gr_exterior_walls_dodge <- preproc_dodge(mcf_data,"exterior_walls",title_prep = str_wrap("What is the main material of the exterior walls in your household?", width = 42))

gr_exterior_walls_fill <- preproc_stack(mcf_data,"exterior_walls",title_prep = str_wrap("What is the main material of the exterior walls in your household?", width = 42))

vars_df <-
  c(
    "fuel_cooking","cropinsurance","healthcare","healthcare_comp","clean_water",
    "clean_water_comp","sanitation","sanitation_comp","electricity","electricity_comp",
    "telephone","telephone_comp","internet","internet_comp","transport","transport_comp",
    "food","food_comp","roads","roads_comp","loans","loans_comp","bank_account", 	
    "bank_account_comp","nature","nature_comp"
  )
var_lab <- 
  c(
    "What type of fuel does your household mainly use for cooking?", 
    "Are there places in this village or near here where people can 
    get insurance (e.g. crop insurance, livestock)?", 
    "Over the past 12 months, how easily did you access to health care services",  
    "How does access over the past 12 months compare to the year before?",
    "Over the past 12 months, how easily did you access to clean water", 
    "How does access over the past 12 months compare to the year before?", 
      "Over the past 12 months, how easily did you access to safe sanitation", 
    "How does access over the past 12 months compare to the year before?", 
      "Over the past 12 months, how easily did you access to electricity", 
    "How does access over the past 12 months compare to the year before?", 
      "Over the past 12 months, how easily did you access to telephone services", 
    "How does access over the past 12 months compare to the year before?", 
      "Over the past 12 months, how easily did you access to internet services", 
    "How does access over the past 12 months compare to the year before?", 
      "Over the past 12 months, how easily did you access to transport", 
    "How does access over the past 12 months compare to the year before?", 
      "Over the past 12 months, how easily did you access to food", 
    "How does access over the past 12 months compare to the year before?",
      "Over the past 12 months, how easily did you access to roads and trading points", 
    "How does access over the past 12 months compare to the year before?",
      "Over the past 12 months, how easily did you access to loans", 
    "How does access over the past 12 months compare to the year before?",
      "Over the past 12 months, how easily did you access a bank account", 
    "How does access over the past 12 months compare to the year before?",
    "Over the past 12 months, how easily did you access places in nature to enjoy", 
    "How does access over the past 12 months compare to the year before?"
      
  )
meshed <- data.frame(x=vars_df,y=var_lab)

for (i in 1:nrow(meshed[,c("x","y")][1])){
  assign(paste(meshed[,c("x","y")][i,1],"_dodge_graph"),preproc_dodge(mcf_data,meshed[,c("x","y")][i,1],title_prep = str_wrap(meshed[,c("x","y")][i,2], width = 42)))
  assign(paste(meshed[,c("x","y")][i,1],"_stack_graph"),preproc_stack(mcf_data,meshed[,c("x","y")][i,1],title_prep = str_wrap(meshed[,c("x","y")][i,2], width = 42)))
  
}


# SECTION E:  Employment (responses only from employed/self-employed) 

vars_df <-
  c(
    "give_chance","type_employ","title","inc_impro","wor_condi","chang_earning",
    "chang_condition","indi_need","fami_need","resp_workplace","resp_carabout",
    "sense_purp","fully_ambition"
    
  )
var_lab <- 
  c(
    "If given the chance, would you like to work more hours than you currently do?", 
    "In the income generating work that you spend most time on, are you self-employed or wage employed?", 
    "In the job that you spend the most time on (${main_activity}), which of the following occupations describe best your current title?", 
    "During the past 3 months, has your income from ${main_activity} (your main activity)improved?", 
    "During the past 3 months, has the working conditions in ${main_activity} ( your main activity) improved",
    "Compared to the same time last year, have you seen any positive or negative changes in your earnings?", 
    "Compared to the same time last year, have you seen any positive or negative  changes in your working conditions?", 
    "Your work pays you enough to meet your individual needs", 
    "Your work pays you enough to meet your family needs", 
    "You feel respected in your workplace?", 
    "Your work is respected by those you care about?",   
    "Your work gives you a sense of purpose?", 
    "Your work is fully aligned with your ambition?"
      
  )
meshed <- data.frame(x=vars_df,y=var_lab)

for (i in 1:nrow(meshed[,c("x","y")][1])){
  assign(paste(meshed[,c("x","y")][i,1],"_dodge_graph"),preproc_dodge(mcf_data,meshed[,c("x","y")][i,1],title_prep = str_wrap(meshed[,c("x","y")][i,2], width = 42)))
  assign(paste(meshed[,c("x","y")][i,1],"_stack_graph"),preproc_stack(mcf_data,meshed[,c("x","y")][i,1],title_prep = str_wrap(meshed[,c("x","y")][i,2], width = 42)))
  
}

# SECTION F:  Specific to Self-employed 

vars_df <-
  c(
    "job_accept","any_working","intro_prod","new_mark","out_supp"
  )
var_lab <- 
  c(
    "If you were offered a full-time wage job to cover the financial needs of you and your dependents, would you accept?",
    "Do you have anyone working for you?",
    "Have you introduced any new products/services over the past 12 months?", 
    "Have you found new markets in the past 12 months?",  
    "Have you ever received any kind of outside support?" 
  )
meshed <- data.frame(x=vars_df,y=var_lab)

for (i in 1:nrow(meshed[,c("x","y")][1])){
  assign(paste(meshed[,c("x","y")][i,1],"_dodge_graph"),preproc_dodge(mcf_data,meshed[,c("x","y")][i,1],title_prep = str_wrap(meshed[,c("x","y")][i,2], width = 42)))
  assign(paste(meshed[,c("x","y")][i,1],"_stack_graph"),preproc_stack(mcf_data,meshed[,c("x","y")][i,1],title_prep = str_wrap(meshed[,c("x","y")][i,2], width = 42)))
  
}

#kindsup variable

mcf_kindsup <- characterize(mcf_data)%>%
  select(matches("^kindsup_[0-9]+$"),gender,geo_entity,cell_weights,stratum)%>%
  mutate(across(matches("^kindsup_[0-9]+$"),~ifelse(.x==1,"Yes","No")))
mcf_kindsup$kindsup_1 <- set_variable_labels(mcf_kindsup$kindsup_1,.labels=c(x1="Financial"))
mcf_kindsup$kindsup_2 <- set_variable_labels(mcf_kindsup$kindsup_2,.labels=c(x1="Business development training"))
mcf_kindsup$kindsup_3 <- set_variable_labels(mcf_kindsup$kindsup_3,.labels=c(x1="Technical training"))
mcf_kindsup$kindsup_4 <- set_variable_labels(mcf_kindsup$kindsup_4,.labels=c(x1="Farming Support"))
mcf_kindsup$kindsup_5 <- set_variable_labels(mcf_kindsup$kindsup_5,.labels=c(x1="Food Support"))
mcf_kindsup$kindsup_6 <- set_variable_labels(mcf_kindsup$kindsup_6,.labels=c(x1="Health Insurance"))
mcf_kindsup$kindsup_98 <- set_variable_labels(mcf_kindsup$kindsup_98,.labels=c(x1="Other (explain)"))

for (i in colnames(mcf_kindsup%>%
                   select(matches("kindsup_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_kindsup,i,title_prep = str_wrap(paste0("If yes, what kind of support?",var_label(mcf_kindsup[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_kindsup,i,title_prep = str_wrap(paste0("If yes, what kind of support?",var_label(mcf_kindsup[c(i)])),width = 42)))
}

#fro_whom variable

mcf_frowhom <- characterize(mcf_data)%>%
  select(matches("fro_whom_[0-9]+_1$"),gender,geo_entity,cell_weights,stratum)
for (i in colnames(mcf_frowhom%>%
                   select(matches("fro_whom_[0-9]+_1$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_frowhom,i,title_prep = str_wrap(paste0("From whom:",var_label(mcf_frowhom[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_frowhom,i,title_prep = str_wrap(paste0("From whom:",var_label(mcf_frowhom[c(i)])),width = 42)))
}

# SECTION G:  Digital
vars_df <-
  c(
    "phone_ownership",
    "internet_access" 
  )
var_lab <- 
  c(
    "Do you own a phone?", 
    "Can the phone access the internet?" 
  )
meshed <- data.frame(x=vars_df,y=var_lab)

for (i in 1:nrow(meshed[,c("x","y")][1])){
  assign(paste(meshed[,c("x","y")][i,1],"_dodge_graph"),preproc_dodge(mcf_data,meshed[,c("x","y")][i,1],title_prep = str_wrap(meshed[,c("x","y")][i,2], width = 42)))
  assign(paste(meshed[,c("x","y")][i,1],"_stack_graph"),preproc_stack(mcf_data,meshed[,c("x","y")][i,1],title_prep = str_wrap(meshed[,c("x","y")][i,2], width = 42)))
  
}

# phone_use variable visuals
mcf_phoneuse <- characterize(mcf_data)%>%
  select(matches("^phone_use_[0-9]+$"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_phoneuse%>%
                   select(matches("^phone_use_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_phoneuse,i,title_prep = str_wrap(paste0("What do you normally use a phone for?",var_label(mcf_phoneuse[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_phoneuse,i,title_prep = str_wrap(paste0("What do you normally use a phone for?",var_label(mcf_phoneuse[c(i)])),width = 42)))
}

#computer_ownership variable visuals
gr_computer_ownership_dodge <- preproc_dodge(mcf_data,"computer_ownership",title_prep = str_wrap("Do you own a computer? ", width = 42))

gr_computer_ownership_fill <- preproc_stack(mcf_data,"computer_ownership",title_prep = str_wrap("Do you own a computer? ", width = 42))

# computer_use variable visuals
mcf_compuse <- characterize(mcf_data)%>%
  select(matches("^computer_use_[0-9]+$"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_compuse%>%
                   select(matches("^computer_use_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_compuse,i,title_prep = str_wrap(paste0(" what do you normally use your computer for?",var_label(mcf_compuse[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_compuse,i,title_prep = str_wrap(paste0(" what do you normally use your computer for?",var_label(mcf_compuse[c(i)])),width = 42)))
}

# digital_use variable visuals
mcf_diguse <- characterize(mcf_data)%>%
  select(matches("^digital_use_[0-9]+$"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_diguse%>%
                   select(matches("^digital_use_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_diguse,i,title_prep = str_wrap(paste0(" what do you normally use your computer for?",var_label(mcf_diguse[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_diguse,i,title_prep = str_wrap(paste0(" what do you normally use your computer for?",var_label(mcf_diguse[c(i)])),width = 42)))
}


#digital_use_in_job variable visuals
gr_digital_use_in_job_dodge <- preproc_dodge(mcf_data,"digitaluseinjob",title_prep = str_wrap("To what extent do you use digital tools in your current job?", width = 42))

gr_digital_use_in_job_fill <- preproc_stack(mcf_data,"digitaluseinjob",title_prep = str_wrap("To what extent do you use digital tools in your current job?", width = 42))

# computer_use variable visuals
mcf_reasnot <- characterize(mcf_data)%>%
  select(matches("^reasons_not_usingdigt_[0-9]+$"),gender,geo_entity,cell_weights,stratum)

for (i in colnames(mcf_reasnot%>%
                   select(matches("^reasons_not_usingdigt_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_reasnot,i,title_prep = str_wrap(paste0("For what reasons don’t you use digital tools in your current job? (If  c or d to the above question) ",var_label(mcf_reasnot[c(i)])),width=42)))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_reasnot,i,title_prep = str_wrap(paste0("For what reasons don’t you use digital tools in your current job? (If  c or d to the above question) ",var_label(mcf_reasnot[c(i)])),width = 42)))
}








print(survey_data, target = "G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/Data visualization Parfait/Visuals1.docx")
