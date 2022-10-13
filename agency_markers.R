#This script shows all processes undertaken to calculate values
#for quantitative indicators in the baseline matrix 

#Load required packages
library(foreign) #for reading .dta files
library(dplyr) #for the filter function
library(questionr) #for the weight function 
library(haven)
library(openxlsx)
library(readr)
library(tidyr)
library(rio)
library(expss)
library(labelled)
library(sjmisc)
library(ggplot2)
library(webr)
library(lubridate)
library(readxl)
library(openxlsx)
library(readxl)

#Load in data ------------ 
mcf_data<-read_sav("data/mcf_data_new.sav")

#Data modification 
var_label(mcf_data$move_impro)<-'H10. Are you willing to move somewhere else to improve your life?'
var_label(mcf_data$any_save)<-'H11. Do you or any other household member regularly save up money?'
mcf_data<-mcf_data%>%
  set_value_labels(sector_choices_reasons_4=c(`Financial Motivation`=2))%>%
  dplyr::rename(youth_country_lead=youth_contrib_innov, youth_contrib_innov=youth_improv_envir,
         youth_improv_envir=youth_gender_equity,youth_gender_equity=youth_work_condition,
         youth_country_lead_2=youth_contrib_innov_2, youth_contrib_innov_2=youth_improv_envir_2,
         youth_improv_envir_2=youth_gender_equity_2,youth_gender_equity_2=youth_work_condition_2,
         youth_country_lead_3=youth_contrib_innov_3, youth_contrib_innov_3=youth_improv_envir_3,
         youth_improv_envir_3=youth_gender_equity_3,youth_gender_equity_3=youth_work_condition_3,
         youth_country_lead_4=youth_contrib_innov_4, youth_contrib_innov_4=youth_improv_envir_4,
         youth_improv_envir_4=youth_gender_equity_4,youth_gender_equity_4=youth_work_condition_4)%>%
  set_value_labels(main_activity=c(`Wholesale and retail trade; repair of motor vehicles and motorcycles`='G',
                                   `Manufacturing`='C',
                                   `Professional, scientific and technical activities`='M',
                                   `Accommodation and food service activities`='I',
                                   `Transportation and storage`='H',
                                   `Other service activities`='S',
                                   `Agriculture; forestry and fishing`='A',
                                   `Information and communication`='J',
                                   `Education`='P',
                                   `Human health and social work activities`='Q',
                                   `Construction`='F',
                                   `Arts, entertainment and recreation`='R',
                                   `Administrative and support service activities`='N',
                                   `Electricity; gas, steam and air conditioning supply`='D',
                                   `Mining and quarrying`='B',
                                   `Public administration and defence; compulsory social security`='O',
                                   `Financial and insurance activities`='K',
                                   `Activities of households as employers; undifferentiated goods- and services-producing activities of households for own use`='T',
                                   `Real estate activities`='L',
                                   `Water supply; sewerage, waste management and remediation activities`='E'),
                   noseek=c(`a. Vacation/leave`=1, `b. Maternity, parental, or sick leave`=2,
                            `c. Currently a student`=3, `d. Doing unpaid work for family income`=4,
                            `e. Own illness or disability`=5, `f. Household chores`=6,
                            `g. Caring for own children`=7,`h. Caring for relatives other than own child`=8,
                            `i. Pregnancy`=9, `j. Unpaid internship in a business not owned by the family`=10,
                            `k. I had a job`=11,`l. Other (specify)`=98,`l. Other (specify)`=90))
mcf_data1<-characterize(mcf_data)
mcf_data1<-as.data.frame(lapply(mcf_data1, function(x) gsub('^[a-z]. *','', x)))

mcf_data1[]<-lapply(mcf_data1, function(x) gsub('^[a-z]. *','', x))
write_labelled_xlsx(mcf_data,'mcf_baseline.xlsx')

a<-read.xlsx('mcf_baseline.xlsx')
variables<-(var_label(mcf_data)) # this will create a list of variables and their labels
`%notin%` <- Negate(`%in%`)


#L5.1.1 a----
#Create four different data sets 
data1<-as.data.frame(mcf_data)%>%
  select(uniqueid,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_1:youth_improv_work_1)) #add weights column 
data2<-as.data.frame(mcf_data)%>%
  select(uniqueid,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_2:youth_improv_work_2))
data3<-as.data.frame(mcf_data)%>%
  select(uniqueid,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_3:youth_improv_work_3))
data4<-as.data.frame(mcf_data)%>%
  select(uniqueid,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_4:youth_improv_work_4))

#Match columns name for each dataset 
colnames(data4)<-colnames(data1)
colnames(data3)<-colnames(data1)
colnames(data2)<-colnames(data1)

combination1<-rbind.fill(data1,data2,data3,data4)

combination<-as.data.frame(combination1)%>% #should have weights 
  filter(!is.na(opp_start_business_1))%>%
  filter(sector_choices_name_1!='')%>%
  select(-c(agreeing_1))%>%
  set_value_labels(ease_access_digni_1=c(`e. Very easy`=5,  `d. Easy`=4, `c. Difficult`=3,  `b. Very difficult`=2, `a. Haven't worked in this sector`=1))
combination$sector_choices_name_1[combination$sector_choices_name_1=='Creative industries']<-'Tourism&Hospitality'

#Overall access to employment ----
combination2<-combination%>%
  select(uniqueid,gender, geo_entity, refuge, pwd, age_group, stratum,
         ease_access_digni_1 ,employing_youth_1,youth_country_lead_1,
         opp_start_business_1,employing_women_1,opp_lead_position_1,
         youth_advancement_1, weights)%>%
  # group_by(uniqueid,gender, geo_entity, refuge, pwd, age_group, stratum)%>%
  # dplyr::summarise(across(ease_access_digni_1:weights,~mean(.x, na.rm=TRUE)))%>%
  rowwise()%>%
  mutate(access=mean(employing_youth_1:opp_lead_position_1, na.rm=TRUE), 
         access1=round(mean((ease_access_digni_1:youth_advancement_1), na.rm=TRUE)),
         access2=case_when(access1%in%c(4,5)~1, access1%in%c(1,2,3)~0))
markers_access<-combination%>%
  select(stratum,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
  group_by()%>%
  dplyr::summarize(across(ease_access_digni_1:youth_advancement_1, ~weighted.mean(.x, weights, na.rm=TRUE)))

markers_access_gender <- combination %>%
  select(
    gender,
    stratum,
    ease_access_digni_1 ,
    employing_youth_1,
    youth_country_lead_1,
    opp_start_business_1,
    employing_women_1,
    opp_lead_position_1,
    youth_advancement_1,
    weights
  ) %>%
  group_by(gender) %>%
  dplyr::summarize(across(
    ease_access_digni_1:youth_advancement_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = c("ease_access_digni_1" ,
    "employing_youth_1",
    "youth_country_lead_1",
    "opp_start_business_1",
    "employing_women_1",
    "opp_lead_position_1",
    "youth_advancement_1")
  ) %>%
  as.data.frame() 

markers_access_gender <-
  markers_access_gender %>%
  mutate(gender =
           ifelse(gender == 2,
                  "Female", "Male")) %>%
  pivot_wider(names_from = "gender") %>%
  move_columns(Male,.after = Female)

markers_access_refuge <- combination %>%
  select(
    refuge,
    stratum,
    ease_access_digni_1 ,
    employing_youth_1,
    youth_country_lead_1,
    opp_start_business_1,
    employing_women_1,
    opp_lead_position_1,
    youth_advancement_1,
    weights
  ) %>%
  group_by(refuge) %>%
  dplyr::summarize(across(
    ease_access_digni_1:youth_advancement_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  dplyr::filter(refuge==2)%>%
  pivot_longer(cols = c("ease_access_digni_1" ,
                        "employing_youth_1",
                        "youth_country_lead_1",
                        "opp_start_business_1",
                        "employing_women_1",
                        "opp_lead_position_1",
                        "youth_advancement_1")
  ) %>%
  as.data.frame()%>%
  select(-refuge,`IDP/ Refugee`=value)


markers_access_pwd <- combination %>%
  select(
    pwd,
    stratum,
    ease_access_digni_1 ,
    employing_youth_1,
    youth_country_lead_1,
    opp_start_business_1,
    employing_women_1,
    opp_lead_position_1,
    youth_advancement_1,
    weights
  ) %>%
  group_by(pwd) %>%
  dplyr::summarize(across(
    ease_access_digni_1:youth_advancement_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  dplyr::filter(pwd==1)%>%
  pivot_longer(cols = c("ease_access_digni_1" ,
                        "employing_youth_1",
                        "youth_country_lead_1",
                        "opp_start_business_1",
                        "employing_women_1",
                        "opp_lead_position_1",
                        "youth_advancement_1")
  ) %>%
  as.data.frame()%>%
  select(-pwd,PWD=value)


markers_access_geo <- combination %>%
  select(
    geo_entity,
    stratum,
    ease_access_digni_1 ,
    employing_youth_1,
    youth_country_lead_1,
    opp_start_business_1,
    employing_women_1,
    opp_lead_position_1,
    youth_advancement_1,
    weights
  ) %>%
  group_by(geo_entity) %>%
  dplyr::summarize(across(
    ease_access_digni_1:youth_advancement_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = c("ease_access_digni_1" ,
                        "employing_youth_1",
                        "youth_country_lead_1",
                        "opp_start_business_1",
                        "employing_women_1",
                        "opp_lead_position_1",
                        "youth_advancement_1")
  ) %>%
  as.data.frame() %>%
  mutate(geo_entity =
           ifelse(geo_entity == 2,
                  "Rural", "Urban")) %>%
  pivot_wider(names_from = "geo_entity") %>%
  move_columns(Urban,.after = Rural)

markers_access_age <- combination %>%
  select(
    age_group,
    stratum,
    ease_access_digni_1 ,
    employing_youth_1,
    youth_country_lead_1,
    opp_start_business_1,
    employing_women_1,
    opp_lead_position_1,
    youth_advancement_1,
    weights
  ) %>%
  group_by(age_group) %>%
  dplyr::summarize(across(
    ease_access_digni_1:youth_advancement_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(cols = c("ease_access_digni_1" ,
                        "employing_youth_1",
                        "youth_country_lead_1",
                        "opp_start_business_1",
                        "employing_women_1",
                        "opp_lead_position_1",
                        "youth_advancement_1")
  ) %>%
  as.data.frame() %>%
  pivot_wider(names_from = "age_group")

markers_access_stratum <- combination %>%
  select(
    stratum,
    ease_access_digni_1 ,
    employing_youth_1,
    youth_country_lead_1,
    opp_start_business_1,
    employing_women_1,
    opp_lead_position_1,
    youth_advancement_1,
    weights
  ) %>%
  group_by(stratum) %>%
  dplyr::summarize(across(
    ease_access_digni_1:youth_advancement_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(
      "ease_access_digni_1" ,
      "employing_youth_1",
      "youth_country_lead_1",
      "opp_start_business_1",
      "employing_women_1",
      "opp_lead_position_1",
      "youth_advancement_1"
    )
  ) %>%
  as.data.frame() %>%
  mutate(
    stratum =
      case_when(
        stratum == 1 ~ "Wage employed",
        stratum == 2 ~ "Self employed",
        stratum == 3 ~ "Students",
        stratum == 4 ~ "Unemployed/Non job-seekers"
      )
  ) %>%
  pivot_wider(names_from = "stratum") %>%
  move_columns(`Wage employed`,.after = `Self employed`) %>%
  move_columns(`Unemployed/Non job-seekers`,.before = Students)

combine_access <- markers_access_gender%>%
  left_join(markers_access_refuge)%>%
  left_join(markers_access_pwd)%>%
  left_join(markers_access_geo)%>%
  left_join(markers_access_age)%>%
  left_join(markers_access_stratum)

#write.xlsx(combine_access,"data/access_disag.xlsx")


#L.5.1.1b - new ----------------------------
#Overall access to employment ---
combination5<-combination%>%
  select(uniqueid, gender, geo_entity, refuge, pwd, age_group, stratum,geo_entity, youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, youth_gender_equity_1,youth_improv_work_1, weights)%>%
  rowwise()%>%
  mutate(growth=mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE), 
         growth1=round(mean((youth_contrib_econ_1:youth_improv_work_1), na.rm=TRUE)),
         growth2=case_when(growth1%in%c(4,5)~1, growth1%in%c(1,2,3)~0))

markers_participation<-combination%>%
  select(stratum,youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, 
         youth_gender_equity_1,youth_improv_work_1, weights)%>%
  group_by()%>%
  dplyr::summarize(across(youth_contrib_econ_1:youth_improv_work_1, ~weighted.mean(.x, weights, na.rm=TRUE)))


markers_participation_gender <- combination %>%
  select(
    gender,
    stratum,
    youth_contrib_econ_1,
    youth_contrib_innov_1,
    youth_improv_envir_1,
    youth_particp_local_1,
    youth_gender_equity_1,
    youth_improv_work_1,
    weights
  ) %>%
  group_by(gender) %>%
  dplyr::summarize(across(
    youth_contrib_econ_1:youth_improv_work_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(
      "youth_contrib_econ_1" ,
      "youth_contrib_innov_1",
      "youth_improv_envir_1",
      "youth_particp_local_1",
      "youth_gender_equity_1",
      "youth_improv_work_1"
    )
  ) %>%
  as.data.frame()%>%
  mutate(gender =
           ifelse(gender == 2,
                  "Female", "Male")) %>%
  pivot_wider(names_from = "gender") %>%
  move_columns(Male,.after = Female)

markers_participation_refuge <- combination %>%
  select(
    refuge,
    stratum,
    youth_contrib_econ_1,
    youth_contrib_innov_1,
    youth_improv_envir_1,
    youth_particp_local_1,
    youth_gender_equity_1,
    youth_improv_work_1,
    weights
  ) %>%
  group_by(refuge) %>%
  dplyr::summarize(across(
    youth_contrib_econ_1:youth_improv_work_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>% 
  dplyr::filter(refuge==2)%>%
  pivot_longer(
    cols = c(
      "youth_contrib_econ_1" ,
      "youth_contrib_innov_1",
      "youth_improv_envir_1",
      "youth_particp_local_1",
      "youth_gender_equity_1",
      "youth_improv_work_1"
    )) %>%
  as.data.frame()%>%
  select(-refuge,`IDP/ Refugee`=value)


markers_participation_pwd <- combination %>%
  select(
    pwd,
    stratum,
    youth_contrib_econ_1,
    youth_contrib_innov_1,
    youth_improv_envir_1,
    youth_particp_local_1,
    youth_gender_equity_1,
    youth_improv_work_1,
    weights
  ) %>%
  group_by(pwd) %>%
  dplyr::summarize(across(
    youth_contrib_econ_1:youth_improv_work_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  dplyr::filter(pwd==1)%>%
  pivot_longer(
    cols = c(
      "youth_contrib_econ_1" ,
      "youth_contrib_innov_1",
      "youth_improv_envir_1",
      "youth_particp_local_1",
      "youth_gender_equity_1",
      "youth_improv_work_1"
    )) %>%
  as.data.frame()%>%
  select(-pwd,PWD=value)


markers_participation_geo <- combination %>%
  select(
    geo_entity,
    stratum,
    youth_contrib_econ_1,
    youth_contrib_innov_1,
    youth_improv_envir_1,
    youth_particp_local_1,
    youth_gender_equity_1,
    youth_improv_work_1,
    weights
  ) %>%
  group_by(geo_entity) %>%
  dplyr::summarize(across(
    youth_contrib_econ_1:youth_improv_work_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(
      "youth_contrib_econ_1" ,
      "youth_contrib_innov_1",
      "youth_improv_envir_1",
      "youth_particp_local_1",
      "youth_gender_equity_1",
      "youth_improv_work_1"
    )
  ) %>%
  as.data.frame() %>%
  mutate(geo_entity =
           ifelse(geo_entity == 2,
                  "Rural", "Urban")) %>%
  pivot_wider(names_from = "geo_entity") %>%
  move_columns(Urban,.after = Rural)

markers_participation_age <- combination %>%
  select(
    age_group,
    stratum,
    youth_contrib_econ_1,
    youth_contrib_innov_1,
    youth_improv_envir_1,
    youth_particp_local_1,
    youth_gender_equity_1,
    youth_improv_work_1,
    weights
  ) %>%
  group_by(age_group) %>%
  dplyr::summarize(across(
    youth_contrib_econ_1:youth_improv_work_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(
      "youth_contrib_econ_1" ,
      "youth_contrib_innov_1",
      "youth_improv_envir_1",
      "youth_particp_local_1",
      "youth_gender_equity_1",
      "youth_improv_work_1"
    )
  ) %>%
  as.data.frame() %>%
  pivot_wider(names_from = "age_group")

markers_participation_stratum <- combination %>%
  select(
    stratum,
    youth_contrib_econ_1,
    youth_contrib_innov_1,
    youth_improv_envir_1,
    youth_particp_local_1,
    youth_gender_equity_1,
    youth_improv_work_1,
    weights
  ) %>%
  group_by(stratum) %>%
  dplyr::summarize(across(
    youth_contrib_econ_1:youth_improv_work_1,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(
      "youth_contrib_econ_1" ,
      "youth_contrib_innov_1",
      "youth_improv_envir_1",
      "youth_particp_local_1",
      "youth_gender_equity_1",
      "youth_improv_work_1"
    )
  ) %>%
  as.data.frame() %>%
  mutate(
    stratum =
      case_when(
        stratum == 1 ~ "Wage employed",
        stratum == 2 ~ "Self employed",
        stratum == 3 ~ "Students",
        stratum == 4 ~ "Unemployed/Non job-seekers"
      )
  ) %>%
  pivot_wider(names_from = "stratum") %>%
  move_columns(`Wage employed`,.after = `Self employed`) %>%
  move_columns(`Unemployed/Non job-seekers`,.before = Students)

combine_participation <- markers_participation_gender%>%
  left_join(markers_participation_refuge)%>%
  left_join(markers_participation_pwd)%>%
  left_join(markers_participation_geo)%>%
  left_join(markers_participation_age)%>%
  left_join(markers_participation_stratum)

#write.xlsx(combine_participation,"data/markers_participations.xlsx")



#L5.1.2a this is not a sector specific analysis-----------------
combination8<-mcf_data%>%
  select(uniqueid, gender, geo_entity, refuge, pwd, age_group, stratum,geo_entity,get_work,workplaces_val,
         work_rewards,workplace_equit, weights)%>%
  filter(!is.na(get_work))





markers_aspiration_gender <- combination8 %>%
  select(gender,
         get_work,
         workplaces_val,
         work_rewards,
         workplace_equit,
         weights) %>%
  group_by(gender) %>%
  dplyr::summarize(across(
    get_work:workplace_equit,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(
      "get_work" ,
      "workplaces_val",
      "work_rewards",
      "workplace_equit"
    )
  ) %>%
  as.data.frame()%>%
  mutate(gender =
           ifelse(gender == 2,
                  "Female", "Male")) %>%
  pivot_wider(names_from = "gender") %>%
  move_columns(Male,.after = Female)

markers_aspiration_refuge <- combination8 %>%
  select(refuge,
         get_work,
         workplaces_val,
         work_rewards,
         workplace_equit,
         weights) %>%
  group_by(refuge) %>%
  dplyr::summarize(across(
    get_work:workplace_equit,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>% 
  dplyr::filter(refuge==2)%>%
  pivot_longer(
    cols = c(
      "get_work" ,
      "workplaces_val",
      "work_rewards",
      "workplace_equit"
    )
  ) %>%
  as.data.frame()%>%
  select(-refuge,`IDP/ Refugee`=value)


markers_aspiration_pwd <- combination8 %>%
  select(pwd,
         get_work,
         workplaces_val,
         work_rewards,
         workplace_equit,
         weights) %>%
  group_by(pwd) %>%
  dplyr::summarize(across(
    get_work:workplace_equit,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  dplyr::filter(pwd==1)%>%
  pivot_longer(
    cols = c(
      "get_work" ,
      "workplaces_val",
      "work_rewards",
      "workplace_equit"
    )
  ) %>%
  as.data.frame()%>%
  select(-pwd,PWD=value)


markers_aspiration_geo <- combination8 %>%
  select(geo_entity,
         get_work,
         workplaces_val,
         work_rewards,
         workplace_equit,
         weights) %>%
  group_by(geo_entity) %>%
  dplyr::summarize(across(
    get_work:workplace_equit,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(
      "get_work" ,
      "workplaces_val",
      "work_rewards",
      "workplace_equit"
    )
  ) %>%
  as.data.frame()%>%
  mutate(geo_entity =
           ifelse(geo_entity == 2,
                  "Rural", "Urban")) %>%
  pivot_wider(names_from = "geo_entity") %>%
  move_columns(Urban,.after = Rural)

markers_aspiration_age <- combination8 %>%
  select(age_group,
         get_work,
         workplaces_val,
         work_rewards,
         workplace_equit,
         weights) %>%
  group_by(age_group) %>%
  dplyr::summarize(across(
    get_work:workplace_equit,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(
      "get_work" ,
      "workplaces_val",
      "work_rewards",
      "workplace_equit"
    )
  ) %>%
  as.data.frame()%>%
  pivot_wider(names_from = "age_group")

markers_aspiration_stratum <- combination8 %>%
  select(stratum,
         get_work,
         workplaces_val,
         work_rewards,
         workplace_equit,
         weights) %>%
  group_by(stratum) %>%
  dplyr::summarize(across(
    get_work:workplace_equit,
    ~ weighted.mean(.x, weights, na.rm = TRUE)
  )) %>%
  pivot_longer(
    cols = c(
      "get_work" ,
      "workplaces_val",
      "work_rewards",
      "workplace_equit"
    )
  ) %>%
  as.data.frame()%>%
  mutate(
    stratum =
      case_when(
        stratum == 1 ~ "Wage employed",
        stratum == 2 ~ "Self employed",
        stratum == 3 ~ "Students",
        stratum == 4 ~ "Unemployed/Non job-seekers"
      )
  ) %>%
  pivot_wider(names_from = "stratum") %>%
  move_columns(`Wage employed`,.after = `Self employed`) %>%
  move_columns(`Unemployed/Non job-seekers`,.before = Students)

combine_aspiration <- markers_aspiration_gender%>%
  left_join(markers_aspiration_refuge)%>%
  left_join(markers_aspiration_pwd)%>%
  left_join(markers_aspiration_geo)%>%
  left_join(markers_aspiration_age)%>%
  left_join(markers_aspiration_stratum)

#write.xlsx(combine_aspiration,"data/markers_aspirations.xlsx")

