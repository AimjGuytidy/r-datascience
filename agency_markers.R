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
#Access to employment by sectors ------------------------
combination4<-combination%>%
  select(sector_choices_name_1,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
  rowwise()%>%
  mutate(access=mean(employing_youth_1:opp_lead_position_1, na.rm=TRUE), 
         access1=round(mean((ease_access_digni_1:youth_advancement_1), na.rm=TRUE)),
         access2=case_when(access1%in%c(4,5)~1, access1%in%c(1,2,3)~0))%>%
  group_by(sector_choices_name_1)%>%
  dplyr::summarise(average=weighted.mean(access, weights))%>%
  pivot_wider(names_from=sector_choices_name_1, values_from = average)
write.xlsx(combination4, 'access_employment_TA.xlsx',overwrite = TRUE)

combination5<-combination%>%
  select(sector_choices_name_1,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
  rowwise()%>%
  mutate(access=mean(employing_youth_1:opp_lead_position_1, na.rm=TRUE), 
         access1=round(mean((ease_access_digni_1:youth_advancement_1), na.rm=TRUE)),
         access2=case_when(access1%in%c(4,5)~1, access1%in%c(1,2,3)~0))%>%
  group_by(sector_choices_name_1,access2)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(perc=total/sum(total))%>%
  filter(access2!=0)%>%
  select(-c(total))%>%
  pivot_wider(names_from=sector_choices_name_1, values_from = perc)
write.xlsx(combination5, 'access_employment_TA_perc.xlsx',overwrite = TRUE)

combination9<-combination%>%
  select(sector_choices_name_1,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
  group_by(sector_choices_name_1)%>%
  dplyr::summarise(across(ease_access_digni_1:youth_advancement_1, ~weighted.mean(.x, weights, na.rm=TRUE)))
write.xlsx(combination9, 'access_employment_TA_markers.xlsx',overwrite = TRUE)

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
write.xlsx(markers_participation,'markers_particip.xlsx', overwrite=TRUE)
#psych::alpha(combination3[,7:12])

growth_average<-combination5%>%
  group_by()%>%
  dplyr::summarize(average=weighted.mean(growth, weights))

growth_gender<-characterize(combination5)%>%
  group_by(gender)%>%
  dplyr::summarize(average=weighted.mean(growth, weights))%>%
  pivot_wider(names_from =gender,  values_from = average)%>%
  as.data.frame()%>%
  move_columns(`b. Female`, .before=`a. Male`)

growth_refugee<-characterize(combination5)%>%
  group_by(refuge)%>%
  dplyr::summarize(average=weighted.mean(growth, weights))%>%
  pivot_wider(names_from =refuge,  values_from = average)%>%
  select(-c(`a. Non refuge`))%>%
  as.data.frame()

growth_pwd<-characterize(combination5)%>%
  group_by(pwd)%>%
  dplyr::summarize(average=weighted.mean(growth, weights))%>%
  pivot_wider(names_from =pwd,  values_from = average)%>%
  select(-c(No))%>%
  as.data.frame()
growth_geo<-characterize(combination5)%>%
  group_by(geo_entity)%>%
  dplyr::summarize(average=weighted.mean(growth, weights))%>%
  pivot_wider(names_from =geo_entity,  values_from = average)%>%
  as.data.frame()
growth_age<-characterize(combination5)%>%
  group_by(age_group)%>%
  dplyr::summarize(average=weighted.mean(growth, weights))%>%
  pivot_wider(names_from =age_group,  values_from = average)%>%
  as.data.frame()
growth_stratum<-characterize(combination5)%>%
  group_by(stratum)%>%
  dplyr::summarize(average=weighted.mean(growth, weights))%>%
  pivot_wider(names_from =stratum,  values_from = average)%>%
  as.data.frame()

growth_final<-growth_average%>%
  cbind(growth_gender)%>%
  cbind(growth_refugee)%>% 
  cbind(growth_pwd)%>%
  cbind(growth_geo)%>%
  cbind(growth_age)%>%
  cbind(growth_stratum)

write.xlsx(growth_final, 'growth_final.xlsx',overwrite=TRUE)

#percentages  
growth_perc_average<-combination5%>%
  group_by(growth2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))

growth_perc_gender<-characterize(combination5)%>%
  group_by(gender,growth2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =gender,  values_from =perc)%>%
  as.data.frame()%>%
  move_columns(`b. Female`, .before=`a. Male`)

growth_perc_refugee<-characterize(combination5)%>%
  group_by(refuge,growth2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =refuge,  values_from =perc)%>%
  select(-c(`a. Non refuge`))%>%
  as.data.frame()

growth_perc_pwd<-characterize(combination5)%>%
  group_by(pwd,growth2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =pwd,  values_from =perc)%>%
  select(-c(No))%>%
  as.data.frame()
growth_perc_geo<-characterize(combination5)%>%
  group_by(geo_entity,growth2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =geo_entity,  values_from =perc)%>%
  as.data.frame()
growth_perc_age<-characterize(combination5)%>%
  group_by(age_group,growth2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =age_group,  values_from =perc)%>%
  as.data.frame()
growth_perc_stratum<-characterize(combination5)%>%
  group_by(stratum,growth2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =stratum,  values_from =perc)%>%
  as.data.frame()

growth_perc_final<-growth_perc_average%>%
  left_join(growth_perc_gender)%>%
  left_join(growth_perc_refugee)%>% 
  left_join(growth_perc_pwd)%>%
  left_join(growth_perc_geo)%>%
  left_join(growth_perc_age)%>%
  left_join(growth_perc_stratum)

write.xlsx(growth_perc_final, 'growth_perc_final.xlsx',overwrite=TRUE)
#participation by sectors ------------------------
combination6<-combination%>%
  select(sector_choices_name_1, youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, youth_gender_equity_1,
         youth_improv_work_1, weights)%>%
  rowwise()%>%
  mutate(growth=mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE), 
         growth1=round(mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE)),
         growth2=case_when(growth1%in%c(4,5)~1, growth1%in%c(1,2,3)~0))%>%
  group_by(sector_choices_name_1)%>%
  dplyr::summarise(average=weighted.mean(growth, weights))%>%
  pivot_wider(names_from=sector_choices_name_1, values_from = average)

combination7<-combination%>%
  select(sector_choices_name_1,youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, youth_gender_equity_1,
         youth_improv_work_1, weights)%>%
  rowwise()%>%
  mutate(growth=mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE), 
         growth1=round(mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE)),
         growth2=case_when(growth1%in%c(4,5)~1, growth1%in%c(1,2,3)~0))%>%
  group_by(sector_choices_name_1,growth2)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(perc=total/sum(total))%>%
  filter(growth2!=0)%>%
  select(-c(total))%>%
  pivot_wider(names_from=sector_choices_name_1, values_from = perc)%>%
  select(-c(growth2))
final<-rbind(combination6,combination7)
write.xlsx(final, 'growth_employment_TA.xlsx',overwrite = TRUE)

combination10<-combination%>%
  select(sector_choices_name_1, youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, youth_gender_equity_1,
         youth_improv_work_1, weights)%>%
  group_by(sector_choices_name_1)%>%
  dplyr::summarise(across(youth_contrib_econ_1:youth_improv_work_1, ~weighted.mean(.x, weights, na.rm=TRUE)))
write.xlsx(combination10, 'participation_TA_markers.xlsx',overwrite = TRUE)

#L5.1.2a this is not a sector specific analysis-----------------
combination4<-mcf_data%>%
  select(gender, geo_entity, refuge, pwd, age_group, stratum, get_work,workplaces_val,
         work_rewards,workplace_equit, weights)%>%
  filter(!is.na(get_work))%>%
  rowwise()%>%
  mutate(aspirations=mean(get_work:workplaces_val, na.rm=TRUE), 
         aspirations1=round(mean(get_work:workplace_equit, na.rm=TRUE)),
         aspirations2=case_when(aspirations1%in%c(4,5)~1, aspirations1%in%c(1,2,3)~0))

aspirations_average<-combination4%>%
  group_by()%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights))

aspirations_gender<-characterize(combination4)%>%
  group_by(gender)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights))%>%
  pivot_wider(names_from =gender,  values_from = average)%>%
  as.data.frame()%>%
  move_columns(`b. Female`, .before=`a. Male`)

aspirations_refugee<-characterize(combination4)%>%
  group_by(refuge)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights))%>%
  pivot_wider(names_from =refuge,  values_from = average)%>%
  select(-c(`a. Non refuge`))%>%
  as.data.frame()

aspirations_pwd<-characterize(combination4)%>%
  group_by(pwd)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights))%>%
  pivot_wider(names_from =pwd,  values_from = average)%>%
  select(-c(No))%>%
  as.data.frame()
aspirations_geo<-characterize(combination4)%>%
  group_by(geo_entity)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights))%>%
  pivot_wider(names_from =geo_entity,  values_from = average)%>%
  as.data.frame()
aspirations_age<-characterize(combination4)%>%
  group_by(age_group)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights))%>%
  pivot_wider(names_from =age_group,  values_from = average)%>%
  as.data.frame()
aspirations_stratum<-characterize(combination4)%>%
  group_by(stratum)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights))%>%
  pivot_wider(names_from =stratum,  values_from = average)%>%
  as.data.frame()

aspirations_final<-aspirations_average%>%
  cbind(aspirations_gender)%>%
  cbind(aspirations_refugee)%>% 
  cbind(aspirations_pwd)%>%
  cbind(aspirations_geo)%>%
  cbind(aspirations_age)%>%
  cbind(aspirations_stratum)

write.xlsx(aspirations_final, 'aspirations_final.xlsx',overwrite=TRUE)

#percentages  
aspirations_perc_average<-combination4%>%
  group_by(aspirations2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))

aspirations_perc_gender<-characterize(combination4)%>%
  group_by(gender,aspirations2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =gender,  values_from =perc)%>%
  as.data.frame()%>%
  move_columns(`b. Female`, .before=`a. Male`)

aspirations_perc_refugee<-characterize(combination4)%>%
  group_by(refuge,aspirations2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =refuge,  values_from =perc)%>%
  select(-c(`a. Non refuge`))%>%
  as.data.frame()

aspirations_perc_pwd<-characterize(combination4)%>%
  group_by(pwd,aspirations2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =pwd,  values_from =perc)%>%
  select(-c(No))%>%
  as.data.frame()
aspirations_perc_geo<-characterize(combination4)%>%
  group_by(geo_entity,aspirations2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =geo_entity,  values_from =perc)%>%
  as.data.frame()
aspirations_perc_age<-characterize(combination4)%>%
  group_by(age_group,aspirations2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =age_group,  values_from =perc)%>%
  as.data.frame()
aspirations_perc_stratum<-characterize(combination4)%>%
  group_by(stratum,aspirations2)%>%
  dplyr::summarize(total=sum(weights, na.rm=TRUE))%>%
  mutate(perc=total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from =stratum,  values_from =perc)%>%
  as.data.frame()

aspirations_perc_final<-aspirations_perc_average%>%
  left_join(aspirations_perc_gender)%>%
  left_join(aspirations_perc_refugee)%>% 
  left_join(aspirations_perc_pwd)%>%
  left_join(aspirations_perc_geo)%>%
  left_join(aspirations_perc_age)%>%
  left_join(aspirations_perc_stratum)

write.xlsx(aspirations_perc_final, 'aspirations_perc_final.xlsx',overwrite=TRUE)


combination8<-mcf_data%>%
  select(get_work,workplaces_val,
         work_rewards,workplace_equit, weights)%>%
  filter(!is.na(get_work))%>%
  group_by()%>%
  dplyr::summarise(across(get_work:workplace_equit,~weighted.mean(.x,weights,na.rm=TRUE)))
write.xlsx(combination8,'aspirations_markers.xlsx')
