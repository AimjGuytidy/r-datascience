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
  rename(youth_country_lead=youth_contrib_innov, youth_contrib_innov=youth_improv_envir,
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

variables<-(var_label(mcf_data)) # this will create a list of variables and their labels
`%notin%` <- Negate(`%in%`)

#L4.3.1.a ------
min_wage<-80950.57143
#Analysis A
#Step 1 and 2
mcf_data<-mcf_data%>%
  mutate(month_inco_total=as.numeric(inco_total)/12,
         categ_inco_total=case_when(month_inco_total>=(min_wage+(min_wage*0.1))~'Above', 
                                    month_inco_total<(min_wage+(min_wage*0.1))~'Below'))

#Analysis B
#Those who "Strongly agree" and "Agree" have been assigned "stro agree" and "Not" for other choices and NA to be still there
mcf_data<-mcf_data%>%
  mutate(marker1=case_when(indi_need%in%c(5,4)&fami_need%in%c(5,4)~1, 
                           indi_need%in%c(1,2,3)|fami_need%in%c(1,2,3)~0),
         marker2=case_when(resp_workplace%in%c(5,4)~1,
                           resp_workplace%in%c(1,2,3)~0),
         marker3=case_when(resp_carabout%in%c(5,4)~1,
                           resp_carabout%in%c(1,2,3)~0),
         marker4=case_when(sense_purp%in%c(5,4)~1,
                           sense_purp%in%c(1,2,3)~0),
         mkr1n2=case_when(marker1==1&marker2==1~1, TRUE~0),
         DFWIA1=case_when(categ_inco_total=='Above'&(marker1==1|
                                                       marker2==1|
                                                       marker3==1|
                                                       marker4==1)~1, TRUE~0))%>%
  mutate(markers_nber= rowSums(.[ ,c('marker1', 'marker2', 'marker3', 'marker4')]),
         DFW2=case_when(markers_nber>=2~1,
                        markers_nber<2~0),)


#L5.1.1 a----
#Create four different data sets 
data1<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,DFWIA1,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_1:youth_improv_work_1)) #add weights column 
data2<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,DFWIA1,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_2:youth_improv_work_2))
data3<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,DFWIA1,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_3:youth_improv_work_3))
data4<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,DFWIA1,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_4:youth_improv_work_4))

#Match columns name for each dataset 
colnames(data4)<-colnames(data1)
colnames(data3)<-colnames(data1)
colnames(data2)<-colnames(data1)

combination1<-bind_rows(data1,data2,data3,data4)

combination<-as.data.frame(combination1)%>% #should have weights 
  filter(!is.na(opp_start_business_1))%>%
  filter(sector_choices_name_1!='')%>%
  select(-c(agreeing_1))%>%
  set_value_labels(ease_access_digni_1=c(`e. Very easy`=5,  `d. Easy`=4, `c. Difficult`=3,  `b. Very difficult`=2, `a. Haven't worked in this sector`=1))
combination$sector_choices_name_1[combination$sector_choices_name_1=='Creative industries']<-'Tourism&Hospitality'

#Overall access to employment ----
combination2<-combination%>%
  select(uniqueid,gender, main_activity,DFWIA1,district_calc,geo_entity, refuge, pwd, age_group, stratum,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
  # group_by(uniqueid,gender, geo_entity, refuge, pwd, age_group, stratum)%>%
  # dplyr::summarise(across(ease_access_digni_1:weights,~mean(.x, na.rm=TRUE)))%>%
  rowwise()%>%
  mutate(access=mean(employing_youth_1:opp_lead_position_1, na.rm=TRUE), 
         access1=round(mean((ease_access_digni_1:youth_advancement_1), na.rm=TRUE)),
         access2=case_when(access1%in%c(4,5)~1, access1%in%c(1,2,3)~0))


#-------------------------------------------------------------------------
#disaggregation by district
access_district<-combination2%>%
  group_by(district_calc)%>%
  dplyr::summarize(average=weighted.mean(access, weights))%>%
  rename(District = district_calc,`Access to  Employment`=average)

access_district_main <- characterize(combination2)%>%
  group_by(district_calc,main_activity)%>%
  dplyr::summarize(average=weighted.mean(access, weights)) %>%
  pivot_wider(names_from = "main_activity",values_from = "average")
#--------------------------------------------------------------------------



#L.5.1.1b - new ----------------------------
#Overall access to employment ---
combination5<-combination%>%
  select(uniqueid, district_calc,main_activity,DFWIA1,gender, geo_entity, refuge, pwd, age_group, stratum,geo_entity, youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, youth_gender_equity_1,youth_improv_work_1, weights)%>%
  rowwise()%>%
  mutate(growth=mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE), 
         growth1=round(mean((youth_contrib_econ_1:youth_improv_work_1), na.rm=TRUE)),
         growth2=case_when(growth1%in%c(4,5)~1, growth1%in%c(1,2,3)~0))
#-----------------------------------------------------------------------------
#access and growth
access_growth_combination<-combination%>%
  select(uniqueid, district_calc,main_activity,DFWIA1,gender, geo_entity, refuge, pwd, age_group, stratum,geo_entity, youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, youth_gender_equity_1,youth_improv_work_1,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
  rowwise()%>%
  mutate(growth=mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE),
         access=mean(employing_youth_1:opp_lead_position_1, na.rm=TRUE),
         acc_growth = (growth+access)/2)
#------------------------------------------------------------
#access and growth combined disaggregation
#average ability score 
acc_growth_overall<-access_growth_combination%>%
  group_by()%>%
  dplyr::summarize(avg_acc_growth=round(weighted.mean(acc_growth, weights,
                                                         na.rm=TRUE),2))%>%
  mutate(name="Overall",value="Overall")
#Disaggregatie by district
acc_growth_district<-access_growth_combination%>%
  group_by(district_calc)%>%
  dplyr::summarize(avg_acc_growth=round(weighted.mean(acc_growth, weights,
                                                         na.rm=TRUE),2))
#Disaggregation by gender 
acc_growth_gender<-characterize(access_growth_combination)%>%
  group_by(gender)%>%
  dplyr::summarize(avg_acc_growth=round(weighted.mean(acc_growth, weights,na.rm=TRUE),2))%>%
  pivot_longer(gender,names_to = "name",values_to = "value")
acc_growth_gender$value<-gsub("^[a-zA-Z0-9]\\.\\s","",acc_growth_gender$value)
#Disaggregation by geoentity 
acc_growth_geoentity<-characterize(access_growth_combination)%>%
  group_by(geo_entity)%>%
  dplyr::summarize(avg_acc_growth=round(weighted.mean(acc_growth, weights,na.rm=TRUE),2))%>%
  pivot_longer(geo_entity,names_to = "name",values_to = "value")
#Disaggregation by age group 
acc_growth_agegroup<-characterize(access_growth_combination)%>%
  group_by(age_group)%>%
  dplyr::summarize(avg_acc_growth=round(weighted.mean(acc_growth, weights,na.rm=TRUE),2))%>%
  pivot_longer(age_group,names_to = "name",values_to = "value")
#Disaggregation by pwd
acc_growth_pwd<-characterize(access_growth_combination)%>%
  group_by(pwd)%>%
  dplyr::summarize(avg_acc_growth=round(weighted.mean(acc_growth, weights
                                                         ,na.rm=TRUE),2))%>%
  filter(pwd=="Yes")%>%
  mutate(pwd= ifelse(pwd=="Yes","PWD",NA))%>%
  pivot_longer(pwd,names_to = "name",values_to = "value")
#Disaggregation by refuge 
acc_growth_refuge<-characterize(access_growth_combination)%>%
  group_by(refuge)%>%
  dplyr::summarize(avg_acc_growth=round(weighted.mean(acc_growth, weights,
                                                         na.rm=TRUE),2))%>%
  filter(refuge=="b. Refugee")%>%
  mutate(refuge= ifelse(refuge=="b. Refugee","IDP/ Refugee",NA))%>%
  pivot_longer(refuge,names_to = "name",values_to = "value")

#Disaggregation by stratum
acc_growth_stratum<-characterize(access_growth_combination)%>%
  group_by(stratum)%>%
  dplyr::summarize(avg_acc_growth=round(weighted.mean(acc_growth, weights,na.rm=TRUE),2))%>%
  pivot_longer(stratum,names_to = "name",values_to = "value")


x <- c("Overall" ,"Female","Male" ,"IDP/ Refugee","PWD","Rural" ,"Urban" 
       ,"18-24" ,"25-35" ,"Self employed","Wage employed" ," Unemployed/Non job-seekers","Students")

df_acc_growth_demo <-rbind(acc_growth_gender,acc_growth_geoentity,acc_growth_agegroup,
                        acc_growth_stratum,acc_growth_overall,acc_growth_refuge,acc_growth_pwd)%>%
  slice(match(x,value))%>%
  select(-name)%>%
  select(`Disaggregation categories`=value,`Average Access and growth combined score`=avg_acc_growth)

#write.xlsx(df_acc_growth_demo,"data/df_acc_growth_demo.xlsx")


#---------------------------------------------------------------------------
#disaggregation by district
growth_district<-combination5%>%
  group_by(district_calc)%>%
  dplyr::summarize(average=weighted.mean(growth, weights))%>%
  rename(District = district_calc,`Growth access`=average)

growth_district_main <- characterize(combination5)%>%
  group_by(district_calc,main_activity)%>%
  dplyr::summarize(average=weighted.mean(growth, weights)) %>%
  pivot_wider(names_from = "main_activity",values_from = "average")

#-----------------------------------------------------------------------------

#L5.1.2a this is not a sector specific analysis-----------------
combination4<-mcf_data%>%
  select(gender, district_calc,main_activity,DFWIA1,geo_entity, refuge, pwd, age_group, stratum, get_work,workplaces_val,
         work_rewards,workplace_equit, weights)%>%
  filter(!is.na(get_work))%>%
  rowwise()%>%
  mutate(aspirations=mean(get_work:workplaces_val, na.rm=TRUE), 
         aspirations1=round(mean(get_work:workplace_equit, na.rm=TRUE)),
         aspirations2=case_when(aspirations1%in%c(4,5)~1, aspirations1%in%c(1,2,3)~0))


#---------------------------------------------------------------------------
#disaggregation by district
aspirations_district<-combination4%>%
  group_by(district_calc)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  rename(District = district_calc,`Aspirations access`=average)

aspirations_district_main <- characterize(combination4)%>%
  group_by(district_calc,main_activity)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  pivot_wider(names_from = "main_activity",values_from = "average")

#-----------------------------------------------------------------------------
combined_ddistrict <- access_district %>%
  left_join(growth_district)%>%
  left_join(aspirations_district)

combo <- read.csv2("data/combined_district.csv")

altogether <- combined_ddistrict %>%
  left_join(combo)
#-----------------------------------------------------------------------------
#write.xlsx(altogether,"data/combination_district_total.xlsx")
mcf_data_l5_t <- mcf_data

keyword_label <-
  c("D14",
    "D16",
    "D18",
    "D20",
    "D22",
    "D24",
    "D26",
    "D28",
    "D30",
    "D32",
    "D34",
    "D36")
variables_for_l531_a <- mcf_data_l5_t %>% look_for(keyword_label)
variables_for_l531_a <- variables_for_l531_a[, "variable"]
var_df <- as.data.frame(variables_for_l531_a)

for (i in 1:length(keyword_label)) {
  mcf_data_l5_t[, var_df[i, ]][mcf_data_l5_t[, var_df[i, ]] == 1] <- 0
  mcf_data_l5_t[, var_df[i, ]][mcf_data_l5_t[, var_df[i, ]] == 2] <- 1
  mcf_data_l5_t[, var_df[i, ]][mcf_data_l5_t[, var_df[i, ]] == 3] <- 2
  mcf_data_l5_t[, var_df[i, ]][mcf_data_l5_t[, var_df[i, ]] == 4] <- 3
  mcf_data_l5_t[, var_df[i, ]][mcf_data_l5_t[, var_df[i, ]] == 5] <- 4
}

# change the values from 1-3 to 0-2
keyword_label_b <-
  c("D15",
    "D17",
    "D19",
    "D21",
    "D23",
    "D25",
    "D27",
    "D29",
    "D31",
    "D33",
    "D35",
    "D37")
variables_for_l531_b <- mcf_data_l5_t %>% look_for(keyword_label_b)
variables_for_l531_b <- variables_for_l531_b[, "variable"]
var_df_b <- as.data.frame(variables_for_l531_b)

for (i in 1:length(keyword_label_b)) {
  mcf_data_l5_t[, var_df_b[i, ]][mcf_data_l5_t[, var_df_b[i, ]] == 1] <-
    0
  mcf_data_l5_t[, var_df_b[i, ]][mcf_data_l5_t[, var_df_b[i, ]] == 2] <-
    1
  mcf_data_l5_t[, var_df_b[i, ]][mcf_data_l5_t[, var_df_b[i, ]] == 3] <-
    2
}

# step 1: summing values from variables covering subquestion a and indicator l5.3.1
#var_df_filter <- grep("_access$", var_df$variable,value=TRUE, ignore.case =T)
mcf_data_l5_t <- mcf_data_l5_t %>%
  mutate(sum_quality_life = rowSums(select(
    .,
    grep(
      "_access$",
      var_df$variable,
      value = TRUE,
      ignore.case = T
    )
  ), na.rm = TRUE))

# step 2: averaging services improvement (subquestion b related)

mcf_data_l5_t <- mcf_data_l5_t %>%
  mutate(avg_improv_quality_life = rowMeans(select(., var_df_b$variable), na.rm = TRUE))

#step 3: computing the product of step 1 and step 2

mcf_data_l5_t <- mcf_data_l5_t %>%
  mutate(prod_quality_life = avg_improv_quality_life * sum_quality_life)

#step 4: adjusting the index to 100 from step 3

mcf_data_l5_t <- mcf_data_l5_t %>%
  mutate(perc_quality_life = (prod_quality_life * 100) / 96)

qual_life_district<-mcf_data_l5_t%>%
  group_by(district_calc)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, weights)) %>%
  rename(District = district_calc,`Quality of Life index`=average)
#write.xlsx(qual_life_district,"data/quality_life_district.xlsx")
qual_life_score_district<-mcf_data_l5_t%>%
  group_by(district_calc)%>%
  dplyr::summarize(average=weighted.mean(avg_improv_quality_life, weights)) %>%
  rename(District = district_calc,`Quality of Life improvement score`=average)

combined_qual_life_district <- qual_life_district %>%
  left_join(qual_life_score_district)
#write.xlsx(combined_qual_life_district,"data/quality_life_district.xlsx")

#-------------------------------------------------------------------------------
qual_life_DF<-mcf_data_l5_t%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Quality of Life index`=average)

qual_life_DF_employed<-mcf_data_l5_t%>%
  filter(stratum==1|stratum==2)%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Quality of Life index`=average)

qual_life_DF_combined <- rbind(qual_life_DF,qual_life_DF_employed)
#write.xlsx(qual_life_DF_combined,"data/qual_life_DF_combined.xlsx")

#------------------------------------------------------------------------------

growth_DF<-combination5%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(growth, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Growth`=average)


growth_DF_employed<-combination5%>%
  filter(stratum==1|stratum==2)%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(growth, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Growth`=average)


growth_DF_combined <- rbind(growth_DF,growth_DF_employed)
#write.xlsx(growth_DF_combined,"data/growth_DF_combined.xlsx")
#------------------------------------------------------------------------------

aspirations_DF<-combination4%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Aspirations access`=average)

aspirations_DF_employed<-combination4%>%
  filter(stratum==1|stratum==2)%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Aspirations access`=average)

aspirations_DF_combined <- rbind(aspirations_DF,aspirations_DF_employed)
#write.xlsx(aspirations_DF_combined,"data/aspirations_DF_combined.xlsx")
#------------------------------------------------------------------------------

access_DF<-combination2%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(access, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Access to employment`=average)

access_DF_employed<-combination2%>%
  filter(stratum==1|stratum==2)%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(access, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Access to employment`=average)

access_DF_combined <- rbind(access_DF,access_DF_employed)
#write.xlsx(access_DF_combined,"data/access_DF_combined.xlsx")
#------------------------------------------------------------------------------
mcf_data <- mcf_data%>%
  dplyr::mutate(work_trainings_t=ifelse(work_trainings==1,5,
                                        ifelse(work_trainings==2,4,
                                               ifelse(work_trainings==3,3,
                                                      ifelse(work_trainings==4,2,
                                                             ifelse(work_trainings==5,1,NA))))))
mcf_data <- mcf_data%>%
  dplyr::mutate(training_jb_market_t=ifelse(training_jb_market==1,5,
                                            ifelse(training_jb_market==2,4,
                                                   ifelse(training_jb_market==3,3,
                                                          ifelse(training_jb_market==4,2,
                                                                 ifelse(training_jb_market==5,1,NA))))))


mcf_data <- mcf_data%>%
  as.data.frame()%>%
  dplyr::mutate(ability_score=rowMeans(dplyr::select(.,c("work_trainings_t","training_jb_market_t")),na.rm = TRUE))
#-------------------------------------------------------------------------------------------------------------------

ability_DF<-mcf_data%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(ability_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Ability score`=average)

ability_DF_employed<-mcf_data%>%
  filter(stratum==1|stratum==2)%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(ability_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Ability score`=average)

ability_DF_combined <- rbind(ability_DF,ability_DF_employed)
#write.xlsx(ability_DF_combined,"data/ability_DF_combined.xlsx")

#---------------------------------------------------------------------------------------------------------------------
#L5.1.2c this is not a sector specific analysis-----------------
keyword_label_c<-c("J1.",	"J2.",	"J3.",	"J4.")
variables_for_l512_c <- mcf_data%>%look_for(keyword_label_c)
variables_for_l512_c <-variables_for_l512_c[,"variable"]
var_df_c <- as.data.frame(variables_for_l512_c)
var_df_c <- var_df_c[1:4,]
#filtering out unemployed and students
mcf_data <- mcf_data%>%
  #dplyr::filter(stratum==1|stratum==2)%>%
  dplyr::mutate(expectation_score = rowMeans(dplyr::select(.,all_of(var_df_c)),na.rm = TRUE),
                exp_score_prop = ifelse(round(expectation_score)>=4,1,0))

#-------------------------------------------------------------------------------------

expectation_DF<-mcf_data%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(expectation_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`expectation score`=average)

expectation_DF_employed<-mcf_data%>%
  filter(stratum==1|stratum==2)%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(expectation_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`expectation score`=average)

expectation_DF_combined <- rbind(expectation_DF,expectation_DF_employed)
#write.xlsx(expectation_DF_combined,"data/expectation_DF_combined.xlsx")

#-------------------------------------------------------------------------------
aspirations_DF_strat<-characterize(combination4)%>%
  group_by(DFWIA1,stratum)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  filter(DFWIA1!=1)%>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Aspirations access`=average)

write.xlsx(aspirations_DF_strat,"data/aspirations_DF_strat.xlsx")

expectation_DF_strat<-characterize(mcf_data)%>%
  group_by(DFWIA1,stratum)%>%
  dplyr::summarize(average=weighted.mean(expectation_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`expectation score`=average)

write.xlsx(expectation_DF_strat,"data/expectation_DF_strat.xlsx")

qual_life_DF_strat<-characterize(mcf_data_l5_t)%>%
  group_by(DFWIA1,stratum)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Quality of Life index`=average)

write.xlsx(qual_life_DF_strat,"data/qual_life_DF_strat.xlsx")



ability_DF_strat<-characterize(mcf_data)%>%
  group_by(DFWIA1,stratum)%>%
  dplyr::summarize(average=weighted.mean(ability_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Ability score`=average)

write.xlsx(ability_DF_strat,"data/ability_DF_strat.xlsx")


access_DF_strat<-characterize(combination2)%>%
  group_by(DFWIA1,stratum)%>%
  dplyr::summarize(average=weighted.mean(access, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Access to employment`=average)


write.xlsx(access_DF_strat,"data/access_DF_strat.xlsx")


growth_DF_strat<-characterize(combination5)%>%
  group_by(DFWIA1,stratum)%>%
  dplyr::summarize(average=weighted.mean(growth, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Growth`=average)


write.xlsx(growth_DF_strat,"data/growth_DF_strat.xlsx")





qual_life_DF_strat_avg<-characterize(mcf_data_l5_t)%>%
  group_by(DFWIA1,stratum)%>%
  dplyr::summarize(average=weighted.mean(avg_improv_quality_life, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Quality of Life index`=average)

write.xlsx(qual_life_DF_strat_avg,"data/qual_life_DF_strat_avg.xlsx")



qual_life_DF_avg<-characterize(mcf_data_l5_t)%>%
group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(avg_improv_quality_life, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Quality of Life index`=average)

qual_life_DF_avg_employed<-mcf_data_l5_t%>%
  filter(stratum==1|stratum==2)%>%
  group_by(DFWIA1)%>%
  dplyr::summarize(average=weighted.mean(avg_improv_quality_life, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Quality of Life index`=average)

qual_life_DF_avg_combined <- rbind(qual_life_DF_avg,qual_life_DF_avg_employed)

#write.xlsx(qual_life_DF_avg_combined,"data/qual_life_DF_avg_combined.xlsx")

#------------------------------------------------------------------------------

qual_life_DF_avg_gender<-characterize(mcf_data_l5_t)%>%
  group_by(DFWIA1,gender)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Quality of Life index`=average)

qual_life_DF_avg_employed_gender<-characterize(mcf_data_l5_t)%>%
  filter(stratum=="Wage employed"|stratum=="Self employed")%>%
  group_by(DFWIA1,gender)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, weights)) %>%
  mutate(DFWIA1=ifelse(DFWIA1==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1,`Quality of Life index`=average)

qual_life_DF_avg_combined_gender <- rbind(qual_life_DF_avg_gender,qual_life_DF_avg_employed_gender)

qual_life_DF_avg_combined_gender <- distinct(qual_life_DF_avg_combined_gender)

