library(dplyr)
library(tidyr)
library(readr)
library(haven)
library(labelled)
library(rio)
library(psych)
library(writexl)

data_directory <- "G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/"


data_path <- "4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/mcf_data_brkdwn2.dta"

mcf_data <-
  haven::read_dta(paste0(
    data_directory,
    data_path
  ))
mcf_data <- mcf_data %>%
  dplyr::mutate(work_trainings_t = ifelse(work_trainings == 1, 5,
                                          ifelse(
                                            work_trainings == 2, 4,
                                            ifelse(work_trainings == 3, 3,
                                                   ifelse(
                                                     work_trainings == 4, 2,
                                                     ifelse(work_trainings ==
                                                              5, 1, NA)
                                                   ))
                                          )))
mcf_data <- mcf_data %>%
  dplyr::mutate(training_jb_market_t = ifelse(
    training_jb_market == 1,
    5,
    ifelse(
      training_jb_market == 2,
      4,
      ifelse(
        training_jb_market == 3,
        3,
        ifelse(training_jb_market ==
                 4, 2,
               ifelse(training_jb_market ==
                        5, 1, NA))
      )
    )
  ))



mcf_data <- dplyr::distinct(mcf_data) %>%
  as.data.frame() %>%
  dplyr::mutate(ability_score = rowMeans(dplyr::select(
    ., c("work_trainings_t", "training_jb_market_t")
  ), na.rm = TRUE))

#L5.1.2c this is not a sector specific analysis-----------------
keyword_label_c <- c("J1.",	"J2.",	"J3.",	"J4.")
variables_for_l512_c <- mcf_data %>% look_for(keyword_label_c)
variables_for_l512_c <- variables_for_l512_c[, "variable"]
var_df_c <- as.data.frame(variables_for_l512_c)
var_df_c <- var_df_c[1:4, ]
#filtering out unemployed and students
mcf_data <- mcf_data %>%
  #dplyr::filter(stratum==1|stratum==2)%>%
  dplyr::mutate(
    expectation_score = rowMeans(dplyr::select(., all_of(var_df_c)), na.rm = TRUE),
    exp_score_prop = ifelse(round(expectation_score) >= 4, 1, 0)
  )

#------------------------------------------------------------------------------

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

#L5.1.1 a----
#Create four different data sets 
data1<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,DFWIA1_new,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_1:youth_improv_work_1)) #add weights column 
data2<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,DFWIA1_new,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_2:youth_improv_work_2))
data3<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,DFWIA1_new,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_3:youth_improv_work_3))
data4<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,DFWIA1_new,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_4:youth_improv_work_4))

#Match columns name for each dataset 
colnames(data4)<-colnames(data1)
colnames(data3)<-colnames(data1)
colnames(data2)<-colnames(data1)

combination1<-rbind(data1,data2,data3,data4)

combination<-as.data.frame(combination1)%>% #should have weights 
  filter(!is.na(opp_start_business_1))%>%
  filter(sector_choices_name_1!='')%>%
  select(-c(agreeing_1))%>%
  set_value_labels(ease_access_digni_1=c(`e. Very easy`=5,  `d. Easy`=4, `c. Difficult`=3,  `b. Very difficult`=2, `a. Haven't worked in this sector`=1))
combination$sector_choices_name_1[combination$sector_choices_name_1=='Creative industries']<-'Tourism&Hospitality'

#Overall access to employment ----
combination2<-combination%>%
  select(uniqueid,gender, main_activity,DFWIA1_new,district_calc,geo_entity, refuge, pwd, age_group, stratum,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
  rowwise()%>%
  mutate(access=mean(employing_youth_1:opp_lead_position_1, na.rm=TRUE), 
         access1=round(mean((ease_access_digni_1:youth_advancement_1), na.rm=TRUE)),
         access2=case_when(access1%in%c(4,5)~1, access1%in%c(1,2,3)~0))



#L.5.1.1b - new ----------------------------
#Overall access to employment ---
combination5<-combination%>%
  select(uniqueid, district_calc,main_activity,DFWIA1_new,gender, geo_entity, refuge, pwd, age_group, stratum,geo_entity, youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, youth_gender_equity_1,youth_improv_work_1, weights)%>%
  rowwise()%>%
  mutate(growth=mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE), 
         growth1=round(mean((youth_contrib_econ_1:youth_improv_work_1), na.rm=TRUE)),
         growth2=case_when(growth1%in%c(4,5)~1, growth1%in%c(1,2,3)~0))

#L5.1.2a this is not a sector specific analysis-----------------
combination4<-mcf_data%>%
  select(gender, district_calc,main_activity,DFWIA1_new,geo_entity, refuge, pwd, age_group, stratum, get_work,workplaces_val,
         work_rewards,workplace_equit, weights)%>%
  filter(!is.na(get_work))%>%
  rowwise()%>%
  mutate(aspirations=mean(get_work:workplaces_val, na.rm=TRUE), 
         aspirations1=round(mean(get_work:workplace_equit, na.rm=TRUE)),
         aspirations2=case_when(aspirations1%in%c(4,5)~1, aspirations1%in%c(1,2,3)~0))

#------------------------------------------------------------------------------

growth_DF<-combination5%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(growth, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Growth`=average)


growth_DF_employed<-combination5%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(growth, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Growth`=average)


growth_DF_combined <- rbind(growth_DF,growth_DF_employed)%>%
  as_tibble()%>%
  distinct()
#write_xlsx(growth_DF_combined,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Growth_DF_overall.xlsx")
#------------------------------------------------------------------------------

aspirations_DF<-combination4%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Aspirations access`=average)

aspirations_DF_employed<-combination4%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Aspirations access`=average)

aspirations_DF_combined <- rbind(aspirations_DF,aspirations_DF_employed)%>%
  as_tibble()%>%
  distinct()
# write_xlsx(aspirations_DF_combined,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Aspiration_DF_overall.xlsx")
#------------------------------------------------------------------------------

access_DF<-combination2%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(access, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Access to employment`=average)

access_DF_employed<-combination2%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(access, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Access to employment`=average)

access_DF_combined <- rbind(access_DF,access_DF_employed)%>%
  as_tibble()%>%
  distinct()
# write_xlsx(access_DF_combined,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Access_DF_overall.xlsx")
#-------------------------------------------------------------------------------
ability_DF<-mcf_data%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(ability_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Ability score`=average)

ability_DF_employed<-mcf_data%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(ability_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Ability score`=average)

ability_DF_combined <- rbind(ability_DF,ability_DF_employed)%>%
  as_tibble()%>%
  distinct()
# write_xlsx(ability_DF_combined,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Ability_DF_overall.xlsx")

#-------------------------------------------------------------------------------------

expectation_DF<-mcf_data%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(expectation_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`expectation score`=average)

expectation_DF_employed<-mcf_data%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new)%>%
  dplyr::summarize(average=weighted.mean(expectation_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`expectation score`=average)

expectation_DF_combined <- rbind(expectation_DF,expectation_DF_employed)%>%
  as_tibble()%>%
  distinct()
#write_xlsx(expectation_DF_combined,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Expectation_DF_overall.xlsx")

#combine the Agency and voice sub indicators 

combined_voa <- access_DF_combined %>%
  left_join(growth_DF_combined) %>%
  left_join(aspirations_DF_combined) %>%
  left_join(ability_DF_combined) %>%
  left_join(expectation_DF_combined) %>%
  mutate(`Disagregation class`="Overall") %>%
  relocate(`Disagregation class`,.after = `Dignified and Fulfilling Work`)

# Disaggregate by gender

growth_DF_gender<-characterize(combination5)%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(growth, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Growth`=average)


growth_DF_employed_gender<-characterize(combination5)%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(growth, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Growth`=average)


growth_DF_combined_gender <- rbind(growth_DF_gender,growth_DF_employed_gender)%>%
  as_tibble()%>%
  distinct()
#write_xlsx(growth_DF_combined,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Growth_DF_overall.xlsx")
#------------------------------------------------------------------------------

aspirations_DF_gender<-characterize(combination4)%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Aspirations access`=average)

aspirations_DF_employed_gender<-characterize(combination4)%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Aspirations access`=average)

aspirations_DF_combined_gender <- rbind(aspirations_DF_gender,aspirations_DF_employed_gender)%>%
  as_tibble()%>%
  distinct()
# write_xlsx(aspirations_DF_combined,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Aspiration_DF_overall.xlsx")
#------------------------------------------------------------------------------

access_DF_gender<-characterize(combination2)%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(access, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Access to employment`=average)

access_DF_employed_gender<-characterize(combination2)%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(access, weights)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Access to employment`=average)

access_DF_combined_gender <- rbind(access_DF_gender,access_DF_employed_gender)%>%
  as_tibble()%>%
  distinct()
# write_xlsx(access_DF_combined,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Access_DF_overall.xlsx")
#-------------------------------------------------------------------------------
ability_DF_gender<-characterize(mcf_data)%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(ability_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Ability score`=average)

ability_DF_employed_gender<-characterize(mcf_data)%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(ability_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`Ability score`=average)

ability_DF_combined_gender <- rbind(ability_DF_gender,ability_DF_employed_gender)%>%
  as_tibble()%>%
  distinct()
# write_xlsx(ability_DF_combined,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Ability_DF_overall.xlsx")

#-------------------------------------------------------------------------------------

expectation_DF_gender<-characterize(mcf_data)%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(expectation_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","not D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`expectation score`=average)

expectation_DF_employed_gender<-characterize(mcf_data)%>%
  filter(stratum=="Self-employed"|stratum=="Wage-employed")%>%
  group_by(DFWIA1_new,gender)%>%
  dplyr::summarize(average=weighted.mean(expectation_score, weights,na.rm=TRUE)) %>%
  mutate(DFWIA1_new=ifelse(DFWIA1_new==1,"Youth in D&F work","working youth not in D&F"))%>%
  rename(`Dignified and Fulfilling Work` = DFWIA1_new,`expectation score`=average)

expectation_DF_combined_gender <- rbind(expectation_DF_gender,expectation_DF_employed_gender)%>%
  as_tibble()%>%
  distinct()

combined_voa_gender <- access_DF_combined_gender %>%
  left_join(growth_DF_combined_gender) %>%
  left_join(aspirations_DF_combined_gender) %>%
  left_join(ability_DF_combined_gender) %>%
  left_join(expectation_DF_combined_gender) %>%
  rename(`Disagregation class`=gender)

combined_voa_final <- rbind(combined_voa,combined_voa_gender)

# write_xlsx(combined_voa_final,"G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/Voice_Agency_DF_overall_gender.xlsx")



