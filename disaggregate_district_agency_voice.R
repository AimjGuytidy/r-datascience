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
#mcf_data1<-characterize(mcf_data)
#mcf_data1<-as.data.frame(lapply(mcf_data1, function(x) gsub('^[a-z]. *','', x)))

#mcf_data1[]<-lapply(mcf_data1, function(x) gsub('^[a-z]. *','', x))
#write_labelled_xlsx(mcf_data,'mcf_baseline.xlsx')

#a<-read.xlsx('mcf_baseline.xlsx')
variables<-(var_label(mcf_data)) # this will create a list of variables and their labels
`%notin%` <- Negate(`%in%`)

employed<-mcf_data%>%
  filter(status==1)%>%
  group_by()%>%
  dplyr::summarise(total=sum(weights))

employed_gender<-(mcf_data)%>%
  filter(status==1)%>%
  group_by(gender)%>%
  dplyr::summarise(total=sum(weights))%>%
  characterize()%>%
  pivot_wider(names_from =gender,  values_from = total)%>%
  as.data.frame()%>%
  move_columns(`b. Female`, .before=`a. Male`)

employed_refugee<-(mcf_data)%>%
  filter(status==1)%>%
  group_by(refuge)%>%
  dplyr::summarise(total=sum(weights))%>%
  characterize()%>%
  pivot_wider(names_from =refuge,  values_from = total)%>%
  dplyr::select(-c(`a. Non refuge`))%>%
  as.data.frame()

employed_pwd<-(mcf_data)%>%
  filter(status==1)%>%
  group_by(pwd)%>%
  dplyr::summarise(total=sum(weights))%>%
  characterize()%>%
  pivot_wider(names_from =pwd,  values_from = total)%>%
  dplyr::select(-c(No))%>%
  as.data.frame()
employed_geo<-(mcf_data)%>%
  filter(status==1)%>%
  group_by(geo_entity)%>%
  dplyr::summarise(total=sum(weights))%>%
  characterize()%>%
  pivot_wider(names_from =geo_entity,  values_from = total)%>%
  as.data.frame()%>%
  move_columns(Urban,.after=Rural)

employed_age<-(mcf_data)%>%
  filter(status==1)%>%
  group_by(age_group)%>%
  dplyr::summarise(total=sum(weights))%>%
  pivot_wider(names_from =age_group,  values_from = total)%>%
  as.data.frame()

employed_stratum<-(mcf_data)%>%
  filter(status==1)%>%
  group_by(stratum)%>%
  characterize()%>%
  dplyr::summarise(total=sum(weights))%>%
  pivot_wider(names_from =stratum,  values_from = total)%>%
  as.data.frame()

employed_final<-cbind(employed, employed_gender, employed_refugee,
                      employed_pwd, employed_geo, employed_age,
                      employed_stratum)
write.xlsx(employed_final,'employed.xlsx', overwrite = TRUE)

#L4.3.1.a ------
min_wage<-80950.57143
#Analysis A
#Step 1 and 2
mcf_data<-mcf_data%>%
  mutate(month_inco_total=as.numeric(inco_total)/12,
         categ_inco_total=case_when(month_inco_total>=(min_wage+(min_wage*0.1))~'Above', 
                                    month_inco_total<(min_wage+(min_wage*0.1))~'Below'))
#Overall
above_total<-characterize(mcf_data)%>%
  group_by(categ_inco_total)%>%
  dplyr::summarize(total=sum(weights))%>%
  as.data.frame()
above_gender<-as.data.frame(characterize(mcf_data))%>%
  group_by(categ_inco_total, gender)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =gender,  values_from = total)%>%
  as.data.frame()%>%
  move_columns(`b. Female`, .before=`a. Male`)

above_refugee<-characterize(mcf_data)%>%
  group_by(categ_inco_total, refuge)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =refuge,  values_from = total)%>%
  select(-c(`a. Non refuge`))%>%
  as.data.frame()
above_pwd<-characterize(mcf_data)%>%
  group_by(categ_inco_total,pwd)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =pwd,  values_from = total)%>%
  select(-c(No))%>%
  as.data.frame()
above_geo<-characterize(mcf_data)%>%
  group_by(categ_inco_total,geo_entity)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =geo_entity,  values_from = total)%>%
  as.data.frame()
above_age<-characterize(mcf_data)%>%
  group_by(categ_inco_total,age_group)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =age_group,  values_from = total)%>%
  as.data.frame()
above_stratum<-characterize(mcf_data)%>%
  group_by(categ_inco_total,stratum)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =stratum,  values_from = total)%>%
  as.data.frame()

above_final<-above_total%>%
  left_join(above_gender)%>%
  left_join(above_refugee)%>% 
  left_join(above_pwd)%>%
  left_join(above_geo)%>%
  left_join(above_age)%>%
  left_join(above_stratum)

write.xlsx(above_final, 'above_final.xlsx',overwrite=TRUE)

#ISIC
above_total<-characterize(mcf_data)%>%
  group_by(categ_inco_total,main_activity)%>%
  dplyr::summarize(total=sum(weights))%>%
  as.data.frame()
above_gender<-as.data.frame(characterize(mcf_data))%>%
  group_by(categ_inco_total,main_activity, gender)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =gender,  values_from = total)%>%
  as.data.frame()%>%
  move_columns(`b. Female`, .before=`a. Male`)

above_refugee<-characterize(mcf_data)%>%
  group_by(categ_inco_total,main_activity, refuge)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =refuge,  values_from = total)%>%
  select(-c(`a. Non refuge`))%>%
  as.data.frame()
above_pwd<-characterize(mcf_data)%>%
  group_by(categ_inco_total,main_activity,pwd)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =pwd,  values_from = total)%>%
  select(-c(No))%>%
  as.data.frame()
above_geo<-characterize(mcf_data)%>%
  group_by(categ_inco_total,main_activity,geo_entity)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =geo_entity,  values_from = total)%>%
  as.data.frame()
above_age<-characterize(mcf_data)%>%
  group_by(categ_inco_total,main_activity,age_group)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =age_group,  values_from = total)%>%
  as.data.frame()
above_stratum<-characterize(mcf_data)%>%
  group_by(categ_inco_total,main_activity,stratum)%>%
  dplyr::summarize(total=sum(weights))%>%
  pivot_wider(names_from =stratum,  values_from = total)%>%
  as.data.frame()

above_final<-above_total%>%
  left_join(above_gender)%>%
  left_join(above_refugee)%>% 
  left_join(above_pwd)%>%
  left_join(above_geo)%>%
  left_join(above_age)%>%
  left_join(above_stratum)

write.xlsx(above_final, 'above_final.xlsx',overwrite=TRUE)

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
  select(uniqueid,main_activity,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_1:youth_improv_work_1)) #add weights column 
data2<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_2:youth_improv_work_2))
data3<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_3:youth_improv_work_3))
data4<-as.data.frame(mcf_data)%>%
  select(uniqueid,main_activity,district_calc,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_4:youth_improv_work_4))

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
  select(uniqueid,gender, main_activity,district_calc,geo_entity, refuge, pwd, age_group, stratum,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
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
#write.xlsx(markers_access,'markers_access.xlsx', overwrite=TRUE)

#-------------------------------------------------------------------------
#disaggregation by district
access_district<-combination2%>%
  group_by(district_calc)%>%
  dplyr::summarize(average=weighted.mean(access, weights))

access_district_main <- characterize(combination2)%>%
  group_by(district_calc,main_activity)%>%
  dplyr::summarize(average=weighted.mean(access, weights)) %>%
  pivot_wider(names_from = "main_activity",values_from = "average")
#--------------------------------------------------------------------------

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

#L.5.1.1b - new ----------------------------
#Overall access to employment ---
combination5<-combination%>%
  select(uniqueid, district_calc,main_activity,gender, geo_entity, refuge, pwd, age_group, stratum,geo_entity, youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, youth_gender_equity_1,youth_improv_work_1, weights)%>%
  rowwise()%>%
  mutate(growth=mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE), 
         growth1=round(mean((youth_contrib_econ_1:youth_improv_work_1), na.rm=TRUE)),
         growth2=case_when(growth1%in%c(4,5)~1, growth1%in%c(1,2,3)~0))


#---------------------------------------------------------------------------
#disaggregation by district
growth_district<-combination5%>%
  group_by(district_calc)%>%
  dplyr::summarize(average=weighted.mean(growth, weights))

growth_district_main <- characterize(combination5)%>%
  group_by(district_calc,main_activity)%>%
  dplyr::summarize(average=weighted.mean(growth, weights)) %>%
  pivot_wider(names_from = "main_activity",values_from = "average")

#-----------------------------------------------------------------------------

#L5.1.2a this is not a sector specific analysis-----------------
combination4<-mcf_data%>%
  select(gender, district_calc,main_activity,geo_entity, refuge, pwd, age_group, stratum, get_work,workplaces_val,
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
  dplyr::summarize(average=weighted.mean(aspirations, weights))

aspirations_district_main <- characterize(combination4)%>%
  group_by(district_calc,main_activity)%>%
  dplyr::summarize(average=weighted.mean(aspirations, weights)) %>%
  pivot_wider(names_from = "main_activity",values_from = "average")

#-----------------------------------------------------------------------------

