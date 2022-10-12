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
library(tibble)
library(plyr)
library(data.table)
#Load in data ------------ 
mcf_data<-read_dta("data/mcf_data_new.dta")

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

variables<-(var_label(mcf_data)) # this will create a list of variables and their labels
`%notin%` <- Negate(`%in%`)

#L4.3.1.a ------
min_wage<-80950.57143
#Analysis A
#Step 1 and 2
mcf_data<-mcf_data%>%
  dplyr::mutate(month_inco_total=as.numeric(inco_total)/12,
         categ_inco_total=dplyr::case_when(month_inco_total>=(min_wage+(min_wage*0.1))~'Above', 
                                    month_inco_total<(min_wage+(min_wage*0.1))~'Below'))


mcf_data<-mcf_data%>%
  dplyr::mutate(marker1=dplyr::case_when(indi_need%in%c(5,4)&fami_need%in%c(5,4)~1, 
                           indi_need%in%c(1,2,3)|fami_need%in%c(1,2,3)~0),
         marker2=dplyr::case_when(resp_workplace%in%c(5,4)~1,
                           resp_workplace%in%c(1,2,3)~0),
         marker3=dplyr::case_when(resp_carabout%in%c(5,4)~1,
                           resp_carabout%in%c(1,2,3)~0),
         marker4=dplyr::case_when(sense_purp%in%c(5,4)~1,
                           sense_purp%in%c(1,2,3)~0),
         mkr1n2=dplyr::case_when(marker1==1&marker2==1~1, TRUE~0),
         DFWIA1=dplyr::case_when(categ_inco_total=='Above'&(marker1==1|
                                                       marker2==1|
                                                       marker3==1|
                                                       marker4==1)~1, TRUE~0))%>%
  dplyr::mutate(markers_nber= rowSums(.[ ,c('marker1', 'marker2', 'marker3', 'marker4')]),
         DFW2=dplyr::case_when(markers_nber>=2~1,
                        markers_nber<2~0),)

#L5.1.1 a----
#Create four different data sets 
data1<-as.data.frame(mcf_data)%>%
  dplyr::select(uniqueid,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_1:youth_improv_work_1)) #add weights column 
data2<-as.data.frame(mcf_data)%>%
  dplyr::select(uniqueid,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_2:youth_improv_work_2))
data3<-as.data.frame(mcf_data)%>%
  dplyr::select(uniqueid,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_3:youth_improv_work_3))
data4<-as.data.frame(mcf_data)%>%
  dplyr::select(uniqueid,weights,gender, age_group, geo_entity,pwd,refuge,stratum, main_activity, sector_choices,c(sector_choices_name_4:youth_improv_work_4))

#Match columns name for each dataset 
colnames(data4)<-colnames(data1)
colnames(data3)<-colnames(data1)
colnames(data2)<-colnames(data1)

combination1<-rbind.fill(data1,data2,data3,data4)

combination<-as.data.frame(combination1)%>% #should have weights 
  dplyr::filter(!is.na(opp_start_business_1))%>%
  dplyr::filter(sector_choices_name_1!='')%>%
  dplyr::select(-c(agreeing_1))%>%
  set_value_labels(ease_access_digni_1=c(`e. Very easy`=5,  `d. Easy`=4, `c. Difficult`=3,  `b. Very difficult`=2, `a. Haven't worked in this sector`=1))
combination$sector_choices_name_1[combination$sector_choices_name_1=='Creative industries']<-'Tourism&Hospitality'

#Overall access to employment ----
combination2<-combination%>%
  dplyr::select(uniqueid,gender, geo_entity, refuge, pwd, age_group, stratum,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
  # group_by(uniqueid,gender, geo_entity, refuge, pwd, age_group, stratum)%>%
  # dplyr::summarise(across(ease_access_digni_1:weights,~mean(.x, na.rm=TRUE)))%>%
  dplyr::rowwise()%>%
  dplyr::mutate(access=mean(employing_youth_1:opp_lead_position_1, na.rm=TRUE), 
         access1=round(mean((ease_access_digni_1:youth_advancement_1), na.rm=TRUE)),
         access2=dplyr::case_when(access1%in%c(4,5)~1, access1%in%c(1,2,3)~0))
markers_access<-combination%>%
  dplyr::select(stratum,ease_access_digni_1 ,employing_youth_1,
         youth_country_lead_1,opp_start_business_1,employing_women_1,
         opp_lead_position_1,youth_advancement_1, weights)%>%
  dplyr::group_by()%>%
  dplyr::summarize(across(ease_access_digni_1:youth_advancement_1, ~weighted.mean(.x, weights, na.rm=TRUE)))



n_occur <- data.frame(table(combination$uniqueid))

filtered_comb<-combination2%>%
  dplyr::group_by(uniqueid,gender,geo_entity,refuge,pwd,age_group,stratum,weights)%>%
  dplyr::top_n(1,ease_access_digni_1)

filtered_comb1 <- mcf_data[,"uniqueid"]%>%
  dplyr::left_join(filtered_comb,by="uniqueid")%>%
  dplyr::group_by(uniqueid,gender,geo_entity,refuge,pwd,age_group,stratum,weights)%>%
  dplyr::top_n(1,ease_access_digni_1)%>%
  dplyr::top_n(1,employing_youth_1)%>%
  dplyr::top_n(1,youth_country_lead_1)%>%
  dplyr::top_n(1,opp_start_business_1)%>%
  dplyr::top_n(1,employing_women_1)%>%
  dplyr::top_n(1,opp_lead_position_1)%>%
  dplyr::top_n(1,youth_advancement_1)%>%
  dplyr::distinct()

#Access to employment by sectors ------------------------


combination5<-combination%>%
  dplyr::select(uniqueid, gender, geo_entity, refuge, pwd, age_group, stratum,geo_entity, youth_contrib_econ_1,youth_contrib_innov_1,
         youth_improv_envir_1,youth_particp_local_1, youth_gender_equity_1,youth_improv_work_1, weights)%>%
  dplyr::rowwise()%>%
  dplyr::mutate(growth=mean(youth_contrib_econ_1:youth_improv_work_1, na.rm=TRUE), 
         growth1=round(mean((youth_contrib_econ_1:youth_improv_work_1), na.rm=TRUE)),
         growth2=dplyr::case_when(growth1%in%c(4,5)~1, growth1%in%c(1,2,3)~0))

filtered_comb_growth<-combination5%>%
  dplyr::group_by(uniqueid,gender,geo_entity,refuge,pwd,age_group,stratum,weights)%>%
  dplyr::top_n(1,youth_contrib_econ_1)%>%
  dplyr::top_n(1,youth_contrib_innov_1)%>%
  dplyr::top_n(1,youth_improv_envir_1)%>%
  dplyr::top_n(1,youth_particp_local_1)%>%
  dplyr::top_n(1,youth_gender_equity_1)%>%
  dplyr::top_n(1,youth_improv_work_1)%>%
  dplyr::distinct()


combination4<-mcf_data%>%
  dplyr::select(uniqueid,gender, geo_entity, refuge, pwd, age_group, stratum, get_work,workplaces_val,
         work_rewards,workplace_equit, weights)%>%
  dplyr::filter(!is.na(get_work))%>%
  dplyr::rowwise()%>%
  dplyr::mutate(aspirations=mean(get_work:workplaces_val, na.rm=TRUE), 
         aspirations1=round(mean(get_work:workplace_equit, na.rm=TRUE)),
         aspirations2=dplyr::case_when(aspirations1%in%c(4,5)~1, aspirations1%in%c(1,2,3)~0))%>%
  dplyr::distinct()%>%
  as.data.frame()%>%
  dplyr::mutate(avg_aspiration=rowMeans(dplyr::select(.,all_of(c("get_work","workplaces_val","work_rewards",
                                                                 "workplace_equit"))),na.rm = TRUE))
filled_comb_aspiration <- combination4%>%
  dplyr::select(-aspirations1,-aspirations2,-avg_aspiration)



growth_stratum<-characterize(filtered_comb_growth)%>%
  dplyr::group_by(stratum)%>%
  dplyr::summarize(average=weighted.mean(growth, weights))%>%
  pivot_wider(names_from =stratum,  values_from = average)%>%
  as.data.frame()


