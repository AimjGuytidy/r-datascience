library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)
library(haven)
library(foreign)
library(labelled)
library(rio)
library(psych)
mcf_data <- read_sav("data/mcf_data_new_reporting_tool.sav")
mcf_data_char <- characterize(mcf_data)
unique(mcf_data$healthcare_access)
#L5.3.1 Quality of life index===========
# Data manipulation
# change the values from 1-6 to 0-4
mcf_data_l5_t <- mcf_data

keyword_label<-c("D14",	"D16",	"D18",	"D20",	"D22",	"D24",	"D26",	"D28",	"D30",	"D32",	"D34",	"D36")
variables_for_l531_a <- mcf_data_l5_t%>%look_for(keyword_label)
variables_for_l531_a <-variables_for_l531_a[,"variable"]
var_df <- as.data.frame(variables_for_l531_a)

for (i in 1:length(keyword_label) ){
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==1]<-0
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==2]<-1
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==3]<-2
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==4]<-3
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==5]<-4
  mcf_data_l5_t[,var_df[i,]][mcf_data_l5_t[,var_df[i,]]==6]<-0
}

# change the values from 1-3 to 0-2 
keyword_label_b<-c("D15",	"D17",	"D19",	"D21",	"D23",	"D25",	"D27",	"D29",	"D31",	"D33",	"D35",	"D37")
variables_for_l531_b <- mcf_data_l5_t%>%look_for(keyword_label_b)
variables_for_l531_b <-variables_for_l531_b[,"variable"]
var_df_b <- as.data.frame(variables_for_l531_b)

for (i in 1:length(keyword_label_b) ){
  mcf_data_l5_t[,var_df_b[i,]][mcf_data_l5_t[,var_df_b[i,]]==1]<-0
  mcf_data_l5_t[,var_df_b[i,]][mcf_data_l5_t[,var_df_b[i,]]==2]<-1
  mcf_data_l5_t[,var_df_b[i,]][mcf_data_l5_t[,var_df_b[i,]]==3]<-2
  mcf_data_l5_t[,var_df_b[i,]][is.na(mcf_data_l5_t[,var_df_b[i,]])]<-1
}

# step 1: summing values from variables covering subquestion a and indicator l5.3.1
var_df_filter <- grep("_access$", var_df$variable,value=TRUE, ignore.case =T)
mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(sum_quality_life=rowSums(select(.,grep("_access$", var_df$variable,value=TRUE, ignore.case =T)),na.rm = TRUE))

# step 2: averaging services improvement (subquestion b related)

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(avg_improv_quality_life=rowMeans(select(.,grep("_access$", var_df$variable,value=TRUE, ignore.case =T)),na.rm = TRUE))

#step 3: computing the product of step 1 and step 2

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prod_quality_life=avg_improv_quality_life*sum_quality_life)

#step 4: adjusting the index to 100 from step 3

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(perc_quality_life=(prod_quality_life*100)/96)

# compute non weighted mean and weighted mean

#qual_mean <- mean(mcf_data_l5_t$perc_quality_life)

qual_mean_w <- weighted.mean(mcf_data_l5_t$perc_quality_life,mcf_data_l5_t$weights)
round(qual_mean_w,2)
#Compute weighted mean

#disaggregating based on geo entity
quality_geo<-mcf_data_l5_t%>%
  dplyr::group_by(geo_entity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on gender
quality_gender<-mcf_data_l5_t%>%
  dplyr::group_by(gender)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on age group
quality_agegroup<-mcf_data_l5_t%>%
  dplyr::group_by(age_group)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on pwd
quality_pwd<-mcf_data_l5_t%>%
  dplyr::group_by(pwd)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on refugee
quality_refuge<-mcf_data_l5_t%>%
  dplyr::group_by(refuge)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on stratum
quality_employment<-mcf_data_l5_t%>%
  dplyr::group_by(stratum)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#ANALYSIS B: step 2: proportion of individuals who report an average of 2 

# step 1: averaging services improvement (subquestion b related)

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prop_great=case_when(avg_improv_quality_life==2~1, TRUE~0))

prop_great_count <- mcf_data_l5_t %>%
  count(prop_great,wt=weights)

prop_great_calc <- mcf_data_l5_t %>%
  count(prop_great,wt=weights)%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by gender
prop_great_gender_count <- mcf_data_l5_t %>%
  count(gender,prop_great,wt=weights)


prop_great_gender_calc <- mcf_data_l5_t %>%
  group_by(gender,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by geo_entity
prop_great_geo_count <- mcf_data_l5_t %>%
  count(geo_entity,prop_great,wt=weights)


prop_great_geo_calc <- mcf_data_l5_t %>%
  group_by(geo_entity,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by pwd
prop_great_pwd_calc <- mcf_data_l5_t %>%
  group_by(pwd,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by refugee status
prop_great_refugee_calc <- mcf_data_l5_t %>%
  group_by(refuge,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by age group
prop_great_agegroup_calc <- mcf_data_l5_t %>%
  group_by(age_group,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by stratum
prop_great_stratum_calc <- mcf_data_l5_t %>%
  group_by(stratum,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))


#L5.1.2b -------
#Option a: average ability score 
mcf_data<-mcf_data%>%
  mutate(educ_knowledge1=5*((educ_knowledge-min(educ_knowledge,na.rm = TRUE))/(max(educ_knowledge,na.rm = TRUE)-min(educ_knowledge,na.rm=TRUE))))
mcf_data<-mcf_data%>%
  mutate(educ_relavent1=5*((educ_relavent-min(educ_relavent,na.rm = TRUE))/(max(educ_relavent,na.rm = TRUE)-min(educ_relavent,na.rm=TRUE))))

skillswork<-mcf_data%>%
  dplyr::select(educ_knowledge1,educ_relavent1)
psych::alpha(skillswork) #reliability test 

#educ_knowledge 
knowledge_ability<-mcf_data%>%
  group_by()%>%
  dplyr::summarize(knowledge_average=round(weighted.mean(educ_knowledge1, weights,na.rm=TRUE),2))
#Disaggregation by gender 
knowledge_ability_gender<-mcf_data%>%
  group_by(gender)%>%
  dplyr::summarize(knowledge_average=round(weighted.mean(educ_knowledge1, weights,na.rm=TRUE),2))
#Disaggregation by geoentity 
knowledge_ability_geoentity<-mcf_data%>%
  group_by(geo_entity)%>%
  dplyr::summarize(knowledge_average=round(weighted.mean(educ_knowledge1, weights,na.rm=TRUE),2))

#Disaggregation by pwds 
knowledge_ability_pwd<-mcf_data%>%
  group_by(pwd)%>%
  dplyr::summarize(knowledge_average=round(weighted.mean(educ_knowledge1, weights,na.rm=TRUE),2))

#Disaggregation by refugee status 
knowledge_ability_refugee<-mcf_data%>%
  group_by(refuge)%>%
  dplyr::summarize(knowledge_average=round(weighted.mean(educ_knowledge1, weights,na.rm=TRUE),2))

#Disaggregation by age group 
knowledge_ability_agegroup<-mcf_data%>%
  group_by(age_group)%>%
  dplyr::summarize(knowledge_average=round(weighted.mean(educ_knowledge1, weights,na.rm=TRUE),2))

#educ_relavent 
educrelevant_ability<-mcf_data%>%
  group_by()%>%
  dplyr::summarize(educrelevant_average=weighted.mean(educ_relavent1, weights,na.rm=TRUE))
#Disaggregation by gender 
educrelevant_ability1<-mcf_data%>%
  group_by(gender)%>%
  dplyr::summarize(educrelevant_average=weighted.mean(educ_relavent1, weights,na.rm=TRUE))
#Disaggregation by geoentity 
educrelevant_ability2<-mcf_data%>%
  group_by(geo_entity)%>%
  dplyr::summarize(educrelevant_average=weighted.mean(educ_relavent1, weights,na.rm=TRUE))

#Option b 
#Proportion of strongly agree/agree educ_knowledge 
mcf_data<-mcf_data%>%
  mutate(agree_knowledge=case_when(educ_knowledge%in%c(1,2)~1,TRUE~0))%>%
  move_columns(agree_knowledge,.after = educ_knowledge)
#Overall
agree_knowledge1<-mcf_data%>%
  group_by(agree_knowledge)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(propor=total/sum(total))
#Disaggreation by gender 
agree_knowledge_gender<-mcf_data%>%
  group_by(gender,agree_knowledge)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(propor=total/sum(total))
#Disaggreation by geoentity 
agree_knowledge_geoentity<-mcf_data%>%
  group_by(geo_entity,agree_knowledge)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(propor=total/sum(total)) 

#Proportion of strongly agree/agree educ_relavent 
mcf_data<-mcf_data%>%
  mutate(agree_relevant=case_when(educ_relavent%in%c(1,2)~1,TRUE~0))%>%
  move_columns(agree_relevant,.after = educ_relavent)
#Overall
agree_relevant1<-mcf_data%>%
  group_by(agree_relevant)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(propor=total/sum(total))
#Disaggreation by gender 
agree_relevant_gender<-mcf_data%>%
  group_by(gender,agree_relevant)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(propor=total/sum(total))
#Disaggreation by geoentity 
agree_relevant_geoentity<-mcf_data%>%
  group_by(geo_entity,agree_relevant)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(propor=total/sum(total))  

#L5.1.2c this is not a sector specific analysis-----------------
indicator2c<-combination%>% #should have weights as well 
  select(gender, geo_entity, work_pay_indiv,work_pay_fam,work_from_home,
         work_freedom, weights)

#Run a reliability test 
psych::alpha(indicator2c)

#weighted average part a
score_indicator2c<-indicator2c%>%
  group_by(geo_entity)%>% #you can group_by gender, geo_entity, etc; for overall, you don't group_by
  dplyr::summarize(work_pay_indiv=weighted.mean(work_pay_indiv,weights, na.rm = TRUE),
                   work_pay_fam=weighted.mean(work_pay_fam,weights , na.rm = TRUE ),
                   work_from_home=weighted.mean(work_from_home,weights , na.rm = TRUE ),
                   work_freedom=weighted.mean(work_freedom,weights , na.rm = TRUE ))
write.xlsx(score_indicator2c, 'scores_indicator2c_3.xlsx')

#part b Here were are looking at those that agree and strongly agree which are coded as 1
indicator2c_dummy<-indicator2c%>% #to dis-aggregate, use group_by gender and geo_entity
  mutate(work_pay_indiv=case_when(work_pay_indiv%in%c(4,5)~1, TRUE~0),
         work_pay_fam=case_when(work_pay_fam%in%c(4,5)~1, TRUE~0),
         work_from_home=case_when(work_from_home%in%c(4,5)~1, TRUE~0),
         work_freedom=case_when(work_freedom%in%c(4,5)~1, TRUE~0))

work_pay_indiv<-wpct(indicator2c_dummy$work_pay_indiv,indicator2c_dummy$weights)
work_pay_fam<-wpct(indicator2c_dummy$work_pay_fam,indicator2c_dummy$weights)
work_from_home<-wpct(indicator2c_dummy$work_from_home,indicator2c_dummy$weights)
work_freedom<-wpct(indicator2c_dummy$work_freedom,indicator2c_dummy$weights)

indicator2c_prop<-as.data.frame(rbind(work_pay_indiv,work_pay_fam,work_from_home,
                                      work_freedom))
indicator2c_prop<-rownames_to_column(indicator2c_prop)
write.xlsx(indicator2c_prop,'indicator2c proportions.xlsx')

#gender and geo_entity
indicator2c_rural<-indicator2c%>% #to dis-aggregate, use group_by gender and geo_entity
  filter(geo_entity%in%1)%>%
  mutate(work_pay_indiv=case_when(work_pay_indiv%in%c(4,5)~1, TRUE~0),
         work_pay_fam=case_when(work_pay_fam%in%c(4,5)~1, TRUE~0),
         work_from_home=case_when(work_from_home%in%c(4,5)~1, TRUE~0),
         work_freedom=case_when(work_freedom%in%c(4,5)~1, TRUE~0))

work_pay_indiv<-wpct(indicator2c_rural$work_pay_indiv,indicator2c_rural$weights)
work_pay_fam<-wpct(indicator2c_rural$work_pay_fam,indicator2c_rural$weights)
work_from_home<-wpct(indicator2c_rural$work_from_home,indicator2c_rural$weights)
work_freedom<-wpct(indicator2c_rural$work_freedom,indicator2c_rural$weights)

indicator2c_prop<-as.data.frame(rbind(work_pay_indiv,work_pay_fam,work_from_home,
                                      work_freedom))
indicator2c_prop<-rownames_to_column(indicator2c_prop)
write.xlsx(indicator2c_prop,'indicator2c proportions_ur.xlsx')












