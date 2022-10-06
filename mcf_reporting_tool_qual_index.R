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
#var_df_filter <- grep("_access$", var_df$variable,value=TRUE, ignore.case =T)
mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(sum_quality_life=rowSums(select(.,grep("_access$", var_df$variable,value=TRUE, ignore.case =T)
                                         ),na.rm = TRUE))

# step 2: averaging services improvement (subquestion b related)

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(avg_improv_quality_life=rowMeans(select(.,var_df_b$variable),na.rm = TRUE))

#step 3: computing the product of step 1 and step 2

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prod_quality_life=avg_improv_quality_life*sum_quality_life)

#step 4: adjusting the index to 100 from step 3

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(perc_quality_life=(prod_quality_life*100)/96)

# compute non weighted mean and weighted mean

#qual_mean <- mean(mcf_data_l5_t$perc_quality_life)

qual_mean_w <- weighted.mean(mcf_data_l5_t$perc_quality_life,mcf_data_l5_t$weights)
View(qual_mean_w)
#Compute weighted mean
quality_overall<-mcf_data_l5_t%>%
  dplyr::group_by()%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on geo entity
quality_geo<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(geo_entity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on gender
quality_gender<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(gender)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on age group
quality_agegroup<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(age_group)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on pwd
quality_pwd<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(pwd)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on refugee
quality_refuge<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(refuge)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on stratum
quality_employment<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(stratum)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, weights))

#ANALYSIS B: step 2: proportion of individuals who report an average of 2 

# step 1: averaging services improvement (subquestion b related)

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prop_great=case_when(avg_improv_quality_life==2~1, TRUE~0))

prop_great_overall <- characterize(mcf_data_l5_t) %>%
  group_by(prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))
#disaggregating by gender

prop_great_gender_calc <- characterize(mcf_data_l5_t) %>%
  group_by(gender,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by geo_entity

prop_great_geo_calc <- characterize(mcf_data_l5_t) %>%
  group_by(geo_entity,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by pwd
prop_great_pwd_calc <- characterize(mcf_data_l5_t) %>%
  group_by(pwd,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by refugee status
prop_great_refugee_calc <- characterize(mcf_data_l5_t) %>%
  group_by(refuge,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by age group
prop_great_agegroup_calc <- characterize(mcf_data_l5_t) %>%
  group_by(age_group,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by stratum
prop_great_stratum_calc <- characterize(mcf_data_l5_t) %>%
  group_by(stratum,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))%>%
  filter(prop_great=="Yes")%>%
  select(-c("n","prop_great"))
  


#L5.1.2b -------
#Option a: average ability score 
#work_trainings and training_jb_market

mcf_data_k <- mcf_data%>%
  mutate(work_trainings_t=ifelse(work_trainings==1,5,
                                 ifelse(work_trainings==2,4,
                                        ifelse(work_trainings==3,3,
                                               ifelse(work_trainings==4,2,
                                                      ifelse(work_trainings==5,1,NA))))))
mcf_data_k <- mcf_data_k%>%
  mutate(training_jb_market_t=ifelse(training_jb_market==1,5,
                                 ifelse(training_jb_market==2,4,
                                        ifelse(training_jb_market==3,3,
                                               ifelse(training_jb_market==4,2,
                                                      ifelse(training_jb_market==5,1,NA))))))

mcf_data_k <- mcf_data_k%>%
  mutate(ability_score=rowMeans(select(.,c("work_trainings_t","training_jb_market_t")),na.rm = TRUE))

mcf_data_k <- mcf_data_k%>%
  mutate(ab_score_prop = ifelse(round(ability_score)>=4,"yes","no"))

#Run a reliability test 
psych::alpha(select(mcf_data_k,all_of(c("work_trainings_t","training_jb_market_t"))))

#average ability score 
avg_ability<-mcf_data_k%>%
  group_by()%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))
#Disaggregation by gender 
avg_ability_gender<-characterize(mcf_data_k)%>%
  group_by(gender)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))
#Disaggregation by geoentity 
avg_ability_geoentity<-characterize(mcf_data_k)%>%
  group_by(geo_entity)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))

#Disaggregation by pwds 
avg_ability_pwd<-characterize(mcf_data_k)%>%
  group_by(pwd)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))

#Disaggregation by refugee status 
avg_ability_refugee<-characterize(mcf_data_k)%>%
  group_by(refuge)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))

#Disaggregation by age group 
avg_ability_agegroup<-characterize(mcf_data_k)%>%
  group_by(age_group)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))

#Disaggregation by stratum 
avg_ability_stratum<-characterize(mcf_data_k)%>%
  group_by(stratum)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))


#total youth with ability score of agree or strongly agree

prop_ability_score_calc11 <- characterize(mcf_data_k) %>%
  group_by(ab_score_prop)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(propotional_great = total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from = "ab_score_prop",values_from = "propotional_great")

#disaggregating by gender
prop_ability_score_gender_calc <- characterize(mcf_data_k) %>%
  group_by(gender,ab_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by geo_entity
prop_ability_score_geo_calc <- characterize(mcf_data_k) %>%
  group_by(geo_entity,ab_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by pwd
prop_ability_score_pwd_calc <- characterize(mcf_data_k)%>%
  group_by(pwd,ab_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by refugee status
prop_ability_score_refugee_calc <- characterize(mcf_data_k)%>%
  group_by(refuge,ab_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by age group
prop_ability_score_agegroup_calc <- characterize(mcf_data_k)%>%
  group_by(age_group,ab_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by stratum
prop_ability_score_stratum_calc <- characterize(mcf_data_k)%>%
  group_by(stratum,ab_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))




#L5.1.2c this is not a sector specific analysis-----------------
keyword_label_c<-c("J1.",	"J2.",	"J3.",	"J4.")
variables_for_l512_c <- mcf_data%>%look_for(keyword_label_c)
variables_for_l512_c <-variables_for_l512_c[,"variable"]
var_df_c <- as.data.frame(variables_for_l512_c)
var_df_c <- var_df_c[1:4,]

#Run a reliability test 
psych::alpha(select(mcf_data,all_of(var_df_c)))
#filtering out unemployed and students
mcf_data <- mcf_data%>%
  filter(stratum==1|stratum==2)%>%
  mutate(expectation_score = rowMeans(select(.,all_of(var_df_c)),na.rm = TRUE),
         exp_score_prop = ifelse(round(expectation_score)>=4,1,0))

avg_expectation_total<-characterize(mcf_data)%>%
  group_by()%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))

#Disaggregation by gender 
avg_expectation_gender<-characterize(mcf_data)%>%
  group_by(gender)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))

#Disaggregation by geo entity 
avg_expectation_geoentity<-characterize(mcf_data)%>%
  group_by(geo_entity)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))

#Disaggregation by pwds 
avg_expectation_pwd<-characterize(mcf_data)%>%
  group_by(pwd)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))

#Disaggregation by refugee status 
avg_expectation_refugee<-characterize(mcf_data)%>%
  group_by(refuge)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))

#Disaggregation by age group 
avg_expectation_agegroup<-characterize(mcf_data)%>%
  group_by(age_group)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))

#Disaggregation by stratum 
avg_expectation_stratum<-characterize(mcf_data)%>%
  group_by(stratum)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))


#total youth with ability score of agree or strongly agree

prop_exp_score_calc <- mcf_data %>%
  group_by(exp_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by gender
prop_exp_score_gender_calc <- characterize(mcf_data) %>%
  group_by(gender,exp_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by geo_entity
prop_exp_score_geo_calc <- characterize(mcf_data) %>%
  group_by(geo_entity,exp_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by pwd
prop_exp_score_pwd_calc <- characterize(mcf_data)%>%
  group_by(pwd,exp_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by refugee status
prop_exp_score_refugee_calc <- characterize(mcf_data)%>%
  group_by(refuge,exp_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by age group
prop_exp_score_agegroup_calc <- characterize(mcf_data)%>%
  group_by(age_group,exp_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))

#disaggregating by stratum
prop_exp_score_stratum_calc <- characterize(mcf_data)%>%
  group_by(stratum,exp_score_prop)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))


#######################################################################
# ISIC data disaggregation
######################################################################
qual_mean_w11 <- characterize(mcf_data_l5_t)%>%
  dplyr::group_by(stratum)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))
write.xlsx(qual_mean_w11,"data/stratum_qualindex_isic.xlsx")

qual_mean_w1 <- characterize(mcf_data_l5_t)%>%
  dplyr::group_by(main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))
write.xlsx(qual_mean_w1,"data/sectors_qualindex_isic.xlsx")

#Compute weighted mean

#disaggregating based on geo entity
quality_geo<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(geo_entity,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "geo_entity",values_from = "average")%>%
  as.data.frame()
write.xlsx(quality_geo,"data/geo_isic.xlsx")
#disaggregating based on gender
quality_gender<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(gender,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "gender",values_from = "average")%>%
  as.data.frame()
write.xlsx(quality_gender,"data/gender_isic.xlsx")

#disaggregating based on age group
quality_agegroup<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(age_group,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "age_group",values_from = "average")%>%
  as.data.frame()
write.xlsx(quality_agegroup,"data/agegroup_isic.xlsx")

#disaggregating based on pwd
quality_pwd<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(pwd,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "pwd",values_from = "average")%>%
  rename(pwd_yes=Yes)%>%
  as.data.frame()
write.xlsx(quality_pwd,"data/pwd_isic.xlsx")

#disaggregating based on refugee
quality_refuge<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(refuge,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "refuge",values_from = "average")
  as.data.frame()
write.xlsx(quality_refuge,"data/ref_isic.xlsx")

overall1_qualindex_isic <- qual_mean_w1%>%
  left_join(quality_gender)%>%
  left_join(quality_geo)%>%
  left_join(quality_agegroup)%>%
  left_join(select(quality_pwd,-c("No")))%>%
  left_join(select(quality_refuge,-c("a. Non refuge")))%>%
  as.data.frame()%>%
  select(c("main_activity","average","b. Female","a. Male","b. Refugee","pwd_yes","Rural","Urban","18-24","25-35"))
write.xlsx(overall1_qualindex_isic,"data/overall_qualindex_isic.xlsx")
#ANALYSIS B: step 2: proportion of individuals who report an average of 2 

# step 1: averaging services improvement (subquestion b related)
# ############
mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prop_great=case_when(round(avg_improv_quality_life)==2~"Yes", TRUE~"No"))


######################################################

prop_great_calc1 <- characterize(mcf_data_l5_t) %>%
  group_by(main_activity,prop_great)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(propotional_great = total/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from = "prop_great",values_from = "propotional_great")%>%
  select(-No)%>%
  rename(Total_overall="Yes")

write.xlsx(prop_great_calc1,"data/prop_great_isic.xlsx")

#disaggregating by gender

prop_great_gender_calc <- characterize(mcf_data_l5_t) %>%
  group_by(gender,main_activity,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = n*100/sum(n))%>%
  select(-n)%>%
  pivot_wider(names_from = "gender",values_from = "propotional_great")%>%
  filter(prop_great=="Yes")%>%
  select(-prop_great)%>%
  as.data.frame()
  

#disaggregating by geo_entity

prop_great_geo_calc <- characterize(mcf_data_l5_t) %>%
  group_by(geo_entity,main_activity,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = n*100/sum(n))%>%
  select(-n)%>%
  pivot_wider(names_from = "geo_entity",values_from = "propotional_great")%>%
  filter(prop_great=="Yes")%>%
  select(-prop_great)%>%
  as.data.frame()

#disaggregating by pwd
prop_great_pwd_calc <- characterize(mcf_data_l5_t) %>%
  group_by(pwd,main_activity,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = n*100/sum(n))%>%
  select(-n)%>%
  pivot_wider(names_from = "pwd",values_from = "propotional_great")%>%
  filter(prop_great=="Yes")%>%
  select(-c("prop_great","No"))%>%
  rename(pwd_yes=Yes)%>%
  as.data.frame()

#disaggregating by refugee status
prop_great_refugee_calc <- characterize(mcf_data_l5_t) %>%
  group_by(refuge,main_activity,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = n*100/sum(n))%>%
  select(-n)%>%
  pivot_wider(names_from = "refuge",values_from = "propotional_great")%>%
  filter(prop_great=="Yes")%>%
  select(-c("prop_great","a. Non refuge"))%>%
  as.data.frame()

#disaggregating by age group
prop_great_agegroup_calc <- characterize(mcf_data_l5_t) %>%
  group_by(age_group,main_activity,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = n*100/sum(n))%>%
  select(-n)%>%
  pivot_wider(names_from = "age_group",values_from = "propotional_great")%>%
  filter(prop_great=="Yes")%>%
  select(-prop_great)%>%
  as.data.frame()

overall2_qualindex_prop_isic <- prop_great_calc1%>%
  left_join(prop_great_gender_calc)%>%
  left_join(prop_great_geo_calc)%>%
  left_join(prop_great_pwd_calc)%>%
  left_join(prop_great_refugee_calc)%>%
  left_join(prop_great_agegroup_calc)%>%
  as.data.frame()%>%
  select(c("main_activity","Total_overall","b. Female","a. Male","b. Refugee","pwd_yes","Rural","Urban","18-24","25-35"))
write.xlsx(overall2_qualindex_prop_isic,"data/overall2_qualindex_prop_isic.xlsx")

#L5.1.2b -------
#Option a: average ability score 
#work_trainings and training_jb_market

#average ability score
avg_ability <- characterize(mcf_data_k) %>%
  group_by(main_activity) %>%
  dplyr::summarize(avg_ability_score = round(weighted.mean(ability_score, weights, na.rm =
                                                             TRUE), 2))%>%
  as.data.frame()
#Disaggregation by gender
avg_ability_gender <- characterize(mcf_data_k) %>%
  group_by(gender, main_activity) %>%
  dplyr::summarize(avg_ability_score = round(weighted.mean(ability_score, weights, na.rm =
                                                             TRUE), 2))%>%
  pivot_wider(names_from = "gender",values_from = "avg_ability_score")%>%
  as.data.frame()
#Disaggregation by geoentity
avg_ability_geoentity <- characterize(mcf_data_k) %>%
  group_by(geo_entity, main_activity) %>%
  dplyr::summarize(avg_ability_score = round(weighted.mean(ability_score, weights, na.rm =
                                                             TRUE), 2))%>%
  pivot_wider(names_from = "geo_entity",values_from = "avg_ability_score")%>%
  as.data.frame()

#Disaggregation by pwds
avg_ability_pwd <- characterize(mcf_data_k) %>%
  group_by(pwd, main_activity) %>%
  dplyr::summarize(avg_ability_score = round(weighted.mean(ability_score, weights, na.rm =
                                                             TRUE), 2))%>%
  pivot_wider(names_from = "pwd",values_from = "avg_ability_score")%>%
  select(-No)%>%
  rename(pwd_yes=Yes)%>%
  as.data.frame()

#Disaggregation by refugee status
avg_ability_refugee <- characterize(mcf_data_k) %>%
  group_by(refuge, main_activity) %>%
  dplyr::summarize(avg_ability_score = round(weighted.mean(ability_score, weights, na.rm =
                                                             TRUE), 2))%>%
  pivot_wider(names_from = "refuge",values_from = "avg_ability_score")%>%
  select(-c("a. Non refuge"))%>%
  as.data.frame()

#Disaggregation by age group
avg_ability_agegroup <- characterize(mcf_data_k) %>%
  group_by(age_group, main_activity) %>%
  dplyr::summarize(avg_ability_score = round(weighted.mean(ability_score, weights, na.rm =
                                                             TRUE), 2))%>%
  pivot_wider(names_from = "age_group",values_from = "avg_ability_score")%>%
  as.data.frame()

overall3_avg_ability_score_isic <- avg_ability%>%
  left_join(avg_ability_gender)%>%
  left_join(avg_ability_geoentity)%>%
  left_join(avg_ability_pwd)%>%
  left_join(avg_ability_refugee)%>%
  left_join(avg_ability_agegroup)%>%
  as.data.frame()%>%
  select(c("main_activity","avg_ability_score","b. Female","a. Male","b. Refugee","pwd_yes","Rural","Urban","18-24","25-35"))
write.xlsx(overall3_avg_ability_score_isic,"data/overall3_avg_ability_score_isic.xlsx")






######################################################################
#total youth with ability score of agree or strongly agree

prop_ability_score_calc <- characterize(mcf_data_k) %>%
  group_by(main_activity, ab_score_prop) %>%
  summarise(n = sum(weights))%>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  #filter(ab_score_prop=="Yes")%>%
  select(-n)%>%
  pivot_wider(names_from = "ab_score_prop",values_from = "propotional_great")%>%
  select(c("main_activity","yes"))%>%
  rename(Total_overall=yes)%>%
  
  as.data.frame()

#disaggregating by gender
prop_ability_score_gender_calc <- characterize(mcf_data_k) %>%
  group_by(gender, main_activity, ab_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  
  select(-c("n"))%>%
  pivot_wider(names_from = "gender",values_from = "propotional_great")%>%
  filter(ab_score_prop=="yes")%>%
  select(-ab_score_prop)%>%
  as.data.frame()

#disaggregating by geo_entity
prop_ability_score_geo_calc <- characterize(mcf_data_k) %>%
  group_by(geo_entity, main_activity, ab_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  filter(ab_score_prop=="yes")%>%
  select(-c("n","ab_score_prop"))%>%
  pivot_wider(names_from = "geo_entity",values_from = "propotional_great")%>%
  as.data.frame()

#disaggregating by pwd
prop_ability_score_pwd_calc <- characterize(mcf_data_k) %>%
  group_by(pwd, main_activity, ab_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  filter(ab_score_prop=="yes")%>%
  select(-c("n","ab_score_prop"))%>%
  pivot_wider(names_from = "pwd",values_from = "propotional_great")%>%
  select(-c("No"))%>%
  rename(pwd_yes=Yes)%>%
  as.data.frame()

#disaggregating by refugee status
prop_ability_score_refugee_calc <- characterize(mcf_data_k) %>%
  group_by(refuge, main_activity, ab_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  filter(ab_score_prop=="yes")%>%
  select(-c("n","ab_score_prop"))%>%
  pivot_wider(names_from = "refuge",values_from = "propotional_great")%>%
  select(-c("a. Non refuge"))%>%
  as.data.frame()

#disaggregating by age group
prop_ability_score_agegroup_calc <- characterize(mcf_data_k) %>%
  group_by(age_group, main_activity, ab_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  filter(ab_score_prop=="yes")%>%
  select(-c("n","ab_score_prop"))%>%
  pivot_wider(names_from = "age_group",values_from = "propotional_great")%>%
  as.data.frame()

overall4_prop_ability_score_isic <- prop_ability_score_calc%>%
  left_join(prop_ability_score_gender_calc)%>%
  left_join(prop_ability_score_geo_calc)%>%
  left_join(prop_ability_score_pwd_calc)%>%
  left_join(prop_ability_score_refugee_calc)%>%
  left_join(prop_ability_score_agegroup_calc)%>%
  as.data.frame()%>%
  select(c("main_activity","Total_overall","b. Female","a. Male","b. Refugee","pwd_yes","Rural","Urban","18-24","25-35"))
write.xlsx(overall4_prop_ability_score_isic,"data/overall4_prop_ability_score_isic.xlsx")



# THIS IS THE REMAINING PART!!!!!
#########################################################################
#L5.1.2c this is not a sector specific analysis-----------------

avg_expectation_total<-characterize(mcf_data)%>%
  group_by(main_activity)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))%>%
  as.data.frame()



#Disaggregation by gender
avg_expectation_gender <- characterize(mcf_data) %>%
  group_by(gender, main_activity) %>%
  dplyr::summarize(avg_exp_score = round(weighted.mean(expectation_score, weights, na.rm =
                                                         TRUE), 2))%>%
  pivot_wider(names_from = "gender",values_from = "avg_exp_score")%>%
  as.data.frame()


#Disaggregation by geo entity
avg_expectation_geoentity <- characterize(mcf_data) %>%
  group_by(geo_entity, main_activity) %>%
  dplyr::summarize(avg_exp_score = round(weighted.mean(expectation_score, weights, na.rm =
                                                         TRUE), 2))%>%
  pivot_wider(names_from = "geo_entity",values_from = "avg_exp_score")%>%
  as.data.frame()


#Disaggregation by pwds
avg_expectation_pwd <- characterize(mcf_data) %>%
  group_by(pwd, main_activity) %>%
  dplyr::summarize(avg_exp_score = round(weighted.mean(expectation_score, weights, na.rm =
                                                         TRUE), 2))%>%
  pivot_wider(names_from = "pwd",values_from = "avg_exp_score")%>%
  select(-No)%>%
  rename(pwd_yes=Yes)%>%
  as.data.frame()

#Disaggregation by refugee status
avg_expectation_refugee <- characterize(mcf_data) %>%
  group_by(refuge, main_activity) %>%
  dplyr::summarize(avg_exp_score = round(weighted.mean(expectation_score, weights, na.rm =
                                                         TRUE), 2))%>%
  pivot_wider(names_from = "refuge",values_from = "avg_exp_score")%>%
  select(-c("a. Non refuge"))%>%
  as.data.frame()


#Disaggregation by age group
avg_expectation_agegroup <- characterize(mcf_data) %>%
  group_by(age_group, main_activity) %>%
  dplyr::summarize(avg_exp_score = round(weighted.mean(expectation_score, weights, na.rm =
                                                         TRUE), 2))%>%
  pivot_wider(names_from = "age_group",values_from = "avg_exp_score")%>%
  as.data.frame()

overall5_avg_expectation_isic <- avg_expectation_total%>%
  left_join(avg_expectation_gender)%>%
  left_join(avg_expectation_geoentity)%>%
  left_join(avg_expectation_pwd)%>%
  left_join(avg_expectation_refugee)%>%
  left_join(avg_expectation_agegroup)%>%
  as.data.frame()%>%
  select(c("main_activity","avg_exp_score","b. Female","a. Male","b. Refugee","pwd_yes","Rural","Urban","18-24","25-35"))
write.xlsx(overall5_avg_expectation_isic,"data/overall5_avg_expectation_isic.xlsx")



##########################################################
#total youth with ability score of agree or strongly agree

prop_exp_score_calc <- characterize(mcf_data) %>%
  group_by(main_activity, exp_score_prop) %>%
  summarise(n=sum(weights))%>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  select(-n)%>%
  filter(exp_score_prop==1)%>%
  select(-exp_score_prop)%>%
  as.data.frame()

#disaggregating by gender
prop_exp_score_gender_calc <- characterize(mcf_data) %>%
  group_by(gender, main_activity, exp_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  select(-n)%>%
  filter(exp_score_prop==1)%>%
  pivot_wider(names_from = "gender",values_from = "propotional_great")%>%
  select(-exp_score_prop)%>%
  as.data.frame()

#disaggregating by geo_entity
prop_exp_score_geo_calc <- characterize(mcf_data) %>%
  group_by(geo_entity, main_activity, exp_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  select(-n)%>%
  filter(exp_score_prop=="Yes")%>%
  pivot_wider(names_from = "geo_entity",values_from = "propotional_great")%>%
  select(-exp_score_prop)%>%
  as.data.frame()

#disaggregating by pwd
prop_exp_score_pwd_calc <- characterize(mcf_data) %>%
  group_by(pwd, main_activity, exp_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  select(-n)%>%
  filter(exp_score_prop=="Yes",pwd=="Yes")%>%
  pivot_wider(names_from = "pwd",values_from = "propotional_great")%>%
  select(-exp_score_prop)%>%
  as.data.frame()

#disaggregating by refugee status
prop_exp_score_refugee_calc <- characterize(mcf_data) %>%
  group_by(refuge, main_activity, exp_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  select(-n)%>%
  filter(exp_score_prop=="Yes",refuge!="a. Non refuge")%>%
  pivot_wider(names_from = "refuge",values_from = "propotional_great")%>%
  select(-exp_score_prop)%>%
  as.data.frame()

#disaggregating by age group
prop_exp_score_agegroup_calc <- characterize(mcf_data) %>%
  group_by(age_group, main_activity, exp_score_prop) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_great = round(n * 100 / sum(n), 2))%>%
  select(-n)%>%
  filter(exp_score_prop=="Yes")%>%
  pivot_wider(names_from = "age_group",values_from = "propotional_great")%>%
  select(-exp_score_prop)%>%
  as.data.frame()


overall6_prop_exp_score_isic <- prop_exp_score_calc%>%
  left_join(prop_exp_score_gender_calc)%>%
  left_join(prop_exp_score_geo_calc)%>%
  left_join(prop_exp_score_pwd_calc)%>%
  left_join(prop_exp_score_refugee_calc)%>%
  left_join(prop_exp_score_agegroup_calc)%>%
  as.data.frame()%>%
  select(c("main_activity","propotional_great","b. Female","a. Male","b. Refugee","Yes","Rural","Urban","18-24","25-35"))
write.xlsx(overall6_prop_exp_score_isic,"data/overall6_prop_exp_score_isic.xlsx")





