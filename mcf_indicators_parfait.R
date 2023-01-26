#Load required packages
library(foreign) #for reading .dta files
library(dplyr) #for the filter function
library(questionr) #for the weight function
library(tidyverse)
library(haven)
library(openxlsx)
library(readr)
library(tidyr)
library(rio)
library(expss)
library(labelled)
library(weights)
library(anesrake)
library(stringr)
library(zoo)
library(sjmisc)
#Load in data
mcf_data<-read_dta("data/MCF_Baseline_Main data - Clean.dta")
# rearranging the columns
mcf_data<-mcf_data%>%
  move_columns(c(type_employ,main_activity), .after=give_chance)%>%
  move_columns(c(main_month,main_years), .before =inc_impro)%>%
  move_columns(c(last_year,year_before), .before =intro_prod)%>%
  move_columns(c(opera_month,opera_years), .before =job_accept)

variables<-var_label(mcf_data)

#Add stratum column
`%notin%` <- Negate(`%in%`)
mcf_data<-mcf_data%>%
  mutate(stratum=case_when(type_employ==1~1,
                           type_employ==2~2,
                           current_educ%notin%c(1, NA)&is.na(type_employ)~3,
                           current_educ%in%c(1, NA)&is.na(type_employ)~4))%>%
  set_value_labels(stratum=c(`Wage employed`=1,
                             `Self employed`=2,
                             `Students`=3,
                             ` Unemployed/Non job-seekers`=4))

#Checking the number in each stratum
stratum<-mcf_data%>%
  group_by(stratum,geo_entity,gender)%>%
  dplyr::summarize(total=n())%>%
  pivot_wider(names_from = c(gender),values_from = c(total))%>%
  rename(Male=`1`, Female=`2`)%>%
  mutate(total=sum(Male, Female))

#Cell weighting 
total_pop<-read.xlsx('data/total_pop.xlsx')
total_pop<-total_pop%>%
  mutate(stratum=case_when(stratum=="Wage employed"~1,
                           stratum=="Self employed"~2,
                           stratum=="Students"~3,
                           stratum=="Unemployed/Non-job seeker"~4),
         geo_entity=case_when(geo_entity=="Rural"~2,
                              geo_entity=="Urban"~1))
cell_weights<-left_join(total_pop, stratum)%>%
  mutate(male_wt=Total.Male/Male, female_wt=Total.Female/Female)
cell_weights<-cell_weights%>%
  select(stratum,geo_entity,female_wt, male_wt)
colnames(cell_weights)<-c('stratum','geo_entity','Female', 'Male')
cell_weights<-cell_weights%>%
  pivot_longer(cols=c('Female','Male'), names_to='gender1', values_to='cell_weights')%>%
  mutate(gender=case_when(gender1=='Male'~1,
                          gender1=='Female'~2))%>%
  select(-c(gender1))

#Add the weight column by joins  
mcf_data<- left_join(mcf_data,cell_weights, by=c("stratum","geo_entity","gender"))%>%
  move_columns(c(main_month, main_years),.before=inc_impro)

#Create a date column showing the year and the month that people started as self employed
mcf_data<-mcf_data%>%
  mutate(new_self_employment=case_when(type_employ==2&main_month<3&main_years==0~1,
                                       type_employ==2&!(main_month<3&main_years==0)&!is.na(main_month)~0),
         sust_self_employment=if_else(new_self_employment==0,1,0),
         new_improv_employment = ifelse(inc_impro %in% c(1,2)|wor_condi %in% c(1,2),1,0),
         new_improv_self_employment=ifelse(type_employ==2,new_improv_employment,NA))%>%
  move_columns(c(new_self_employment,sust_self_employment), .after=main_years)%>%
  move_columns(new_improv_employment, .after=word_condi)

# weighting the newly generated variables----
# variable: New self employment
# generating the number of new self employed youth for the sampled youth population
new_self_sample_weight<-mcf_data%>%
  group_by(new_self_employment)%>%
  dplyr::summarize(total=n()/nrow(mcf_data))
# generating the number of new self employed youth taking into account the whole youth population
new_self_pop_weight<-mcf_data%>%
  group_by(new_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))

#disaggregating by gender
new_self_gender_sample_weight<-mcf_data%>%
  group_by(gender,new_self_employment)%>%
  dplyr::summarize(total=n()/nrow(mcf_data))

gender_weight_new<-mcf_data%>%
  group_by(gender,new_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))

#disaggregating by stratum
stratum_weight_new<-mcf_data%>%
  group_by(stratum,new_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))

#disaggregating by geo_entity
new_self_geo_sample_weight<-mcf_data%>%
  group_by(geo_entity,new_self_employment)%>%
  dplyr::summarize(total=n()/nrow(mcf_data))

geo_weight_new<-mcf_data%>%
  group_by(geo_entity,new_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))
# variable: sustainable self employment
# generating the number of sustainable self employed youth for the sampled youth population
sust_self_sample_weight<-mcf_data%>%
  group_by(sust_self_employment)%>%
  dplyr::summarize(total=n()/nrow(mcf_data))

# generating the number of sustainable self employed youth taking into account the whole youth population
sust_self_pop_weight<-mcf_data%>%
  group_by(sust_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))

sust_self_gender_sample_weight<-mcf_data%>%
  group_by(gender,sust_self_employment)%>%
  dplyr::summarize(total=n()/nrow(mcf_data))

#disaggregating by gender
gender_weight_sust<-mcf_data%>%
  group_by(gender,sust_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))

#disaggregating by stratum
stratum_weight_sust<-mcf_data%>%
  group_by(stratum,sust_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))

#disaggregating by geo_entity
sust_self_geo_sample_weight<-mcf_data%>%
  group_by(geo_entity,sust_self_employment)%>%
  dplyr::summarize(total=n()/nrow(mcf_data))

geo_weight_sust<-mcf_data%>%
  group_by(geo_entity,sust_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))
# variable: New improved self employment

# generating the number of new improved self employed youth for the sampled youth population
new_improv_self_sample_weight<-mcf_data%>%
  group_by(new_improv_self_employment)%>%
  dplyr::summarize(total=n()/nrow(mcf_data))

# generating the number of new improved self employed youth taking into account the whole youth population
new_improv_self_pop_weight<-mcf_data%>%
  group_by(new_improv_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))

#disaggregating by gender
improv_gender_sample_weight<-mcf_data%>%
  group_by(gender,new_improv_self_employment)%>%
  dplyr::summarize(total=n()/nrow(mcf_data))

gender_weight_improv<-mcf_data%>%
  group_by(gender,new_improv_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))
view(gender_weight_improv)

gender_weight_improv1<-mcf_data%>%
  group_by(gender,new_improv_self_employment)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  ungroup()%>%
  dplyr::mutate(prp= n / sum(n))
view(gender_weight_improv1)

gender_weight_improv2<-mcf_data%>%
  group_by(gender,new_improv_self_employment)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  dplyr::mutate(prp= n / sum(n))
view(gender_weight_improv2)

#disaggregating by stratum
stratum_weight_improv<-mcf_data%>%
  group_by(stratum,new_improv_self_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))
view(stratum_weight_improv)
#for the following code we are calculating the percentage of improved employment among the youth population
stratum_weight_improv1<-mcf_data%>%
  group_by(stratum,new_improv_self_employment)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  ungroup()%>%
  dplyr::mutate(prp= n / sum(n))
view(stratum_weight_improv1)
#for the following code we are calculating the percentage of improved employment among the self employed population
stratum_weight_improv2<-mcf_data%>%
  group_by(stratum,new_improv_self_employment)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  dplyr::mutate(prp= n / sum(n))
view(stratum_weight_improv2)

#for the following code we are calculating the percentage of improved employment among the self employed population
stratum_weight_improv3<-mcf_data%>%
  group_by(stratum,new_improv_employment)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  dplyr::mutate(prp= n / sum(n))
view(stratum_weight_improv3)

#for the following code we are calculating the percentage of improved employment among the youth population
stratum_weight_improv4<-mcf_data%>%
  group_by(stratum,new_improv_employment)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  ungroup()%>%
  dplyr::mutate(prp= n / sum(n))
view(stratum_weight_improv4)

#disaggregating by geo_entity
improv_geo_sample_weight<-mcf_data%>%
  group_by(geo_entity,new_improv_self_employment)%>%
  dplyr::summarize(total=n()/nrow(mcf_data))

geo_weight_improv<-mcf_data%>%
  group_by(geo_entity,new_improv_employment)%>%
  dplyr::summarize(total=sum(cell_weights)/sum(total_pop$Total.population))

# INDICATOR L5.3.1 QUALITY OF LIFE INDEX (Individual)===========
# DATA ENGINEERING
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

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(sum_quality_life=rowSums(select(.,var_df$variable),na.rm = TRUE))


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
qual_mean <- mean(mcf_data_l5_t$perc_quality_life)
qual_mean_w <- weighted.mean(mcf_data_l5_t$perc_quality_life,mcf_data_l5_t$cell_weights)
#Compute weighted mean
quality_geo<-mcf_data_l5_t%>%
  dplyr::group_by(geo_entity)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, cell_weights))

quality_gender<-mcf_data_l5_t%>%
  dplyr::group_by(gender)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, cell_weights))


#ANALYSIS B: step 2: proportion of individuals who report an average of 2 

# step 1: averaging services improvement (subquestion b related)

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(avg_improv_quality_life=rowMeans(select(.,var_df_b$variable),na.rm = TRUE))%>%
  mutate(prop_great=case_when(avg_improv_quality_life==2~1, TRUE~0))

prop_great_count <- mcf_data_l5_t %>%
  count(prop_great,wt=cell_weights)

prop_great_calc <- mcf_data_l5_t %>%
  count(prop_great,wt=cell_weights)%>%
  mutate(propotional_great = n*100/sum(n))

#disaggregating by gender
prop_great_gender_count <- mcf_data_l5_t %>%
  count(gender,prop_great,wt=cell_weights)


prop_great_gender_calc <- mcf_data_l5_t %>%
  group_by(gender,prop_great)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  mutate(propotional_great = n*100/sum(n))

#disaggregating by geo_entity
prop_great_geo_count <- mcf_data_l5_t %>%
  count(geo_entity,prop_great,wt=cell_weights)


prop_great_geo_calc <- mcf_data_l5_t %>%
  group_by(geo_entity,prop_great)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  mutate(propotional_great = n*100/sum(n))

################################################################################################
################################################################################################

# INDICATOR L5.3.2 QUALITY OF LIFE INDEX (household)===========

mcf_data_l5_household<-mcf_data_l5_t[mcf_data_l5_t$hhrelation==1,]

# WEIGHTING L5.3.2=========
# compute non weighted mean and weighted mean
qual_mean <- mean(mcf_data_l5_household$perc_quality_life)
qual_mean_w <- weighted.mean(mcf_data_l5_household$perc_quality_life,mcf_data_l5_household$cell_weights)
#Compute weighted mean
quality_geo<-mcf_data_l5_household%>%
  dplyr::group_by(geo_entity)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, cell_weights))

quality_gender<-mcf_data_l5_household%>%
  dplyr::group_by(gender)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, cell_weights))


#ANALYSIS B: step 2: proportion of households who report an average of 2 

# step 1: averaging services improvement (subquestion b related)

prop_great_count <- mcf_data_l5_household %>%
  count(prop_great,wt=cell_weights)

prop_great_calc <- mcf_data_l5_household %>%
  count(prop_great,wt=cell_weights)%>%
  mutate(propotional_great = n*100/sum(n))

#disaggregating by gender
prop_great_gender_count <- mcf_data_l5_household %>%
  count(gender,prop_great,wt=cell_weights)


prop_great_gender_calc <- mcf_data_l5_household %>%
  group_by(gender,prop_great)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  mutate(propotional_great = n*100/sum(n))

#disaggregating by geo_entity
prop_great_geo_count <- mcf_data_l5_household %>%
  count(geo_entity,prop_great,wt=cell_weights)


prop_great_geo_calc <- mcf_data_l5_household %>%
  group_by(geo_entity,prop_great)%>%
  dplyr::summarize(n=sum(cell_weights))%>%
  mutate(propotional_great = n*100/sum(n))

#write_dta(mcf_data_l5_t,"data/mcf_clean_parfait.dta")
