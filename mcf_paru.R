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
mcf_data_l5_t$new_improv_employment <- NULL

#write_dta(mcf_data_l5_t,"data/mcf_parfait.dta")
