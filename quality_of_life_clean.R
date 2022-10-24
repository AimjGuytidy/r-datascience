library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)
library(haven)
library(foreign)
library(labelled)
library(rio)
library(psych)
data_directory <- "G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/"


data_path <- "4. QUANT/2. Data analysis/2. Primary data analysis/Databook/data/mcf_data_brkdwn.dta"

mcf_data <-
  read_dta(paste0(
    data_directory,
    data_path
    ))

#L5.3.1 Quality of life index===========
# Data manipulation
# change the values from 1-6 to 0-4
mcf_data_l5_t <- mcf_data

keyword_label<-c("D14",	"D16",	"D18",	"D20",	"D22",	"D24",	"D26",	"D28",	"D30",	"D32",	"D34",	"D36")
variables_jb <- mcf_data_l5_t%>%look_for(keyword_label)
variables_jb <-variables_jb[,"variable"]
var_df_jb <- as.data.frame(variables_jb)

for (i in 1:length(keyword_label) ){
  mcf_data_l5_t[,var_df_jb[i,]][mcf_data_l5_t[,var_df_jb[i,]]==1]<-0
  mcf_data_l5_t[,var_df_jb[i,]][mcf_data_l5_t[,var_df_jb[i,]]==2]<-1
  mcf_data_l5_t[,var_df_jb[i,]][mcf_data_l5_t[,var_df_jb[i,]]==3]<-2
  mcf_data_l5_t[,var_df_jb[i,]][mcf_data_l5_t[,var_df_jb[i,]]==4]<-3
  mcf_data_l5_t[,var_df_jb[i,]][mcf_data_l5_t[,var_df_jb[i,]]==5]<-4
}

# change the values from 1-3 to 0-2 
keyword_label_b<-c("D15",	"D17",	"D19",	"D21",	"D23",	"D25",	"D27",	"D29",	"D31",	"D33",	"D35",	"D37")
variables_jb_change <- mcf_data_l5_t%>%look_for(keyword_label_b)
variables_jb_change <-variables_jb_change[,"variable"]
var_df_jb_b <- as.data.frame(variables_jb_change)

for (i in 1:length(keyword_label_b) ){
  mcf_data_l5_t[,var_df_jb_b[i,]][mcf_data_l5_t[,var_df_jb_b[i,]]==1]<-0
  mcf_data_l5_t[,var_df_jb_b[i,]][mcf_data_l5_t[,var_df_jb_b[i,]]==2]<-1
  mcf_data_l5_t[,var_df_jb_b[i,]][mcf_data_l5_t[,var_df_jb_b[i,]]==3]<-2
}

# step 1: summing values from variables covering subquestion a and indicator l5.3.1----

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(sum_quality_life=rowSums(select(.,grep("_access$", var_df_jb$variable,value=TRUE, ignore.case =T)
  ),na.rm = TRUE))

# step 2: averaging services improvement (subquestion b related)

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(avg_improv_quality_life=rowMeans(select(.,var_df_jb_b$variable),na.rm = TRUE))

#step 3: computing the product of step 1 and step 2

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prod_quality_life=avg_improv_quality_life*sum_quality_life)

#step 4: adjusting the index to 100 from step 3

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(perc_quality_life=(prod_quality_life*100)/96)


# compute non weighted mean and weighted mean

#Compute weighted mean
quality_overall_jb<-mcf_data_l5_t%>%
  dplyr::group_by()%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm=TRUE),2))

#disaggregating based on geo entity
quality_geo_jb<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(geo_entity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on gender
quality_gender_jb<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(gender)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on age group
quality_agegroup_jb<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(age_group)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on pwd
quality_pwd_jb<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(pwd)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on refugee
quality_refuge_jb<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(refuge)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))

#disaggregating based on stratum
quality_employment_jb<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(stratum)%>%
  dplyr::summarize(average=weighted.mean(perc_quality_life, weights))

#ANALYSIS B: step 2: proportion of individuals who report an average of 2 ------

# step 1: averaging services improvement (subquestion b related)

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prop_great=case_when(round(avg_improv_quality_life)==2~1, TRUE~0))


#overall average
prop_great_overall_jb <- characterize(mcf_data_l5_t) %>%
  group_by(prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  select(-c("n","prop_great"))
#disaggregating by gender

prop_great_gender_calc_jb <- characterize(mcf_data_l5_t) %>%
  group_by(gender,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  select(-c("n","prop_great"))

#disaggregating by geo_entity

prop_great_geo_calc_jb <- characterize(mcf_data_l5_t) %>%
  group_by(geo_entity,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  select(-c("n","prop_great"))

#disaggregating by pwd
prop_great_pwd_calc_jb <- characterize(mcf_data_l5_t) %>%
  group_by(pwd,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  select(-c("n","prop_great"))

#disaggregating by refugee status
prop_great_refugee_calc_jb <- characterize(mcf_data_l5_t) %>%
  group_by(refuge,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  select(-c("n","prop_great"))

#disaggregating by age group
prop_great_agegroup_calc_jb <- characterize(mcf_data_l5_t) %>%
  group_by(age_group,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  select(-c("n","prop_great"))

#disaggregating by stratum
prop_great_stratum_calc_jb <- characterize(mcf_data_l5_t) %>%
  group_by(stratum,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_great = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  select(-c("n","prop_great"))

#quality of life improvement score-------------

#disaggregating average quality of life improvement by demographics 

avg_qual_life_improv_segments <- characterize(mcf_data_l5_t)%>%
  group_by(stratum)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm=TRUE),2))%>%
  pivot_longer(stratum,names_to = "name",values_to = "value")



