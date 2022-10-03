library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)
library(haven)
library(foreign)
library(labelled)
library(rio)
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
  mutate(propotional_great = n*100/sum(n))