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

#disaggregating based on geo entity
quality_geo<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(geo_entity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_longer(geo_entity,names_to = "name",values_to = "value")

#disaggregating based on gender
quality_gender<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(gender)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_longer(gender,names_to = "name",values_to = "value")
quality_gender$value<-gsub("^[a-zA-Z0-9]\\.\\s","",quality_gender$value)
#disaggregating based on age group
quality_agegroup<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(age_group_brkdwn)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_longer(age_group_brkdwn,names_to = "name",values_to = "value")
#disaggregating based on pwd
quality_pwd<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(pwd)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  filter(pwd=="Yes")%>%
  mutate(pwd= ifelse(pwd=="Yes","PWD",NA))%>%
  pivot_longer(pwd,names_to = "name",values_to = "value")
#disaggregating based on refugee
quality_refuge<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(refugee_brkdwn)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  filter(refugee_brkdwn=="Refugee")%>%
  pivot_longer(refugee_brkdwn,names_to = "name",values_to = "value")

#disaggregating based on income
quality_income<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(income_brkdwn)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  dplyr::filter(income_brkdwn!="")%>%
  pivot_longer(income_brkdwn,names_to = "name",values_to = "value")

#disaggregating based on education
quality_education<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(education_brkdwn)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_longer(education_brkdwn,names_to = "name",values_to = "value")
#------------------------------------------------------------------------------
#disaggregating based on main sector
quality_sector<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(main_sector)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_longer(main_sector,names_to = "name",values_to = "value")
#------------------------------------------------------------------------------ 
#disaggregating based on underemployed
quality_underemployed<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_3)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_3=ifelse(labor_force_status_3==1,var_label(labor_force_status_3),0))%>%
  filter(labor_force_status_3!=0) %>%
  pivot_longer(labor_force_status_3,names_to = "name",values_to = "value")

#disaggregating based on fully employed
quality_fullyemployed<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_4)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_4=ifelse(labor_force_status_4==1,var_label(labor_force_status_4),0))%>%
  filter(labor_force_status_4!=0) %>%
  pivot_longer(labor_force_status_4,names_to = "name",values_to = "value")

#disaggregating based on unemployed
quality_unemployed<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_5)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_5=ifelse(labor_force_status_5==1,var_label(labor_force_status_5),0))%>%
  filter(labor_force_status_5!=0) %>%
  pivot_longer(labor_force_status_5,names_to = "name",values_to = "value")

#disaggregating based on Non-seeker
quality_Non_seeker<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_6)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_6=ifelse(labor_force_status_6==1,var_label(labor_force_status_6),0))%>%
  filter(labor_force_status_6!=0) %>%
  pivot_longer(labor_force_status_6,names_to = "name",values_to = "value")

#disaggregating based on Labor force
quality_Labor_force<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_7)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_7=ifelse(labor_force_status_7==1,var_label(labor_force_status_7),0))%>%
  filter(labor_force_status_7!=0) %>%
  pivot_longer(labor_force_status_7,names_to = "name",values_to = "value")



df_quality_life_demo <-rbind(quality_gender,quality_geo,
                             quality_agegroup,quality_pwd,
                             quality_refuge,
                             quality_income,
                             quality_education,
                             quality_underemployed,
                             quality_fullyemployed,
                             quality_unemployed,
                             quality_Non_seeker,
                             quality_Labor_force)

#Disaggregation by stratum
qual_life_stratum<-characterize(mcf_data_l5_t)%>%
  group_by(stratum)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm=TRUE),2))%>%
  mutate(stratum=case_when(stratum=="Employed"~"Wage employed",
                           stratum=="Self-employed"~"Self-employed",
                           stratum=="Student"~"Student",
                           stratum=="Non-employed"~"Non-employed"))%>%
  pivot_longer(stratum,names_to = "name",values_to = "value")
#disaggregating based on total
quality_overall<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by()%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm=TRUE),2))%>%
  mutate(name="Overall",value="Overall")

#disaggregating based on Agriculture/agribusiness
quality_Agriculture<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_1)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_1=ifelse(sector_choices_1==1,var_label(sector_choices_1),0))%>%
  filter(sector_choices_1!=0) %>%
  pivot_longer(sector_choices_1,names_to = "name",values_to = "value")

#disaggregating based on Tourism&Hospitality
quality_Tourism<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_2)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_2=ifelse(sector_choices_2==1,var_label(sector_choices_2),0))%>%
  filter(sector_choices_2!=0) %>%
  pivot_longer(sector_choices_2,names_to = "name",values_to = "value")

#disaggregating based on Creative industries
quality_creativeindustry<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_3)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_3=ifelse(sector_choices_3==1,var_label(sector_choices_3),0))%>%
  filter(sector_choices_3!=0) %>%
  pivot_longer(sector_choices_3,names_to = "name",values_to = "value")

#disaggregating based on Digital economy
quality_Dig_economy<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_4)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_4=ifelse(sector_choices_4==1,var_label(sector_choices_4),0))%>%
  filter(sector_choices_4!=0) %>%
  pivot_longer(sector_choices_4,names_to = "name",values_to = "value")

#disaggregating based on Education sector
quality_Education_sector<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_5)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_5=ifelse(sector_choices_5==1,var_label(sector_choices_5),0))%>%
  filter(sector_choices_5!=0) %>%
  pivot_longer(sector_choices_5,names_to = "name",values_to = "value")



df_quality_life_demo_seg_overall <- rbind(df_quality_life_demo,
                                          qual_life_stratum,
                                          quality_overall,
                                          quality_Agriculture,
                                          quality_Tourism,
                                          quality_creativeindustry,
                                          quality_Dig_economy,
                                          quality_Education_sector)%>%
  #select(value,average)%>%
  select(`Disaggregation groups`=value,name,`Quality of life index` = average)


x <- c("Overall" ,"Female","Male" ,"Refugee","PWD","Rural" ,"Urban" 
       ,"18-24" ,"25-29","30-35" ,"Fully employed","Self-employed",
       "Wage employed" ,"Non-employed","Student","Underemployed",
       "Unemployed","Non-seeker","Labor force","Non-employed",
       "0","1-20,000","20,000-40,000","40,000-60,000","60,000-80,000",
       "80,000-100,000","Above 100,000","None","Primary","Secondary","TVET",
       "Agriculture/agribusiness","Creative industries","Digital economy",
       "Education sector","Tourism&Hospitality")

df_quality_life_demo_seg_overall <- df_quality_life_demo_seg_overall%>%
  slice(match(x,`Disaggregation groups`))



#ANALYSIS B: step 2: proportion of individuals who report an average of 2 ------

# step 1: averaging services improvement (subquestion b related)

mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prop_great=case_when(round(avg_improv_quality_life)==2~1, TRUE~0))


#disaggregating the proportion of youth with improved qol by demographics

#disaggregating by stratum
prop_quality_stratum <- characterize(mcf_data_l5_t) %>%
  group_by(stratum,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_qual_life = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  pivot_longer(stratum,names_to = "name",values_to = "value")%>%
  select(-c("n","prop_great"))
#disaggregating based on geo entity
prop_quality_geo<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(geo_entity,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_qual_life = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  pivot_longer(geo_entity,names_to = "name",values_to = "value")%>%
  select(-c("n","prop_great"))

#disaggregating based on gender
prop_quality_gender<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(gender,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_qual_life = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  pivot_longer(gender,names_to = "name",values_to = "value")%>%
  select(-c("n","prop_great"))
prop_quality_gender$value<-gsub("^[a-zA-Z0-9]\\.\\s","",prop_quality_gender$value)
#disaggregating based on age group
prop_quality_agegroup<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(age_group,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_qual_life = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  pivot_longer(age_group,names_to = "name",values_to = "value")%>%
  select(-c("n","prop_great"))
#disaggregating based on pwd
prop_quality_pwd<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(pwd,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_qual_life = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  filter(pwd=="Yes")%>%
  mutate(pwd= ifelse(pwd=="Yes","PWD",NA))%>%
  pivot_longer(pwd,names_to = "name",values_to = "value")%>%
  select(-c("n","prop_great"))

#disaggregating based on refugee
prop_quality_refuge<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(refuge,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_qual_life = round(n*100/sum(n),2))%>%
  filter(prop_great==1&refuge=="b. Refugee")%>%
  pivot_longer(refuge,names_to = "name",values_to = "value")%>%
  select(-c("n","prop_great"))

prop_quality_overall<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_qual_life = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  mutate(name="Overall",value="Overall")%>%
  select(-c("n","prop_great"))

#disaggregating on income
prop_quality_income <- characterize(mcf_data_l5_t) %>%
  group_by(income_brkdwn, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1&income_brkdwn != "") %>%
  pivot_longer(income_brkdwn, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))
#disaggregating based on education
prop_quality_education <- characterize(mcf_data_l5_t) %>%
  dplyr::group_by(education_brkdwn, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(education_brkdwn, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))

#disaggregating based on underemployed
prop_quality_underemployed <- characterize(mcf_data_l5_t) %>%
  dplyr::group_by(labor_force_status_3, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(labor_force_status_3, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))%>%
  mutate(value=ifelse(value==1,var_label(value),0))%>%
  filter(value!=0)
  

#disaggregating by on fully employed
prop_quality_fullyemployed <- characterize(mcf_data_l5_t) %>%
  group_by(labor_force_status_4, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(labor_force_status_4, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))%>%
  mutate(value=ifelse(value==1,var_label(value),0))%>%
  filter(value!=0)

#disaggregating based on unemployed
prop_quality_unemployed <- characterize(mcf_data_l5_t) %>%
  dplyr::group_by(labor_force_status_5, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(labor_force_status_5, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))%>%
  mutate(value=ifelse(value==1,var_label(value),0))%>%
  filter(value!=0&!is.na(value))

#disaggregating based on Non-seeker
prop_quality_Non_seeker <- characterize(mcf_data_l5_t) %>%
  dplyr::group_by(labor_force_status_6, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(labor_force_status_6, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))%>%
  mutate(value=ifelse(value==1,var_label(value),0))%>%
  filter(value!=0&!is.na(value))

#disaggregating by on Labor force
prop_quality_Labor_force <- characterize(mcf_data_l5_t) %>%
  group_by(labor_force_status_7, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(labor_force_status_7, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))%>%
  mutate(value=ifelse(value==1,var_label(value),0))%>%
  filter(value!=0&!is.na(value))

#disaggregating based on Agriculture/agribusiness
prop_quality_Agriculture <- characterize(mcf_data_l5_t) %>%
  dplyr::group_by(sector_choices_1, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(sector_choices_1, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))

#disaggregating based on Tourism&Hospitality
prop_quality_Tourism <- characterize(mcf_data_l5_t) %>%
  dplyr::group_by(sector_choices_2, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(sector_choices_2, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))

#disaggregating based on Creative industries
prop_quality__creativeindustry <- characterize(mcf_data_l5_t) %>%
  dplyr::group_by(sector_choices_2, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(sector_choices_2, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))
#disaggregating based on Digital economy
prop_quality_Dig_economy <- characterize(mcf_data_l5_t) %>%
  dplyr::group_by(sector_choices_4, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(sector_choices_4, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))
#disaggregating based on Education sector
prop_quality_Education_sector <- characterize(mcf_data_l5_t) %>%
  dplyr::group_by(sector_choices_5, prop_great) %>%
  dplyr::summarize(n = sum(weights)) %>%
  mutate(propotional_qual_life = round(n * 100 / sum(n), 2)) %>%
  filter(prop_great == 1) %>%
  pivot_longer(sector_choices_5, names_to = "name", values_to = "value") %>%
  select(-c("n", "prop_great"))


df_proportional_quality_life_demo <-rbind(prop_quality_overall,prop_quality_refuge,
                                          prop_quality_gender,prop_quality_geo,
                                          prop_quality_agegroup,prop_quality_pwd,
                                          prop_quality_income,
                                          prop_quality_education,
                                          prop_quality_underemployed,
                                          prop_quality_fullyemployed,
                                          prop_quality_unemployed,
                                          prop_quality_Non_seeker,
                                          prop_quality_Labor_force,
                                          prop_quality_Agriculture,
                                          prop_quality_Tourism,
                                          prop_quality__creativeindustry,
                                          prop_quality_Dig_economy,
                                          prop_quality_Education_sector)%>%
  select(-name,value,propotional_qual_life)


#quality of life improvement score-------------



#disaggregating based on geo entity
improv_quality_life_geo<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(geo_entity)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_longer(geo_entity,names_to = "name",values_to = "value")

#disaggregating based on gender
improv_quality_life_gender<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(gender)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_longer(gender,names_to = "name",values_to = "value")
improv_quality_life_gender$value<-gsub("^[a-zA-Z0-9]\\.\\s","",improv_quality_life_gender$value)
#disaggregating based on age group
improv_quality_life_agegroup<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(age_group_brkdwn)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_longer(age_group_brkdwn,names_to = "name",values_to = "value")
#disaggregating based on pwd
improv_quality_life_pwd<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(pwd)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  filter(pwd=="Yes")%>%
  mutate(pwd= ifelse(pwd=="Yes","PWD",NA))%>%
  pivot_longer(pwd,names_to = "name",values_to = "value")
#disaggregating based on refugee
improv_quality_life_refuge<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(refugee_brkdwn)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  filter(refugee_brkdwn=="Refugee")%>%
  pivot_longer(refugee_brkdwn,names_to = "name",values_to = "value")

#disaggregating based on income
improv_quality_life_income<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(income_brkdwn)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  dplyr::filter(income_brkdwn!="")%>%
  pivot_longer(income_brkdwn,names_to = "name",values_to = "value")

#disaggregating based on education
improv_quality_life_education<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(education_brkdwn)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_longer(education_brkdwn,names_to = "name",values_to = "value")

#disaggregating based on underemployed
improv_quality_life_underemployed<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_3)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_3=ifelse(labor_force_status_3==1,var_label(labor_force_status_3),0))%>%
  filter(labor_force_status_3!=0) %>%
  pivot_longer(labor_force_status_3,names_to = "name",values_to = "value")

#disaggregating based on fully employed
improv_quality_life_fullyemployed<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_4)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_4=ifelse(labor_force_status_4==1,var_label(labor_force_status_4),0))%>%
  filter(labor_force_status_4!=0) %>%
  pivot_longer(labor_force_status_4,names_to = "name",values_to = "value")

#disaggregating based on unemployed
improv_quality_life_unemployed<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_5)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_5=ifelse(labor_force_status_5==1,var_label(labor_force_status_5),0))%>%
  filter(labor_force_status_5!=0) %>%
  pivot_longer(labor_force_status_5,names_to = "name",values_to = "value")

#disaggregating based on Non-seeker
improv_quality_life_Non_seeker<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_6)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_6=ifelse(labor_force_status_6==1,var_label(labor_force_status_6),0))%>%
  filter(labor_force_status_6!=0) %>%
  pivot_longer(labor_force_status_6,names_to = "name",values_to = "value")

#disaggregating based on Labor force
improv_quality_life_Labor_force<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(labor_force_status_7)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(labor_force_status_7=ifelse(labor_force_status_7==1,var_label(labor_force_status_7),0))%>%
  filter(labor_force_status_7!=0) %>%
  pivot_longer(labor_force_status_7,names_to = "name",values_to = "value")



df_improv_quality_life_life_demo <-rbind(improv_quality_life_gender,improv_quality_life_geo,
                                         improv_quality_life_agegroup,improv_quality_life_pwd,
                                         improv_quality_life_refuge,
                                         improv_quality_life_income,
                                         improv_quality_life_education,
                                         improv_quality_life_underemployed,
                                         improv_quality_life_fullyemployed,
                                         improv_quality_life_unemployed,
                                         improv_quality_life_Non_seeker,
                                         improv_quality_life_Labor_force)

#Disaggregation by stratum
improv_qual_life_stratum<-characterize(mcf_data_l5_t)%>%
  group_by(stratum)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm=TRUE),2))%>%
  mutate(stratum=case_when(stratum=="Employed"~"Wage employed",
                           stratum=="Self-employed"~"Self-employed",
                           stratum=="Student"~"Student",
                           stratum=="Non-employed"~"Non-employed"))%>%
  pivot_longer(stratum,names_to = "name",values_to = "value")
#disaggregating based on total
improv_quality_life_overall<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by()%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm=TRUE),2))%>%
  mutate(name="Overall",value="Overall")

#disaggregating based on Agriculture/agribusiness
improv_quality_life_Agriculture<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_1)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_1=ifelse(sector_choices_1==1,var_label(sector_choices_1),0))%>%
  filter(sector_choices_1!=0) %>%
  pivot_longer(sector_choices_1,names_to = "name",values_to = "value")

#disaggregating based on Tourism&Hospitality
improv_quality_life_Tourism<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_2)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_2=ifelse(sector_choices_2==1,var_label(sector_choices_2),0))%>%
  filter(sector_choices_2!=0) %>%
  pivot_longer(sector_choices_2,names_to = "name",values_to = "value")

#disaggregating based on Creative industries
improv_quality_life_creativeindustry<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_3)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_3=ifelse(sector_choices_3==1,var_label(sector_choices_3),0))%>%
  filter(sector_choices_3!=0) %>%
  pivot_longer(sector_choices_3,names_to = "name",values_to = "value")

#disaggregating based on Digital economy
improv_quality_life_Dig_economy<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_4)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_4=ifelse(sector_choices_4==1,var_label(sector_choices_4),0))%>%
  filter(sector_choices_4!=0) %>%
  pivot_longer(sector_choices_4,names_to = "name",values_to = "value")

#disaggregating based on Education sector
improv_quality_life_Education_sector<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(sector_choices_5)%>%
  dplyr::summarize(average=round(weighted.mean(avg_improv_quality_life, weights,na.rm = TRUE),2))%>%
  mutate(sector_choices_5=ifelse(sector_choices_5==1,var_label(sector_choices_5),0))%>%
  filter(sector_choices_5!=0) %>%
  pivot_longer(sector_choices_5,names_to = "name",values_to = "value")


df_improv_quality_life_overall <- rbind(df_improv_quality_life_life_demo,
                                        improv_qual_life_stratum,
                                        improv_quality_life_overall,
                                        improv_quality_life_Agriculture,
                                        improv_quality_life_Tourism,
                                        improv_quality_life_creativeindustry,
                                        improv_quality_life_Dig_economy,
                                        improv_quality_life_Education_sector)%>%
  #select(value,average)%>%
  select(`Disaggregation groups`=value,name,`Quality of life improvement score` = average)


x <- c("Overall" ,"Female","Male" ,"Refugee","PWD","Rural" ,"Urban" 
       ,"18-24" ,"25-29","30-35" ,"Fully employed","Self-employed",
       "Wage employed" ,"Non-employed","Student","Underemployed",
       "Unemployed","Non-seeker","Labor force","Non-employed",
       "0","1-20,000","20,000-40,000","40,000-60,000","60,000-80,000",
       "80,000-100,000","Above 100,000","None","Primary","Secondary","TVET",
       "Agriculture/agribusiness","Creative industries","Digital economy",
       "Education sector","Tourism&Hospitality")

df_improv_quality_life_overall <- df_improv_quality_life_overall%>%
  slice(match(x,`Disaggregation groups`))


#########################
qual_mean_w1 <- characterize(mcf_data_l5_t)%>%
  dplyr::group_by(main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))
#disaggregating based on geo entity
quality_geo<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(geo_entity,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "geo_entity",values_from = "average")%>%
  as.data.frame()
#disaggregating based on gender
quality_gender<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(gender,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "gender",values_from = "average")%>%
  as.data.frame()

#disaggregating based on age group
quality_agegroup<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(age_group,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "age_group",values_from = "average")%>%
  as.data.frame()

#disaggregating based on pwd
quality_pwd<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(pwd,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "pwd",values_from = "average")%>%
  rename(pwd_yes=Yes)%>%
  as.data.frame()

#disaggregating based on refugee
quality_refuge<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(refuge,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_wider(names_from = "refuge",values_from = "average")

overall1_qualindex_isic <- qual_mean_w1%>%
  left_join(quality_gender)%>%
  left_join(quality_geo)%>%
  left_join(quality_agegroup)%>%
  left_join(select(quality_pwd,-c("No")))%>%
  left_join(select(quality_refuge,-c("a. Non refuge")))%>%
  as.data.frame()%>%
  select(c("main_activity","average","b. Female","a. Male","b. Refugee","pwd_yes","Rural","Urban","18-24","25-35"))


prop_great_calc1 <- characterize(mcf_data_l5_t) %>%
  group_by(main_activity,prop_great)%>%
  dplyr::summarise(total=sum(weights))%>%
  mutate(propotional_great = total*100/sum(total))%>%
  select(-c(total))%>%
  pivot_wider(names_from = "prop_great",values_from = "propotional_great")%>%
  select(-No)%>%
  rename(Total_overall="Yes")

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




#disaggregating based on district----
quality_district<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(district_calc)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))

#disaggregating based on district and main activity
quality_main_district<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(district_calc,main_activity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights,na.rm = TRUE),2))%>%
  pivot_wider(names_from = "main_activity",values_from = "average")

#attach to combine
combined_district <- combined_district %>%
  left_join(quality_district)

combined_district <- rename(combined_district,District = district_calc,
                            `Ability score`=avg_ability_score,
                            `Expectation score`=avg_exp_score,
                            `Quality of Life index`=average)