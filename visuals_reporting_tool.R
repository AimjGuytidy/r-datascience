library(dplyr)
library(tidyr)
library(readr)
library(openxlsx)
library(haven)
library(foreign)
library(labelled)
library(rio)
library(psych)
library(ggplot2)
library(stringr)
library(forcats)
Light_grey <- c("#F2F2F2") #Light grey for the background
Blue <- c("#097ABC") #Blue
Light_blue <- c("#9DC3E5") #Light blue
Dark_blue <- c("#1b2f55") #Dark blue
Green <- c("#8ccc98") #Green
Dark_grey <- c("#7F7F7F") #Dark grey
Dark_green <- c("#49711E") #Dark green

#Visualizing L5.1.2b to get the overall view
mcf_data <- read_sav("data/mcf_data_new_reporting_tool.sav")
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

col_name1 <- var_label(mcf_data_k$work_trainings)
col_name1 <- gsub("^[a-zA-Z0-9]+\\.\\s","",col_name1)
col_name2 <- var_label(mcf_data_k$training_jb_market)
col_name2 <- gsub("^[a-zA-Z0-9]+\\.\\s","",col_name2)
df_1 <- tibble(work=c(col_name1,col_name2),value=c(weighted.mean(mcf_data_k$work_trainings_t,mcf_data_k$weights,na.rm=TRUE),
                                                   weighted.mean(mcf_data_k$training_jb_market_t,mcf_data_k$weights,na.rm=TRUE)))
#View(tibble("{col_name}":=weighted.mean(mcf_data_k$work_trainings_t,mcf_data_k$weights,na.rm=TRUE))

#Visuals of determinants of ability
ggplot(data=df_1,mapping = aes(str_wrap(work,32),value))+
  geom_bar(stat = "identity",fill=Blue)+
  geom_text(aes(label=paste0(round(value,2))),
            vjust=-.5,
            size = 4)+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )

#Visuals of average ability score across demographics
#average ability score 
avg_ability<-mcf_data_k%>%
  group_by()%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))
#Disaggregation by gender 
avg_ability_gender<-characterize(mcf_data_k)%>%
  group_by(gender)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))%>%
  pivot_longer(gender,names_to = "name",values_to = "value")
avg_ability_gender$value<-gsub("^[a-zA-Z0-9]\\.\\s","",avg_ability_gender$value)
#Disaggregation by geoentity 
avg_ability_geoentity<-characterize(mcf_data_k)%>%
  group_by(geo_entity)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))%>%
  pivot_longer(geo_entity,names_to = "name",values_to = "value")
#Disaggregation by age group 
avg_ability_agegroup<-characterize(mcf_data_k)%>%
  group_by(age_group)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))%>%
  pivot_longer(age_group,names_to = "name",values_to = "value")

df_ability_demo <-rbind(avg_ability_gender,avg_ability_geoentity,avg_ability_agegroup)

ggplot(df_ability_demo,aes(value,avg_ability_score))+
  geom_bar(stat = "identity",fill=Blue)+
  geom_text(aes(label=paste0(round(avg_ability_score,2))),
            vjust=-.5,
            size = 3.3)+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )



#Disaggregation by stratum
avg_ability_stratum<-characterize(mcf_data_k)%>%
  group_by(stratum)%>%
  dplyr::summarize(avg_ability_score=round(weighted.mean(ability_score, weights,na.rm=TRUE),2))%>%
  pivot_longer(stratum,names_to = "name",values_to = "value")

ggplot(avg_ability_stratum,aes(str_wrap(value,15),avg_ability_score))+
  geom_bar(stat = "identity",fill=Blue,aes(group=value))+
  geom_text(aes(label=paste0(round(avg_ability_score,2))),
            vjust=-.5,
            size = 3.3)+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )






########################################## 
#Visualizing L5.1.2c

mcf_data <- read_sav("data/mcf_data_new_reporting_tool.sav")
keyword_label_c<-c("J1.",	"J2.",	"J3.",	"J4.")
variables_for_l512_c <- mcf_data%>%look_for(keyword_label_c)
variables_for_l512_c <-variables_for_l512_c[,"variable"]
var_df_c <- as.data.frame(variables_for_l512_c)
var_df_c <- var_df_c[1:4,]

#filtering out unemployed and students
mcf_data <- mcf_data%>%
  filter(stratum==1|stratum==2)%>%
  mutate(expectation_score = rowMeans(select(.,all_of(var_df_c)),na.rm = TRUE),
         exp_score_prop = ifelse(round(expectation_score)>=4,1,0))

col_name3<- var_label(mcf_data[[var_df_c[1]]])
col_name3 <- gsub("^[a-zA-Z0-9]+\\.\\s","",col_name3)
col_name4<- var_label(mcf_data[[var_df_c[2]]])
col_name4 <- gsub("^[a-zA-Z0-9]+\\.\\s","",col_name4)
col_name5<- var_label(mcf_data[[var_df_c[3]]])
col_name5 <- gsub("^[a-zA-Z0-9]+\\.\\s","",col_name5)
col_name6<- var_label(mcf_data[[var_df_c[4]]])
col_name6 <- gsub("^[a-zA-Z0-9]+\\.\\s","",col_name6)

df_2 <- tibble(expectation=c(col_name3,col_name4,col_name5,col_name6),value=c(weighted.mean(mcf_data[[var_df_c[1]]],mcf_data$weights,na.rm=TRUE),
                                                   weighted.mean(mcf_data[[var_df_c[2]]],mcf_data$weights,na.rm=TRUE),weighted.mean(mcf_data[[var_df_c[3]]],mcf_data$weights,na.rm=TRUE),
                                                   weighted.mean(mcf_data[[var_df_c[4]]],mcf_data$weights,na.rm=TRUE)))

#visuals of determinants of expectations
ggplot(data=df_2,mapping = aes(str_wrap(expectation,32),value))+
  geom_bar(stat = "identity",fill=Blue)+
  coord_flip()+
  geom_text(aes(label=paste0(round(value,2))),
            hjust=-.2,
            size = 3)+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),#remove y axis ticks
    axis.text.x = element_blank()
  )
#Visuals of average expectation score across demographics
#average expectation score 
avg_expectation_total<-characterize(mcf_data)%>%
  group_by()%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))
#Disaggregation by gender 
avg_expectation_gender<-characterize(mcf_data)%>%
  group_by(gender)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))%>%
  pivot_longer(gender,names_to = "name",values_to = "value")
avg_expectation_gender$value<-gsub("^[a-zA-Z0-9]\\.\\s","",avg_expectation_gender$value)
#Disaggregation by geo entity 
avg_expectation_geoentity<-characterize(mcf_data)%>%
  group_by(geo_entity)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))%>%
  pivot_longer(geo_entity,names_to = "name",values_to = "value")
#Disaggregation by age group 
avg_expectation_agegroup<-characterize(mcf_data)%>%
  group_by(age_group)%>%
  dplyr::summarize(avg_exp_score=round(weighted.mean(expectation_score, weights,na.rm=TRUE),2))%>%
  pivot_longer(age_group,names_to = "name",values_to = "value")

df_expectation_demo <-rbind(avg_expectation_gender,avg_expectation_geoentity,avg_expectation_agegroup)

ggplot(df_expectation_demo,aes(value,avg_exp_score))+
  geom_bar(stat = "identity",fill=Blue)+
  geom_text(aes(label=paste0(round(avg_exp_score,2))),
            vjust=-.5,
            size = 3.3)+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )


#####################################################################
#####################################################################
#Visualizing the quality of life index

mcf_data <- read_sav("data/mcf_data_new_reporting_tool.sav")
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

#disaggregating based on geo entity
quality_geo<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(geo_entity)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_longer(geo_entity,names_to = "name",values_to = "value")

#disaggregating based on gender
quality_gender<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(gender)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_longer(gender,names_to = "name",values_to = "value")
quality_gender$value<-gsub("^[a-zA-Z0-9]\\.\\s","",quality_gender$value)
#disaggregating based on age group
quality_agegroup<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(age_group)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  pivot_longer(age_group,names_to = "name",values_to = "value")
#disaggregating based on pwd
quality_pwd<-characterize(mcf_data_l5_t)%>%
  dplyr::group_by(pwd)%>%
  dplyr::summarize(average=round(weighted.mean(perc_quality_life, weights),2))%>%
  filter(pwd=="Yes")%>%
  mutate(pwd= ifelse(pwd=="Yes","PWD",NA))%>%
  pivot_longer(pwd,names_to = "name",values_to = "value")

df_quality_life_demo <-rbind(quality_gender,quality_geo,quality_agegroup,quality_pwd)

ggplot(df_quality_life_demo,aes(value,average))+
  geom_bar(stat = "identity",fill=Blue)+
  geom_text(aes(label=paste0(round(average,2))),
            vjust=-.5,
            size = 3.3)+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )


#Disaggregation by stratum
qual_life_stratum<-characterize(mcf_data_l5_t)%>%
  group_by(stratum)%>%
  dplyr::summarize(avg_qual_life=round(weighted.mean(perc_quality_life, weights,na.rm=TRUE),2))%>%
  pivot_longer(stratum,names_to = "name",values_to = "value")

ggplot(qual_life_stratum,aes(str_wrap(value,15),avg_qual_life))+
  geom_bar(stat = "identity",fill=Blue,aes(group=value))+
  geom_text(aes(label=paste0(round(avg_qual_life,2))),
            vjust=-.5,
            size = 3.3)+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )

#Proportion of youth reporting 
mcf_data_l5_t<-mcf_data_l5_t%>%
  mutate(prop_great=case_when(round(avg_improv_quality_life)==2~1, TRUE~0))

#disaggregating by stratum
prop_quality_stratum <- characterize(mcf_data_l5_t) %>%
  group_by(stratum,prop_great)%>%
  dplyr::summarize(n=sum(weights))%>%
  mutate(propotional_qual_life = round(n*100/sum(n),2))%>%
  filter(prop_great==1)%>%
  select(-c("n","prop_great"))

ggplot(prop_quality_stratum,aes(str_wrap(stratum,15),propotional_qual_life))+
  geom_bar(stat = "identity",fill=Blue)+
  geom_text(aes(label=paste0(round(propotional_qual_life,2),"%")),
            vjust=-.5,
            size = 3.3)+
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(
    plot.background = element_rect(fill = c("#F2F2F2")),
    panel.background = element_rect(fill = c("#F2F2F2")),
    panel.grid = element_blank(),
    #remove x axis ticks
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    #remove y axis labels
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()#remove y axis ticks
  )

#disaggregating by demographics

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



df_proportional_quality_life_demo <-rbind(prop_quality_gender,prop_quality_geo,prop_quality_agegroup,prop_quality_pwd)






