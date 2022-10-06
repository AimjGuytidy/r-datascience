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

