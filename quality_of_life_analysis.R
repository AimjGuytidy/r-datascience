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
library(data.table)
Light_grey <- c("#F2F2F2") #Light grey for the background
Blue <- c("#097ABC") #Blue
Light_blue <- c("#9DC3E5") #Light blue
Dark_blue <- c("#1b2f55") #Dark blue
Green <- c("#8ccc98") #Green
Dark_grey <- c("#7F7F7F") #Dark grey
Dark_green <- c("#49711E") #Dark green

mcf_data <- read_sav("data/mcf_data_new_reporting_tool.sav")
mcf_data_l5_t <- mcf_data

keyword_label<-c("D14",	"D16",	"D18",	"D20",	"D22",	"D24",	"D26",	"D28",	"D30",	"D32",	"D34",	"D36")
variables_for_l531_a <- mcf_data_l5_t%>%look_for(keyword_label)
variables_for_l531_a <-variables_for_l531_a[,"variable"]
var_df <- as.data.frame(variables_for_l531_a)
services_access <- grep("_access$", var_df$variable,value=TRUE, ignore.case =T)


for (i in 1:length(services_access) ){
  mcf_data_l5_t[,services_access[i]][mcf_data_l5_t[,services_access[i]]==1]<-0
  mcf_data_l5_t[,services_access[i]][mcf_data_l5_t[,services_access[i]]==2]<-1
  mcf_data_l5_t[,services_access[i]][mcf_data_l5_t[,services_access[i]]==3]<-2
  mcf_data_l5_t[,services_access[i]][mcf_data_l5_t[,services_access[i]]==4]<-3
  mcf_data_l5_t[,services_access[i]][mcf_data_l5_t[,services_access[i]]==5]<-4
}

#filter services that had been accessed to easily
# mcf_data_l5_tt <- mcf_data_l5_t%>%
#   filter(if_all(services_access,~.>=2))

for (i in services_access){
  assign(paste0(i,"_possess_gender"),
         characterize(mcf_data_l5_t%>%
           mutate(prop_great=case_when(!!as.name(i)>=2~1, TRUE~0))%>%
           group_by(gender,prop_great)%>%
             dplyr::summarize(n=sum(weights))%>%
             mutate(!!gsub("_access$","",i) := round(n*100/sum(n),2))%>%
             filter(prop_great==1)%>%
             select(-n)%>%
             rename(name=gender)))
         }
  
for (i in services_access){
  assign(paste0(i,"_possess_geo"),
         characterize(mcf_data_l5_t%>%
                        mutate(prop_great=case_when(!!as.name(i)>=2~1, TRUE~0))%>%
                        group_by(geo_entity,prop_great)%>%
                        dplyr::summarize(n=sum(weights))%>%
                        mutate(!!gsub("_access$","",i) := round(n*100/sum(n),2))%>%
                        filter(prop_great==1)%>%
                        select(-n)%>%
                      rename(name=geo_entity)))
}

for (i in services_access){
  assign(paste0(i,"_possess_age"),
         characterize(mcf_data_l5_t%>%
                        mutate(prop_great=case_when(!!as.name(i)>=2~1, TRUE~0))%>%
                        group_by(age_group,prop_great)%>%
                        dplyr::summarize(n=sum(weights))%>%
                        mutate(!!gsub("_access$","",i) := round(n*100/sum(n),2))%>%
                        filter(prop_great==1)%>%
                        select(-n)%>%
                        rename(name=age_group)))
    
}

for (i in services_access){
  assign(paste0(i,"_possess_stratum"),
         characterize(mcf_data_l5_t%>%
                        mutate(prop_great=case_when(!!as.name(i)>=2~1, TRUE~0))%>%
                        group_by(stratum,prop_great)%>%
                        dplyr::summarize(n=sum(weights))%>%
                        mutate(!!gsub("_access$","",i) := round(n*100/sum(n),2))%>%
                        filter(prop_great==1)%>%
                        select(-n)%>%
                        rename(name=stratum)))
  
}

for (i in services_access){
  assign(paste0(i,"_possess_overall"),
         characterize(mcf_data_l5_t%>%
                        mutate(prop_great=case_when(!!as.name(i)>=2~1, TRUE~0))%>%
                        group_by(prop_great)%>%
                        dplyr::summarize(n=sum(weights))%>%
                        mutate(!!gsub("_access$","",i) := round(n*100/sum(n),2),
                               name="Overall")%>%
                        select(name,prop_great,!!gsub("_access$","",i))%>%
                        filter(prop_great==1)))
}

bank_account_total <-rbind(bank_account_access_possess_overall,
                           bank_account_access_possess_gender,
                           bank_account_access_possess_geo,
                           bank_account_access_possess_age,
                           bank_account_access_possess_stratum)

clean_water_total <-rbind(clean_water_access_possess_overall,
                          clean_water_access_possess_gender,
                          clean_water_access_possess_geo,
                          clean_water_access_possess_age,
                          clean_water_access_possess_stratum)

electricity_total <-rbind(electricity_access_possess_overall,
                          electricity_access_possess_gender,
                          electricity_access_possess_geo,
                          electricity_access_possess_age,
                          electricity_access_possess_stratum)

food_total <-rbind(food_access_possess_overall,
                   food_access_possess_gender,
                   food_access_possess_geo,
                   food_access_possess_age,
                   food_access_possess_stratum)

healthcare_total <-rbind(healthcare_access_possess_overall,
                         healthcare_access_possess_gender,
                         healthcare_access_possess_geo,
                         healthcare_access_possess_age,
                         healthcare_access_possess_stratum)

internet_total <-rbind(internet_access_possess_overall,
                       internet_access_possess_gender,
                       internet_access_possess_geo,
                       internet_access_possess_age,
                       internet_access_possess_stratum)

loans_total <-rbind(loans_access_possess_overall,
                           loans_access_possess_gender,
                           loans_access_possess_geo,
                           loans_access_possess_age,
                    loans_access_possess_stratum)

nature_total <-rbind(nature_access_possess_overall,
                           nature_access_possess_gender,
                           nature_access_possess_geo,
                           nature_access_possess_age,
                     nature_access_possess_stratum)

roads_total <-rbind(roads_access_possess_overall,
                           roads_access_possess_gender,
                           roads_access_possess_geo,
                           roads_access_possess_age,
                    roads_access_possess_stratum)

sanitation_total <-rbind(sanitation_access_possess_overall,
                           sanitation_access_possess_gender,
                           sanitation_access_possess_geo,
                           sanitation_access_possess_age,
                         sanitation_access_possess_stratum)

telephone_total <-rbind(telephone_access_possess_overall,
                           telephone_access_possess_gender,
                           telephone_access_possess_geo,
                           telephone_access_possess_age,
                        telephone_access_possess_stratum)

bank_account_total <-rbind(bank_account_access_possess_overall,
                           bank_account_access_possess_gender,
                           bank_account_access_possess_geo,
                           bank_account_access_possess_age,
                           bank_account_access_possess_stratum)

bank_account_total <-rbind(bank_account_access_possess_overall,
                           bank_account_access_possess_gender,
                           bank_account_access_possess_geo,
                           bank_account_access_possess_age,
                           bank_account_access_possess_stratum)
