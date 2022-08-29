#load necessary libraries
library(dplyr)
library(officer)
library(ggplot2)
library(haven)
library(labelled)
library(rio)
Light_grey<-c("#F2F2F2") #Light grey for the background
Blue<-c("#097ABC") #Blue
Light_blue<-c("#9DC3E5") #Light blue
Dark_blue<-c("#1b2f55") #Dark blue
Green<-c("#8ccc98") #Green
Dark_grey<-c("#7F7F7F") #Dark grey
Dark_green<-c("#49711E") #Dark green

#load data
mcf_data <- read_dta(paste0(
  "G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/",
  "4. QUANT/1. Data collection/Data management/Clean datasets/Main clean dataset/",
  "Final dataset/MCF_Baseline_Main data - Clean.dta"))

survey_data <- read_docx()

# create visuals for gender
gender <- ggplot2::ggplot(mcf_data,mapping=aes(factor(gender)))+
  geom_bar(stat = "count",fill=c("#097ABC","#9DC3E5"))+
  xlab("Gender")+
  ylab("Count")+
  ggtitle("Gender")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c("Male", "Female"))+
  theme(plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")))

# create visuals for the distribution of age 
age <- ggplot(mcf_data,mapping = aes(age))+
  geom_bar(fill="#097ABC")+
  ggtitle("Age Distribution Histogram plot")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid.major = element_blank(),
        panel.grid = element_blank() )

age_distribution<-ggplot(mcf_data,mapping = aes(age))+
  geom_density(col="#097ABC")+
  ggtitle("Age Distribution Density plot")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank())

# create marital status visuals
mart_count<-characterize(mcf_data)%>%
  count(mart_status)%>%
  ungroup()
mart_count$mart_status<-gsub("^[a-z0-9]+\\.\\s+","",mart_count$mart_status)
mart_status <- ggplot(mart_count,mapping=aes(mart_status,n))+
  geom_bar(stat = "identity",fill=c(Blue,
                                    Light_blue,
                                    Dark_blue,
                                    Green,
                                    Dark_grey,
                                    Dark_green))+
  xlab("Marital Status ")+
  ylab("Count")+
  ggtitle("Marital Status")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank())

# create relationship to head of household visuals
hhead_count<-characterize(mcf_data)%>%
  count(hhrelation)%>%
  ungroup()
hhead_count$hhrelation<-gsub("^[a-z0-9]+\\.\\s+","",hhead_count$hhrelation)
hhead_relation <- hhead_count%>%
  ggplot(mapping=aes(n,hhrelation))+
  geom_bar(stat = "identity",fill=Blue)+
  #xlab("Marital Status ")+
  #ylab("Count")+
  ggtitle("relationship to head of household")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank())
