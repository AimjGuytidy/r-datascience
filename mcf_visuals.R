#load necessary libraries
library(dplyr)
library(officer)
library(ggplot2)
library(haven)
library(labelled)
library(rio)
library(stringr)
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
survey_data <- body_add_gg(survey_data, value = gender, style = "centered" )

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
survey_data <- body_add_gg(survey_data, value = mart_status, style = "centered" )
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



hheady<-characterize(mcf_data)%>%
  count(hhrelation,gender)
hheady<-hheady%>%
  mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)))
# sapply(hheady,function(x)gsub("^[a-z]+\\.\\s+","",x))
# hheady$hhrelation<-gsub("^[a-z0-9]+\\.\\s+","",hheady$hhrelation)
# hheady$gender<-gsub("^[a-z0-9]+\\.\\s+","",hheady$gender)
(hhead_relation <- hheady%>%
  ggplot(mapping=aes(n,hhrelation))+
  geom_bar(stat = "identity",aes(fill=gender))+
  #xlab("Marital Status ")+
  #ylab("Count")+
  ggtitle("relationship to head of household")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank())+
  scale_fill_manual(
    values=c(Blue,Light_blue),
    aesthetics = "fill"
  ) )
survey_data <- body_add_gg(survey_data, value = hhead_relation, style = "centered" )
#geom_text(aes(label=hhcount),hjust=-0.5)+
# scale_x_continuous(labels = function(x){paste0(x/1000, "k")}))


# create visuals for Gender of the household head

hhgender<-characterize(mcf_data)%>%
  count(hh_gender)%>%
  mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)))

(hhead_gender <- hhgender%>%
    ggplot(mapping=aes(hh_gender,n))+
    geom_bar(stat = "identity",fill=c(Blue,Light_blue))+
    xlab("")+
    ylab("")+
    ggtitle("Gender of the head of household")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(fill = c("#F2F2F2")),
          panel.background = element_rect(fill = c("#F2F2F2")),
          panel.grid = element_blank()))
survey_data <- body_add_gg(survey_data, value = hhead_gender, style = "centered" )
# create visuals for Gender of the household head

refugees<-characterize(mcf_data)%>%
  count(refuge)%>%
  mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)))

(refugees_plot <- refugees%>%
    ggplot(mapping=aes(refuge,n))+
    geom_bar(stat = "identity",fill=c(Blue,Light_blue))+
    xlab("")+
    ylab("")+
    ggtitle("refugee status")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(fill = c("#F2F2F2")),
          panel.background = element_rect(fill = c("#F2F2F2")),
          panel.grid = element_blank()))
survey_data <- body_add_gg(survey_data, value = refugees_plot, style = "centered" )
# create visuals for the seriousness of the conditions that respondents have

difficulty<-characterize(mcf_data)%>%
  count(difficulty_1,gender)%>%
  mutate_all(~replace(., is.na(.), "Not applicable"))%>%
  mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)),
         difficulty_1 = str_wrap(difficulty_1,width = 10))
  
  

(diff_plot <- difficulty%>%
    ggplot(mapping=aes(difficulty_1,as.integer(n)))+
    geom_bar(stat = "identity",position = "dodge",aes(fill=gender))+
    xlab("")+
    ylab("")+
    ggtitle("respondent conditions")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(fill = c("#F2F2F2")),
          panel.background = element_rect(fill = c("#F2F2F2")),
          panel.grid = element_blank())+
    scale_fill_manual(
      values=c(Blue,Light_blue),
      aesthetics = "fill"))
survey_data <- body_add_gg(survey_data, value = diff_plot, style = "centered" )
# create visuals for employment status

employment<-characterize(mcf_data)%>%
  count(employment,gender)%>%
  mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)),
         employment = str_wrap(employment,width = 10))



(emp_plot <- employment%>%
    ggplot(mapping=aes(employment,n))+
    geom_bar(stat = "identity",position = "dodge",aes(fill=gender))+
    xlab("")+
    ylab("")+
    ggtitle("Employment Status")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(fill = c("#F2F2F2")),
          panel.background = element_rect(fill = c("#F2F2F2")),
          panel.grid = element_blank())+
    scale_fill_manual(
      values=c(Blue,Light_blue),
      aesthetics = "fill"))
survey_data <- body_add_gg(survey_data, value = emp_plot, style = "centered" )
# create visuals for Farming status

farming<-characterize(mcf_data)%>%
  count(own_farming,gender)%>%
  mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)),
         own_farming = str_wrap(own_farming,width = 10))



(farm_plot <- farming%>%
    ggplot(mapping=aes(own_farming,n))+
    geom_bar(stat = "identity",position = "dodge",aes(fill=gender))+
    xlab("")+
    ylab("")+
    ggtitle(paste0("Did you do any farm work such as growing crops,","\nraising or tending animals, fishing, forestry"))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(fill = c("#F2F2F2")),
          panel.background = element_rect(fill = c("#F2F2F2")),
          panel.grid = element_blank())+
    scale_fill_manual(
      values=c(Blue,Light_blue),
      aesthetics = "fill"))
survey_data <- body_add_gg(survey_data, value = farm_plot, style = "centered" )
# create visuals for Farming sales status

farming_sale<-characterize(mcf_data)%>%
  count(sell_goods,gender)%>%
  mutate_all(~replace(., is.na(.), "Not applicable"))%>%
  mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)),
         sell_goods = str_wrap(sell_goods,width = 10),
         n=as.integer(n))



(farm_sale_plot <- farming_sale%>%
    ggplot(mapping=aes(sell_goods,n))+
    geom_bar(stat = "identity",position = "dodge",aes(fill=gender))+
    xlab("")+
    ylab("")+
    ggtitle(paste0("Did you do any farm work such as growing crops,","\nraising or tending animals, fishing, forestry"))+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(fill = c("#F2F2F2")),
          panel.background = element_rect(fill = c("#F2F2F2")),
          panel.grid = element_blank())+
    scale_fill_manual(
      values=c(Blue,Light_blue),
      aesthetics = "fill"))
survey_data <- body_add_gg(survey_data, value = farm_sale_plot, style = "centered" )
print(survey_data, target = "Visuals.docx")
