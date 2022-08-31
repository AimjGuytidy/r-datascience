#load necessary libraries
library(dplyr)
library(officer)
library(ggplot2)
library(haven)
library(labelled)
library(rio)
library(stringr)
library(sjmisc)
library(tidyr)
library(lubridate)
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
  dplyr::mutate(stratum=case_when(type_employ==1~1,
                           type_employ==2~2,
                           current_educ%notin%c(1, NA)&is.na(type_employ)~3,
                           current_educ%in%c(1, NA)&is.na(type_employ)~4))%>%
  set_value_labels(stratum=c(`Wage employed`=1,
                             `Self employed`=2,
                             `Students`=3,
                             ` Unemployed/Non job-seekers`=4))

#Checking the number in each stratum
stratum<-mcf_data%>%
  dplyr::group_by(stratum,geo_entity,gender)%>%
  dplyr::summarize(total=n())%>%
  pivot_wider(names_from = c(gender),values_from = c(total))%>%
  dplyr::rename(Male=`1`, Female=`2`)%>%
  dplyr::mutate(total=sum(Male, Female))

#Cell weighting 
total_pop<-read_excel('data/total_pop.xlsx')
total_pop<-total_pop%>%
  dplyr::mutate(stratum=case_when(stratum=="Wage employed"~1,
                           stratum=="Self employed"~2,
                           stratum=="Students"~3,
                           stratum=="Unemployed/Non-job seeker"~4),
         geo_entity=case_when(geo_entity=="Rural"~2,
                              geo_entity=="Urban"~1))
cell_weights<-left_join(total_pop, stratum)%>%
  dplyr::mutate(male_wt=`Total Male`/Male, female_wt=`Total Female`/Female)
cell_weights<-cell_weights%>%
  dplyr::select(stratum,geo_entity,female_wt, male_wt)
colnames(cell_weights)<-c('stratum','geo_entity','Female', 'Male')
cell_weights<-cell_weights%>%
  pivot_longer(cols=c('Female','Male'), names_to='gender1', values_to='cell_weights')%>%
  dplyr::mutate(gender=case_when(gender1=='Male'~1,
                          gender1=='Female'~2))%>%
  dplyr::select(-c(gender1))

#Add the weight column by joins  
mcf_data<- left_join(mcf_data,cell_weights, by=c("stratum","geo_entity","gender"))%>%
  move_columns(c(main_month, main_years),.before=inc_impro)




survey_data <- read_docx()

# create visuals for gender
gendr <- mcf_data%>%
  count(gender,wt = cell_weights)%>%
  dplyr::mutate(n = n/sum(n))
(gender <- ggplot2::ggplot(gendr,mapping=aes(factor(gender),n))+
  geom_bar(stat = "identity",fill=c("#097ABC","#9DC3E5"))+
  xlab("")+
  ylab("")+
  ggtitle("Gender")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(labels=c("Male", "Female"))+
  theme(plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid.major = element_blank(),
        panel.grid = element_blank())+
  scale_y_continuous(labels = scales::percent_format()))
survey_data <- body_add_gg(survey_data, value = gender, style = "centered" )

# create visuals for the distribution of age 
ager <- mcf_data%>%
  count(age,wt = cell_weights)%>%
  dplyr::mutate(n = n/sum(n),
                age = year(today())-age)

(age <- ggplot(ager,mapping = aes(age,n))+
  geom_bar(fill="#097ABC",stat = "identity")+
  ggtitle("Age Distribution Histogram plot")+
  xlab("")+
  ylab("")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid.major = element_blank(),
        panel.grid = element_blank())+
  scale_y_continuous(limits =c(0,0.1), labels = scales::percent_format())+
  scale_x_continuous(breaks=seq(18,35,2),labels=seq(18,35,2)))

age_distribution<-ggplot(mcf_data,mapping = aes(age))+
  geom_density(col="#097ABC")+
  ggtitle("Age Distribution Density plot")+
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank())

# create marital status visuals
mart_count<-characterize(mcf_data)%>%
  count(mart_status,wt=cell_weights)%>%
  ungroup()%>%
  mutate(n = n/sum(n))
mart_count$mart_status<-gsub("^[a-z0-9]+\\.\\s+","",mart_count$mart_status)
(mart_status <- ggplot(mart_count,mapping=aes(mart_status,n))+
  geom_bar(stat = "identity",fill=c(Blue,
                                    Light_blue,
                                    Dark_blue,
                                    Green,
                                    Dark_grey,
                                    Dark_green))+
  xlab("")+
  ylab("")+
  ggtitle("Marital Status")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank())+
  scale_y_continuous(limits =c(0,0.8), labels = scales::percent_format()))
survey_data <- body_add_gg(survey_data, value = mart_status, style = "centered" )
# create relationship to head of household visuals
hhead_count<-characterize(mcf_data)%>%
  count(hhrelation,wt=cell_weights)%>%
  ungroup()%>%
  mutate(n = n/sum(n))
hhead_count$hhrelation<-gsub("^[a-z0-9]+\\.\\s+","",hhead_count$hhrelation)
(hhead_relation <- hhead_count%>%
  ggplot(mapping=aes(n,hhrelation))+
  geom_bar(stat = "identity",fill=Blue)+
  xlab("")+
  ylab("")+
  ggtitle("relationship to head of household")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank())+
  scale_x_continuous(limits =c(0,0.6), labels = scales::percent_format()))


hheady<-characterize(mcf_data)%>%
  count(hhrelation,gender)
hheady<-hheady%>%
  mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)),
         n = n/sum(n))
# sapply(hheady,function(x)gsub("^[a-z]+\\.\\s+","",x))
# hheady$hhrelation<-gsub("^[a-z0-9]+\\.\\s+","",hheady$hhrelation)
# hheady$gender<-gsub("^[a-z0-9]+\\.\\s+","",hheady$gender)
(hhead_relation <- hheady%>%
  ggplot(mapping=aes(n,hhrelation))+
  geom_bar(stat = "identity",aes(fill=Blue),show.legend = FALSE)+
  xlab("")+
  ylab("")+
  ggtitle("relationship to head of household")+
  theme(plot.title = element_text(hjust = 0.5))+
  theme(plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank())+
  scale_fill_manual(
    values=c(Blue,Light_blue),
    aesthetics = "fill"
  )+
  scale_x_continuous(limits =c(0,0.5), labels = scales::percent_format()) )
survey_data <- body_add_gg(survey_data, value = hhead_relation, style = "centered" )
#geom_text(aes(label=hhcount),hjust=-0.5)+
# scale_x_continuous(labels = function(x){paste0(x/1000, "k")}))


# create visuals for Gender of the household head

hhgender<-characterize(mcf_data)%>%
  count(hh_gender,wt=cell_weights)%>%
  dplyr::mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)),
                n=n/sum(n))

(hhead_gender <- hhgender%>%
    ggplot(mapping=aes(hh_gender,n))+
    geom_bar(stat = "identity",fill=c(Blue,Light_blue),width=0.5)+
    #coord_fixed(ratio=2,)+
    xlab("")+
    ylab("")+
    ggtitle("Gender of the head of household")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(fill = c("#F2F2F2")),
          panel.background = element_rect(fill = c("#F2F2F2")),
          panel.grid = element_blank())+
  scale_y_continuous(limits =c(0,0.8), labels = scales::percent_format()))
survey_data <- body_add_gg(survey_data, value = hhead_gender, style = "centered" )
# create visuals for Gender of the household head

refugees<-characterize(mcf_data)%>%
  count(refuge,wt=cell_weights)%>%
  dplyr::mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)),
         n = n/sum(n))

(refugees_plot <- refugees%>%
    ggplot(mapping=aes(refuge,n))+
    geom_bar(stat = "identity",fill=c(Blue,Light_blue))+
    xlab("")+
    ylab("")+
    ggtitle("refugee status")+
    theme(plot.title = element_text(hjust = 0.5))+
    theme(plot.background = element_rect(fill = c("#F2F2F2")),
          panel.background = element_rect(fill = c("#F2F2F2")),
          panel.grid = element_blank())+
    scale_y_continuous(limits =c(0,1), labels = scales::percent_format()))
survey_data <- body_add_gg(survey_data, value = refugees_plot, style = "centered" )
# create visuals for the seriousness of the conditions that respondents have

difficulty<-characterize(mcf_data)%>%
  count(difficulty_1,gender,wt=cell_weights)%>%
  dplyr::mutate_all(~replace(., is.na(.), "Not applicable"))%>%
  dplyr::mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)),
         difficulty_1 = str_wrap(difficulty_1,width = 10),
         n = as.integer(n),
         n = n/sum(n))
  
  

(diff_plot <- difficulty%>%
    ggplot(mapping=aes(difficulty_1,n))+
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

# stacked 

difficulty1<-characterize(mcf_data)%>%
  group_by(difficulty_1,gender)%>%
  summarise(n=sum(cell_weights))%>%
  dplyr::mutate(n = n*100/sum(n))%>%
  ungroup()%>%
  #dplyr::select(difficulty_1,gender,cell_weights)%>%
  dplyr::mutate_all(~replace(., is.na(.), "Not applicable"))%>%
  dplyr::mutate(across(-n,function(x)gsub("^[a-z]+\\.\\s+","",x)),
                difficulty_1 = str_wrap(difficulty_1,width = 10),
                n=as.integer(n))



(
  diff_plot1 <- difficulty1 %>%
    ggplot(mapping = aes(difficulty_1, n)) +
    geom_bar(stat = "identity", position = "fill", aes(fill = gender)) +
    xlab("") +
    ylab("") +
    geom_text(
      aes(label = paste0(n, "%")),
      position = position_fill(vjust = 0.5),
      size = 3,
      color = "white"
    ) +
    ggtitle("respondent conditions") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      plot.background = element_rect(fill = c("#F2F2F2")),
      panel.background = element_rect(fill = c("#F2F2F2")),
      panel.grid = element_blank()
    ) +
    scale_fill_manual(values = c(Blue, Light_blue),
                      aesthetics = "fill")
)
survey_data <- body_add_gg(survey_data, value = diff_plot1, style = "centered" )

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
