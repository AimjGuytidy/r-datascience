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
library(readxl)
library(RColorBrewer)
library(vtable)
library(openxlsx)

Light_grey <- c("#F2F2F2") #Light grey for the background
Blue <- c("#097ABC") #Blue
Light_blue <- c("#9DC3E5") #Light blue
Dark_blue <- c("#1b2f55") #Dark blue
Green <- c("#8ccc98") #Green
Dark_grey <- c("#7F7F7F") #Dark grey
Dark_green <- c("#49711E") #Dark green

mcf_recall <- read_dta("G:/Shared drives/MCF Baseline - external baseline/4. Baseline assessment/4. QUANT/1. Data collection/MCF RECALL/Data management/Clean datasets/Pilot clean dataset/MCF Recall Survey PILOT.dta") 
mcf_recall<-characterize(mcf_recall)
mcf_recall <- mcf_recall%>%
  mutate(gender=gender_calc,
         cell_weights = 0)
# let's create a function to help us automate some operations

preproc_stack <- function(data_prep,i,title_prep="no title provided") {
  temp <- characterize(data_prep) %>%
    rename(temp1 = as.factor(i)) %>%
    group_by(temp1, gender) %>%
    summarise(n = sum(cell_weights)) %>%
    dplyr::mutate(n = n * 100 / sum(n)) %>%
    ungroup() %>%
    dplyr::mutate_all( ~ replace(., is.na(.), "Not applicable")) %>%
    dplyr::mutate(across(-n, function(x)
      gsub("^[a-z]+\\.\\s*", "", x)),
      temp1 = str_wrap(temp1, width = 10),
      n = as.integer(n))
  if (nrow(temp) > 6){
    temp <- characterize(data_prep) %>%
      rename(temp1 = as.factor(i)) %>%
      group_by(temp1, gender) %>%
      summarise(n = sum(cell_weights)) %>%
      dplyr::mutate(n = n * 100 / sum(n)) %>%
      ungroup() %>%
      dplyr::mutate_all( ~ replace(., is.na(.), "Not applicable")) %>%
      dplyr::mutate(across(-n, function(x)
        gsub("^[a-z]+\\.\\s*", "", x)),
        temp1 = str_wrap(temp1, width = 28),
        n = as.integer(n))
    
    temp3<-
      temp %>%
      ggplot(mapping = aes(temp1, n)) +
      geom_bar(stat = "identity", position = "fill", aes(fill =
                                                           gender)) +
      xlab("") +
      ylab("") +
      geom_text(
        aes(label = paste0(n, "%")),
        position = position_fill(vjust = 0.5),
        size = 2,
        color = "white"
      ) +
      ggtitle(title_prep)+
      coord_flip()+
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(
        plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank(),
        #remove x axis ticks
        axis.text.x = element_blank(),
        #remove y axis labels
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank()#remove y axis ticks
      ) +
      scale_fill_manual(values = c(Blue, Light_blue),
                        aesthetics = "fill")+
      #scale_y_discrete(guide = guide_axis(n.dodge=2))+
      scale_y_continuous(expand=expansion(mult=c(0,0.1)))
    survey_data <-
      body_add_gg(survey_data, value = temp3, style = "centered",height = 10)
    return(temp3)
    
  }else{
    
    temp3<-
      temp %>%
      ggplot(mapping = aes(temp1, n)) +
      geom_bar(stat = "identity", position = "fill", aes(fill =
                                                           gender)) +
      xlab("") +
      ylab("") +
      geom_text(
        aes(label = paste0(n, "%")),
        position = position_fill(vjust = 0.5),
        size = 3,
        color = "white"
      ) +
      ggtitle(title_prep) +
      theme(plot.title = element_text(hjust = 0.5)) +
      theme(
        plot.background = element_rect(fill = c("#F2F2F2")),
        panel.background = element_rect(fill = c("#F2F2F2")),
        panel.grid = element_blank(),
        #remove x axis ticks
        axis.text.y = element_blank(),
        #remove y axis labels
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank()#remove y axis ticks
      ) +
      scale_fill_manual(values = c(Blue, Light_blue),
                        aesthetics = "fill")
    survey_data <-
      body_add_gg(survey_data, value = temp3, style = "centered")
    return(temp3)
    
  }
}

preproc_dodge <- function(data_prep,i,title_prep="no title provided") {
  temp <- characterize(data_prep) %>%
    rename(temp1 = as.factor(i)) %>%
    count(temp1, gender, wt = cell_weights) %>%
    dplyr::mutate_all(~ replace(., is.na(.), "Not applicable")) %>%
    dplyr::mutate(
      across(-n, function(x)
        gsub("^[a-z]+\\.\\s*", "", x)),
      temp1 = str_wrap(temp1, width = 10),
      n = as.integer(n),
      n = n / sum(n)
    )
  if (nrow(temp)>6){
    temp <- characterize(data_prep) %>%
      rename(temp1 = as.factor(i)) %>%
      count(temp1, gender, wt = cell_weights) %>%
      dplyr::mutate_all(~ replace(., is.na(.), "Not applicable")) %>%
      dplyr::mutate(
        across(-n, function(x)
          gsub("^[a-z]+\\.\\s*", "", x)),
        temp1 = str_wrap(temp1, width = 28),
        n = as.integer(n),
        n = n / sum(n)
      )
    temp3<-
      (temp %>%
         ggplot(mapping = aes(temp1, n)) +
         geom_bar(stat = "identity", position = "dodge", aes(fill = gender)) +
         xlab("") +
         ylab("") +
         ggtitle(title_prep) +
         coord_flip()+
         theme(plot.title = element_text(hjust = 0.5)) +
         theme(
           plot.background = element_rect(fill = c("#F2F2F2")),
           panel.background = element_rect(fill = c("#F2F2F2")),
           panel.grid = element_blank()
         ) +
         scale_fill_manual(values = c(Blue, Light_blue),
                           aesthetics = "fill") +
         scale_y_continuous(
           limits = c(0, (max(temp$n,na.rm = TRUE) + .1)),
           labels = scales::percent_format()
         ))
    survey_data <-
      body_add_gg(survey_data, value = temp3, style = "centered",height = 10)
    return(temp3)
  }else{
    temp3<-
      (temp %>%
         ggplot(mapping = aes(temp1, n)) +
         geom_bar(stat = "identity", position = "dodge", aes(fill = gender)) +
         xlab("") +
         ylab("") +
         ggtitle(title_prep) +
         theme(plot.title = element_text(hjust = 0.5)) +
         theme(
           plot.background = element_rect(fill = c("#F2F2F2")),
           panel.background = element_rect(fill = c("#F2F2F2")),
           panel.grid = element_blank()
         ) +
         scale_fill_manual(values = c(Blue, Light_blue),
                           aesthetics = "fill") +
         scale_y_continuous(
           limits = c(0, (max(temp$n,na.rm = TRUE) + .1)),
           labels = scales::percent_format()
         ))
    survey_data <-
      body_add_gg(survey_data, value = temp3, style = "centered")
    return(temp3)
  }
}

survey_data <- read_docx()
# respondents conditions visuals

mcf_bank <- characterize(mcf_recall)%>%
  select(matches("^have_bank_account_r_[0-9]+$"),gender,cell_weights)%>%
  mutate(across(matches("^have_bank_account_r_[0-9]+$"),~ifelse(.x==1,"Yes","No")))

for (i in colnames(mcf_bank%>%
                   select(matches("^have_bank_account_r_[0-9]+$")))){
  assign(paste(i,"_dodge_graph"),preproc_dodge(mcf_bank,i,title_prep = paste0("Respondents Conditions:",var_label(mcf_bank[c(i)]))))
  assign(paste(i,"_stack_graph"),preproc_stack(mcf_bank,i,title_prep = paste0("Respondents Conditions:",var_label(mcf_bank[c(i)]))))
}
survey_data <- read_docx()
(tab1 <- sumtable(mcf_recall,vars="out_vila_r_1",out = "csv",file = "summary_stats.csv"))
tab2 <- read.csv("summary_stats.csv")
k <-as.data.frame(st(mcf_recall,vars="out_vila_r_1",out = "return"))
write.xlsx(k,"k.xlsx")
dk <- read_excel("k.xlsx")
survey_data <- body_add_table(survey_data,value = dk)
print(survey_data,"imama.docx")

sink("trialsink.tex")
st(mcf_recall,vars="out_vila_r_1",out = "latex")
sink()

read.tex