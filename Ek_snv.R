# gender
# male
# female
#
library(tidyverse)
library(haven)
library(labelled)
library(officer)
#install.packages("Rtools")
Dark_blue <- c("#1b2f55") #Dark blue
snv_data <- read_dta("data/SNV HortInvest Evaluation_preclean.dta")

copestrategies <- snv_data %>%
  select (matches("^copestrategies_[1-9][0-9]*"), -copestrategies_98) %>%
  pivot_longer(cols = everything(), names_to = "snv", values_to = "selected") %>%
  #filter(selected == 1) %>%
  mutate(snv = sapply(snv, function(snv) var_label(snv_data[[snv]])),
         snv = sapply(snv, function(snv) paste(strwrap(snv, 40), collapse = "\n")))
duda <- copestrategies%>%
  count(snv,selected)%>%
  mutate(percy = as.integer(n*100/60,1),
         selecty = ifelse(selected==1,"Selected","Not Selected"))%>%
  filter(selected==1)%>%
  arrange(percy)
(duda%>%
    ggplot(aes(forcats::fct_inorder(snv),percy))+
    geom_bar(aes(fill=selecty,group=snv),stat = "identity",position = "dodge") +
    geom_text(
      aes(label = paste0(percy, "%")),
      vjust = 0.5,
      hjust=-.2,
      size = 4,
      color = Dark_blue
    )+
    theme(legend.position="none")+
    xlab("")+
    ylab("")+
    coord_flip()+
    #scale_color_binned(guide = guide_coloursteps(ticks = TRUE))+
    theme(axis.text.y = element_text(hjust =  1))+
    scale_y_continuous(expand=expansion(mult=c(0,0.1)))+
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(
      plot.background = element_rect(fill = c("#F2F2F2")),
      panel.background = element_rect(fill = c("#F2F2F2")),
      panel.grid = element_blank(),
      axis.ticks.y = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    )+
    scale_fill_manual(values = c(rgb(8,102,52,maxColorValue = 255),rgb(48,22,32,maxColorValue = 255)),
                      aesthetics = "fill"))
