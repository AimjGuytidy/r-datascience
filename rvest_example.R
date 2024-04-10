rm(list = ls())

library(rvest)
library(tidyverse)

edxsubjects <- read_html("https://web.archive.org/web/20200801210910/https://www.edx.org/subjects")
subjectslist <- html_elements(edxsubjects,xpath = "//li[@class = 'subject-card mb-3']")
subjectstext <- html_text2(subjectslist)


subjectshtml<-html_nodes(edxsubjects, ".align-items-center")
subjecttext<-html_text(subjectshtml)
print(subjecttext)