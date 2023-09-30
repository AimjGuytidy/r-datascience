# web scraping in R !!!
library(rvest)

# using selectorgadget and wayback machine we are going to scrape edx data from 
# 2020
edxweb <- read_html("https://web.archive.org/web/20200801210910/https://www.edx.org/subjects")
subjectshtml <- html_nodes(edxweb, ".mb-4+ .mb-4 .align-items-center")
subjecttext <- html_text(subjectshtml)
print(subjecttext)
