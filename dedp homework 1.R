library(tidyverse)

papers <- as_tibble(read_csv("data/CitesforSara.csv"))
papers_select <- select(papers,journal,year,cites,title,au1)
nrow(filter(papers_select,cites>=100))

view(count(papers_select,journal,cites))
papers%>%
  group_by(journal)%>%
  summarise(cites_total=sum(cites,na.rm = TRUE))
