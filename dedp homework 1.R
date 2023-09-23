library(tidyverse)

papers <- as_tibble(read_csv("data/CitesforSara.csv"))
papers_select <- select(papers,journal,year,cites,title,au1)
