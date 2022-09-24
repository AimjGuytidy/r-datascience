library(dplyr)
library(haven)
# load data
data4 <- read_dta("data/dataset4.dta")
data5 <- read_dta("data/dataset5.dta")
data1 <- read_dta("data/dataset1.dta")
data2 <- read_dta("data/dataset2.dta")
data3 <- read_dta("data/dataset3.dta")
# joining dataframe using bind_rows
new_data <- bind_rows(data4, data5)

# rename unique id to unique in dataset 4

data4 <- data4 %>%
  rename(unique1 = uniqueid)

new_data1 <- bind_rows(data4, data5)

# Joining tables with left_join, right_join and full_join

new_data2 <- left_join(data1, data2, by = c("uniqueid", "age"))

dataA <- data1 %>%
  filter(between(age, 18, 20))
dataB <- data2 %>%
  filter(between(age, 19, 21))
datac <- dataA %>%
  inner_join(dataB)
