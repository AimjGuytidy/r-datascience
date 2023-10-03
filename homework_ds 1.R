# Preliminaries

rm(list = ls()) # removing all objects to start with a clean slate
library("utils")
library("tidyverse")

getwd() # make sure I am in the right directory

# getting the data
gender_data <- as_tibble(read_csv("data/Gender_StatsData.csv"))

view(head(gender_data,3))

# creating a tibble with only adolescent fertility rate indicator
teenager_fr <- filter(gender_data, Indicator.Code=="SP.ADO.TFRT") # we can use subset too!

rm(gender_data) # we no longer need the entire dataset

mean(teenager_fr$X1975, na.rm = TRUE)

mean(teenager_fr$X1960,na.rm = TRUE)
sd(teenager_fr$X1960,na.rm = TRUE)

mean(teenager_fr$X2000,na.rm = TRUE)
sd(teenager_fr$X2000,na.rm = TRUE)

# creating a tibble with only income level data 
byincomelevel <- filter(teenager_fr, Country.Code %in% c("LIC","MIC","HIC","WLD"))

# turning data in rectangular form
test <- pivot_longer(byincomelevel,cols=starts_with("X"),names_to = "Year",values_to = "FertilityRate")
test2 <- gather(byincomelevel,"Year","FertilityRate",X1960:X2015)
plotdata_bygroupyear1 <- test[test$Year!="X"&!is.na(test$FertilityRate),]
plotdata_bygroupyear <- test2 |>
  select(Year, Country.Name, Country.Code, FertilityRate)

# make income level as columns
plotdata_byyear <- select(plotdata_bygroupyear, Country.Code, Year,
                          FertilityRate) |>
  spread(Country.Code,FertilityRate)
plotdata_byyear1 <- select(plotdata_bygroupyear1,Country.Code, Year,
                           FertilityRate) |>
  pivot_wider(id_cols = "Year",names_from = Country.Code, values_from = FertilityRate)

# let's plot!!

ggplot(plotdata_bygroupyear, aes(x=Year, y = FertilityRate, group = Country.Code,
                                 color=Country.Code)) +
  geom_line()
