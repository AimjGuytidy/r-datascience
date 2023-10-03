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
  geom_line()+
  labs(title="Fertility Rate by Country-Income-Level over Time")
plotdata_bygroupyear <- mutate(plotdata_bygroupyear,Year=as.numeric(str_sub(Year,-4)))
plotdata_bygroupyear1 <- mutate(plotdata_bygroupyear1,
                                Year=as.numeric(str_replace(Year,"X","")))

ggplot(plotdata_bygroupyear, aes(x=Year, y = FertilityRate, group = Country.Code,
                                 color=Country.Code)) +
  geom_line()+
  labs(title="Fertility Rate by Country-Income-Level over Time")

# Histogram Analysis
histdata_twoyears <- select(teenager_fr, Country.Name, Country.Code, 
                            Indicator.Name, Indicator.Code, X1960, X2000)
histdata_twoyears <- pivot_longer(histdata_twoyears,cols = c("X1960","X2000"),
                                  names_to = "Year", values_to = "FertilityRate")%>%
  select(Year, Country.Name, Country.Code, FertilityRate)
histdata_twoyears <- filter(histdata_twoyears, !is.na(FertilityRate))

ggplot(histdata_twoyears, aes(x=FertilityRate)) +
  geom_histogram(data = subset(histdata_twoyears, Year=="X1960"),color="darkred",
                 fill="red", alpha = 0.2) +
  geom_histogram(data = subset(histdata_twoyears, Year == "X2000"), color = "darkblue",
                 fill = "blue", alpha = 0.2)
ggsave("visuals/hist_ds.png")

# Adding kernels to  the histogram
ggplot(histdata_twoyears, aes(x=FertilityRate, group = Year, color = Year,
                              alpha = 0.2)) + 
  geom_histogram(aes(y=..density..)) + # use after_stat(density) !!!!
  geom_density(data = subset(histdata_twoyears, Year == "X1960"), color = "darkred",
               fill = "red", alpha = 0.2, bw = 5) +
  geom_density(data = subset(histdata_twoyears, Year == "X2000"), color = "darkblue",
               fill = "blue", alpha = 0.2, bw = 5)
  