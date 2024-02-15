rm(list = ls())
library(tidyverse)
devtools::install_github("hadley/neiss")
library(neiss)

download_path <- "shinier/datasets/neiss/"

top_prod <- injuries %>%
  filter(trmt_date >= as.Date("2017-01-01"), trmt_date < as.Date("2018-01-01")) %>%
  count(prod1, sort = TRUE) %>%
  filter(n > 5 * 365)

injuries %>%
  filter(trmt_date >= as.Date("2017-01-01"), trmt_date < as.Date("2018-01-01")) %>%
  semi_join(top_prod, by = "prod1") %>%
  mutate(age = floor(age), sex = tolower(sex), race = tolower(race)) %>%
  filter(sex != "unknown") %>%
  select(trmt_date, age, sex, race, body_part, diag, location, prod_code = prod1, weight, narrative) %>%
  vroom::vroom_write(paste0(download_path,"injuries.csv"))

products %>%
  semi_join(top_prod, by = c("code" = "prod1")) %>%
  rename(prod_code = code) %>%
  vroom::vroom_write(paste0(download_path,"products.csv"))

population %>%
  filter(year == 2017) %>%
  select(-year) %>%
  rename(population = n) %>%
  vroom::vroom_write(paste0(download_path,"population.csv"))