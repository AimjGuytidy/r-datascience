rm(list = ls())
#install.packages("vroom")
library(tidyverse)
library(vroom)
library(shiny)

# Getting data on the computer
dir.create("shinier/datasets/neiss")
download_path <- "shinier/datasets/neiss/"

#Creating a function to download datasets 
download <- function(name) {
  url <- "https://github.com/hadley/mastering-shiny/tree/main/neiss/"
  download.file(url = paste0(url,name),destfile = paste0(download_path,name),
                quiet = TRUE)
}

#Downloading data
download("injuries.tsv.gz")
download("population.tsv")
download("products.tsv")

#Importing the main dataset that we  will use: injuries
injuries <- vroom::vroom(paste0(download_path,"injuries1.tsv"))
products <- vroom::vroom(paste0(download_path,"products1.tsv"))
population <- vroom::vroom(paste0(download_path,"population1.tsv"))

#Exploration 

selected <- injuries |>
  filter(prod_code == 649)
nrow(selected)
selected |> 
  count(location, wt = weight, sort = T)
selected |>
  count(body_part, wt = weight, sort = T)
selected |>
  count(diag, wt = weight, sort = T)

summary_data <- selected |>
  count(age, sex, wt = weight)
summary_data

ggplot(summary_data,aes(age,n,color = sex)) +
  geom_line(size = 0.8) +
  ylab("Estimated number of injuries") +
  xlab("age")





ui <- fluidPage(
  
)


server <- function(input, output) {

  
}

# Run the application 
shinyApp(ui = ui, server = server)
