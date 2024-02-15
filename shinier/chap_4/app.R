rm(list = ls())
#install.packages("vroom")
library(tidyverse)
library(vroom)
library(shiny)

# Getting data on the computer
download_path <- "C:/Users/HP/source/repos/r-datascience/shinier/datasets/neiss/"

#Importing the main dataset that we  will use: injuries
injuries <- vroom::vroom(paste0(download_path,"injuries1.tsv"))
products <- vroom::vroom(paste0(download_path,"products1.tsv"))
population <- vroom::vroom(paste0(download_path,"population1.tsv"))

prod_codes <- setNames(products$prod_code, products$title)

count_top <- function(df, var, n= 5) {
  df |>
    mutate({{var}} := fct_lump(fct_infreq({{var}}),n= n)) |>
    group_by({{var}}) |>
    summarize(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(column(6,
                  selectInput("code","Product",choices = prod_codes))),
  fluidRow(column(4,dataTableOutput("diag")),
           column(4,dataTableOutput("body_part")),
           column(4,dataTableOutput("location"))),
  fluidRow(plotOutput("age_sex"))
)


server <- function(input, output) {
  selected <- reactive({injuries |>
    filter(prod_code == input$code)})
  summary_population <- reactive({selected() |>
    count(age, sex, wt = weight) |>
    left_join(population, by = c("age","sex")) |>
    mutate(rate = n/population * 1e4)})
  output$diag <- renderDataTable({
    count_top(selected(), diag)}, options = list(dom = "t",searching = F)
    )
  output$body_part <- renderDataTable({
    count_top(selected(), body_part)}, options = list(dom = "t",searching = F)
    )
  output$location <- renderDataTable({
    count_top(selected(), location)}, options = list(dom = "t",searching = F)
    )
  output$age_sex <- renderPlot(
    {ggplot(summary_population(), aes(age, rate, color = sex)) +
      geom_line(na.rm = T, size = 0.8) +
      labs(y = "Injuries rate per 10,000 People")}, res = 96
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
