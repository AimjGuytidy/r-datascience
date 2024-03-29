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

temp <- injuries |>
  filter(prod_code == 649)

count_top <- function(df, var, n= 5) {
  df |>
    mutate({{var}} := fct_lump(fct_infreq({{var}}),n= n)) |>
    group_by({{var}}) |>
    summarize(n = as.integer(sum(weight)))
}

count_top1 <- function(df, var, n= 5) { # placement of fct_lump vis a vis fct_infreq doesn't matter
  df |>
    mutate({{var}} := fct_infreq(fct_lump({{var}},n= n))) |>
    group_by({{var}}) |>
    summarize(n = as.integer(sum(weight)))
}

ui <- fluidPage(
  fluidRow(column(8,
                  selectInput("code","Product",choices = prod_codes,width = "100%")),
           column(2,
                  selectInput("y","Y axis", choices = c("rate","count"))),
           column(2,
                  numericInput("x","rows",value = 4, min = 2, max = 12)
           )),
  fluidRow(column(4,dataTableOutput("diag")),
           column(4,dataTableOutput("body_part")),
           column(4,dataTableOutput("location"))),
  fluidRow(plotOutput("age_sex")),
  fluidRow(column(2,
                  actionButton("action","Tell me a story")),
           column(8,
                  textOutput("narrative")),
           column(2,
                  actionButton("backward","Previous story")))
)

k = 7

server <- function(input, output) {

  selected <- reactive({injuries |>
      filter(prod_code == input$code)})
  summary_population <- reactive({selected() |>
      count(age, sex, wt = weight) |>
      left_join(population, by = c("age","sex")) |>
      mutate(rate = n/population * 1e4)})
  output$diag <- renderDataTable({
    count_top(selected(), diag, n = input$x)}, options = list(dom = "t",searching = F)
  )
  output$body_part <- renderDataTable({
    count_top(selected(), body_part, n = input$x)}, options = list(dom = "t",searching = F)
  )
  output$location <- renderDataTable({
    count_top(selected(), location, n = input$x)}, options = list(dom = "t",searching = F)
  )
  output$age_sex <- renderPlot(
    {if(input$y == "rate"){
      ggplot(summary_population(), aes(age, rate, color = sex)) +
        geom_line(na.rm = T, size = 0.8) +
        labs(y = "Injuries rate per 10,000 People")}
      else {
        ggplot(summary_population(), aes(age, n, color = sex)) +
          geom_line(na.rm = T, size = 0.8) +
          labs(y = "Injuries rate per 10,000 People")}
    }, res = 96
  )
  
  output$narrative <- renderText({
    select_subset <- input$action - input$backward + 1
    if(select_subset >=0) {
    selected()$narrative[select_subset] }
    else if (select_subset > nrow(selected())) {selected()$narrative[1]}
    else {selected()$narrative[nrow(selected())+select_subset+1]}
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
