# clear all available objects
rm(list=ls())
# install shiny package
#install.packages("shiny")

library(shiny)
library(tidyverse)

# creating app directory and file

# ui <- fluidPage("Hello, world!")
# server <- function(input, output, session) {}
# shinyApp(ui, server)


# Adding UI controls

ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table"),
  plotOutput("ploty")
)

server <- function(input, output, session) {
  output$summary <- renderPrint({
    dataset <- get(input$dataset,"package:datasets")
    summary(dataset)
  })
  output$table <- renderTable({
    dataset <- get(input$dataset, "package:datasets")
    head(dataset,6)
    
  })
  output$ploty <- renderPlot({
    dataset <- get(input$dataset,"package:datasets")
    nums <- Filter(is.numeric, dataset)
    columns <- colnames(nums)
    hist(nums[,1],col = "blue",border = "white", main = columns[1])
  })
}
shinyApp(ui,server)