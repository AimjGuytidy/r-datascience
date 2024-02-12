rm(list = ls())
library(tidyverse)
library(shiny)


ui <- fluidPage(
textOutput("text"),
verbatimTextOutput("code"),
verbatimTextOutput("print"),
tableOutput("static"),
dataTableOutput("dynamic")
)


server <- function(input, output,session) {
  # output$text <- renderText({
  #   "Hello World!"
  # })
  output$text <- renderText("Hello Kamugisha!")
  # output$code <- renderPrint({
  #   summary(1:10)
  # })
  output$code <- renderPrint(summary(1:10))
  output$print <- renderPrint(print("Hello"))
  output$static <- renderTable(head(mtcars,5))
  output$dynamic <- renderDataTable({
    mtcars
  },options = list(pageLength = 6))
}

# Run the application 
shinyApp(ui = ui, server = server)
