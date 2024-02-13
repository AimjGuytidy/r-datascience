library(tidyverse)
library(shiny)

ui <- fluidPage( 
  # front end interface 
  fluidRow(numericInput("count", "Enter the number of cows in the area", value = 100),
  textOutput("text")),
  fluidRow(titlePanel("    ")),
  fluidRow(textInput("name", "What's your name?",placeholder = "Enter your name here!"),
  textOutput("named"))
  
)

server <- function(input, output,session) {
  # back end logic
  texty <- reactive({
    paste0("Hello ", input$name, "!")
  })
  output$text <- renderText(paste("given your choice of a number, there are", input$count, "cows in the area"))
  output$named <- renderText({
    texty()
  })
  }

# Run the application 
shinyApp(ui = ui, server = server)
