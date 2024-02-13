library(tidyverse)
library(shiny)

ui <- fluidPage( 
  # front end interface 
  fluidRow(numericInput("count", "Enter the number of cows in the area", value = 100),
  textOutput("text")),
  fluidRow(titlePanel("    ")),
  fluidRow(textInput("name", "What's your name?",placeholder = "Enter your name here!"),
  textOutput("named")),
  fluidRow(titlePanel("\nExercises\n")),
  fluidRow(
    textInput("name1", "What's your name?"),
    textOutput("greeting")
  )
  
)

server <- function(input, output,session) {
  # back end logic
  texty <- reactive({
    # reactive expression takes input and produce an output!
    paste0("Hello ", input$name, "!")
  })
  output$text <- renderText(paste("given your choice of a number, there are", input$count, "cows in the area"))
  output$named <- renderText({
    texty()
  })
  output$greeting <- renderText(paste0("Hello ", input$name1))
  }

# Run the application 
shinyApp(ui = ui, server = server)
