library(tidyverse)
library(shiny)



ui <- fluidPage(
  textInput("name","What's your name?"),
  textOutput("greetings")
  
)

server <- function(input, output) {
  string <- reactive({paste0("Hello ", input$name,"!")})
  output$greetings <- renderText(string())
  observeEvent(input$name,{
    message("Greetings complete!")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
