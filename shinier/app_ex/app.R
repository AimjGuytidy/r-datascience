#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)


ui <- fluidPage(
  titlePanel("Minor Information about the User"),
  textInput("name",label = "What's your name?", placeholder = "Empty"),
  numericInput("age", label = "How old are you?", value = NA),
  textOutput("greeting"),
  textOutput("old")

)


server <- function(input, output, session) {
  output$greeting <- renderText({
    paste0("Hello, ",input$name)
  })
  output$old <- renderText({
    paste0("You are ",input$age, " years old!")
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
