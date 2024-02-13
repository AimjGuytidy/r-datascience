library(tidyverse)
library(shiny)
library(reactable)

ui <- fluidPage(
verbatimTextOutput("text"),
verbatimTextOutput("code"),
plotOutput("plot",height = "300px", width = "700px"),
dataTableOutput("table"),
reactableOutput("reactable")
)


server <- function(input, output,session) {
  output$text <- renderPrint(str(lm(mpg~wt, data = mtcars)))
  output$code <- renderPrint(t.test(1:5, 2:6))
  output$plot <- renderPlot(plot(1:5),res = 96, 
                            alt = "This is a scatterplot of five random numbers")
  output$table <- renderDataTable(mtcars, 
                                  options = list(pageLenth = 5,dom = "t", ordering = FALSE))
  output$reactable <- renderReactable(reactable(mtcars))
}

# Run the application 
shinyApp(ui = ui, server = server)
