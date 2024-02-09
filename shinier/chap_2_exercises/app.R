

library(shiny)

ui <- fluidPage(
  textInput("names", "Enter your name",placeholder = "Your name"),
  sliderInput("delivery","When should we deliver?",
              value = as.Date("2024-02-09","%Y-%m-%d"),
              min = as.Date("2024-02-01","%Y-%m-%d"),max = as.Date("2024-10-31","%Y-%m-%d"),
              timeFormat = "%Y-%m-%d"),
  sliderInput("animated","select the value:",value = 5,min = 0, 
              max = 100,step = 5,animate = TRUE),
  selectInput("state","Choose a state:",
              list(`East Coast` = list("NY", "NJ", "CT"),
                   `West Coast` = list("WA", "OR", "CA"),
                   `Midwest` = list("MN", "WI", "IA")))
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
