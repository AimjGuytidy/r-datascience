library(shiny)

# free text
ui <- fluidPage(
  sliderInput("min", "Limit (minimum)", value = 20, min = 10, max = 50),
  sliderInput("num", "Number two", value = c(10,20), min = 0, max = 50),
  numericInput("num1", "Number one", value = 0, min = 0, max = 50),
  textInput("name", "What's your name?"),
  passwordInput("passcode","what's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3),
  dateInput("date","Date of Birth"),
  dateRangeInput("date_range","when do you want to go on vacation next?")
)


server <- function(input, output, session) {

}

# Run the application
shinyApp(ui = ui, server = server)

# Numeric dates
