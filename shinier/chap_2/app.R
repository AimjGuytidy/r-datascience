library(shiny)

animals <- c("dog", "cat", "mouse", "bird", "other", "I hate animals")

# free text
ui <- fluidPage(
  sliderInput("min", "Limit (minimum)", value = 20, min = 10, max = 50),
  sliderInput("num", "Number two", value = c(10,20), min = 0, max = 50),
  numericInput("num1", "Number one", value = 0, min = 0, max = 50),
  textInput("name", "What's your name?"),
  passwordInput("passcode","what's your password?"),
  textAreaInput("story", "Tell me about yourself", rows = 3),
  dateInput("date","Date of Birth"),
  dateRangeInput("date_range","when do you want to go on vacation next?"),
  selectInput("states","What's your favorite state?",state.name,multiple = TRUE),
  radioButtons("pet", "What's your favorite animal?",animals),
  radioButtons("rb", "choose One:",
               choiceNames = list(
                 icon("angry"),
                 icon("smile"),
                 icon("sad-tear")
               ),
               choiceValues = list("angry","happy","sad")),
  checkboxGroupInput("checkit","What animals do you like?",animals),
  checkboxInput("checker", "cleanup", value = TRUE),
  checkboxInput("shutdown", "Shutdown"),
  fileInput("upload", NULL),
  actionButton("click","Click me!"),
  actionButton("drink", "Drink me!", icon = icon("cocktail")),
  fluidRow(
    actionButton("click1","Click me!",class = "btn-danger"),
    actionButton("drink", "Drink me!", class = "btn-lg btn-success")
  ),
  fluidRow(
    actionButton("eat","Eat me!",class = "btn-block")
  )
)


server <- function(input, output, session) {

}

# Run the application
shinyApp(ui = ui, server = server)

# Numeric dates
