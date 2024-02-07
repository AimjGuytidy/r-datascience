# clear all available objects
rm(list=ls())
# install shiny package
install.packages("shiny")

library(shiny)

# creating app directory and file
ui <- fluidPage("Hello, world!")
server <- function(input, output, session) {}
shinyApp(ui, server)


