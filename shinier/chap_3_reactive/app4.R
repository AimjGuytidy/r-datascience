library(tidyverse)
library(shiny)

# creating functions that we will use in our server function
freqpoly <- function(x1,x2, binwidth = 0.1, xlim = c(-3,3)) {
  df <- data.frame(
    x = c(x1,x2),
    g = c(rep("x1",length(x1)),rep("x2", length(x2)))
  )
  ggplot(df, aes(x,color = g)) +
    geom_freqpoly(binwidth = binwidth,size = 1) + 
    coord_cartesian(xlim = xlim)
}

t_test <- function(x1,x2) {
  test <- t.test(x1,x2)
  sprintf(
    "p value: %0.3f\n[%0.2f, %0.2f]",
    test$p.value, test$conf.int[1], test$conf.int[2]
  )
}

# set.seed(42)
# 
# x1 <- rnorm(100, mean = 0, sd = 0.5)
# x2 <- rnorm(200, mean = 0.15, sd = 0.9)
# 
# freqpoly(x1, x2)
# cat(t_test(x1,x2))

ui <- fluidPage(
  fluidRow(column(3,
                  numericInput("dist1_l1","lambda1",value = 3),
                  numericInput("dist1_l2", "lambda2", value = 4),
                  numericInput("dist1_n", "n", value = 1000,min = 0,max = 10000),
                  actionButton("simulate","Simulate!")),
           column(9,plotOutput("freqpoly")))
  
)

server <- function(input, output) {
  timer <- reactiveTimer(500) # updates every 500ms
  x1 <- eventReactive(input$simulate,{
    rpois(input$dist1_n,lambda = input$dist1_l1)})
  x2 <- eventReactive(input$simulate,{
    rpois(input$dist1_n,lambda = input$dist1_l2)})
  output$freqpoly <- renderPlot({
    freqpoly(x1(),x2(), binwidth = 1, xlim = c(0,40))
  },res = 96)
}

# Run the application 
shinyApp(ui = ui, server = server)
