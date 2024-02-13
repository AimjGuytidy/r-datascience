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
  fluidRow(column(4,"Distribution 1",
                  numericInput("dist1_num","n",value = 1000,min = 0,max = 10000),
                  numericInput("dist1_mean", "µ", value = 0),
                  numericInput("dist1_sd", "σ", value = 0.5)),
           column(4,"Distribution 2",
                  numericInput("dist2_num","n",value = 1000,min = 0,max = 10000),
                  numericInput("dist2_mean", "µ", value = 0),
                  numericInput("dist2_sd", "σ", value = 0.5)),
           column(4,"Frequency polygon",
                  numericInput("freq_bin","Bin width",value = 0.1),
                  sliderInput("freq_range", "range", value = c(-3,3),min = -5, max = 5))),
  
  fluidRow(column(9,plotOutput("freqpoly")),
           column(3,textOutput("ttest")))

)

server <- function(input, output) {
  x1 <- reactive({rnorm(input$dist1_num,mean = input$dist1_mean, sd = input$dist1_sd)})
  x2 <- reactive({rnorm(input$dist2_num,mean = input$dist2_mean, sd = input$dist2_sd)})
  output$freqpoly <- renderPlot(
    freqpoly(x1(),x2(),
             binwidth = input$freq_bin,xlim = input$freq_range)
  )
  output$ttest <- renderText(t_test(x1(),x2()))
}

# Run the application 
shinyApp(ui = ui, server = server)
