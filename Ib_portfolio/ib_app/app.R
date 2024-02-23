rm(list = ls())

#install.packages("plotly")
#install.packages("tidyquant")
library(tidyverse)
library(tidyquant)
library(plotly)

library(shiny)
set.seed(42)
asset_classes <- c("Cash","Equities","Fixed Income","Real Estate")
tickers <- list(cash = c("GBP","USD"), Equities = list(ETFs = c("SPY","UKx","XLU")),
                `Fixed Income` = list(ETFs = c("BND", "HYG","MBB", "SPTL", "SPTS", "TLT")),
                `Real Estate` = list(ETFs = c("VNQ"), Stocks = c("ABR", "AGNC")))

holdings <- data.frame(tickers = c("GBP","USD","SPY","UKx","XLU","BND", "HYG",
                                   "MBB", "SPTL", "SPTS", "TLT","VNQ","ABR", "AGNC"),
                       unit_cost = c(0,0,440.25,86.78,63.46,73.75,78.45,89.42,29.13,29.26,
                                     94.29,75.12,12.63,10.16))

# Define UI for application that draws a histogram
ui <- fluidPage(


)

# Define server logic required to draw a histogram
server <- function(input, output,session) {


}

# Run the application 
shinyApp(ui = ui, server = server)
