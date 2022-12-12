
library(tidyverse)
library(plotly)

all_less150 <- readRDS("all_less150.rds")
df_app_arg_less150 <- readRDS("df_app_arg_less150.rds")

source("wedgeplay.R")

function(input, output, session){
  
  output$all <- renderPlotly(expr = all_wedge_analysis(x))
  output$scatter <- renderPlotly(expr = wedge_analysis(input$player, input$location))
  
}
