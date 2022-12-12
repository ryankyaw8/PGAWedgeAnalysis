
library(tidyverse)
library(plotly)

all_less150 <- readRDS("all_less150.rds")
df_app_arg_less150 <- readRDS("df_app_arg_less150.rds")

source("wedgeplay.R")

ui <- fluidPage(
  
  titlePanel("Proximity to Hole as Approach Distance Increases (Shotlink Data from 2017-2021)"),
  
  mainPanel(
    plotlyOutput(outputId = "all"),
    selectInput("player", "Select Player", choices = c("None", unique(df_app_arg_less150$Name))),
    selectInput("location", "Select Location", 
                choices = c("Fairway", "Rough", "Bunker")),
    plotlyOutput(outputId ="scatter")
  )
  
)


