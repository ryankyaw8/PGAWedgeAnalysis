
library(tidyverse)
library(plotly)

all_less150 <- readRDS("all_less150.rds")
df_app_arg_less150 <- readRDS("df_app_arg_less150.rds")

####### Analysis of PGA Tour Prox to hole as distance increases ###########

### Get data from each year ###

# df <- readRDS("shot_df_2017_best.rds")
# 
# df <- df %>%
#   filter(Strokes.Gained.Category == "Approach the Green" | Strokes.Gained.Category == "Around the Green")
# 
# df$Distance.to.Pin.yds <- round((df$Distance.to.Pin/12)/3, 0)
# 
# df$Year <- 2017
# 
# # Getting the putting data from 2010-2021
# for (i in 2018:2021) {
#   year = i
#   print(paste0("Year: ", year))
# 
#   df_new = readRDS(paste0("shot_df_", year, "_best.rds"))
# 
#   df_new <- df_new %>%
#     filter(Strokes.Gained.Category == "Approach the Green" | Strokes.Gained.Category == "Around the Green")
# 
#   df_new$Distance.to.Pin.yds <- round((df_new$Distance.to.Pin/12)/3, 0)
# 
#   df_new$Year <- year
# 
#   df = rbind(df, df_new)
# }
# 
# #######################################
# 
# ##### Clean Data ####
# 
# df_app_arg_less150 <- df %>%
#   filter(Distance.to.Pin.yds <= 150 & Distance.to.Pin.yds >= 25)
# 
# df_app_arg_less150$Distance.to.Hole.after.the.Shot.ft <-
#   round(df_app_arg_less150$Distance.to.Hole.after.the.Shot/12, 2)
# 
# df_app_arg_less150$Name <- paste(df_app_arg_less150$Player.First.Name,
#                                       df_app_arg_less150$Player.Last.Name)
# 
# # Labeling locations as fwy, rough, bunker, other
# df_app_arg_less150$From.Location <- NA
# 
# df_app_arg_less150$From.Location[df_app_arg_less150$From.Location.Scorer. == "Fairway"] <- "Fairway"
# 
# df_app_arg_less150$From.Location[df_app_arg_less150$From.Location.Scorer. == "Primary Rough" |
#                                         df_app_arg_less150$From.Location.Scorer. == "Intermediate Rough"] <- "Rough"
# 
# df_app_arg_less150$From.Location[df_app_arg_less150$From.Location.Scorer. == "Fairway Bunker" |
#                                         df_app_arg_less150$From.Location.Scorer. == "Greenside Bunker"] <- "Bunker"
# 
# df_app_arg_less150$From.Location[is.na(df_app_arg_less150$From.Location)] <- "Other"
# 
# saveRDS(df_app_arg_less150, "df_app_arg_less150.rds")
# 
# # Aggregate all data
# all_less150_prox <- aggregate(df_app_arg_less150$Distance.to.Hole.after.the.Shot.ft,
#                               list(df_app_arg_less150$Distance.to.Pin.yds,
#                                    df_app_arg_less150$From.Location),
#                               mean)
# colnames(all_less150_prox) <- c("Distance.to.Hole", "Shot.Location", "Proximity.After.Shot")
# 
# 
# 
# all_less150_shots <- aggregate(df_app_arg_less150$Year,
#                                list(df_app_arg_less150$Distance.to.Pin.yds,
#                                     df_app_arg_less150$From.Location),
#                                length)
# colnames(all_less150_shots) <- c("Distance.to.Hole", "Shot.Location", "Number of Shots")
# 
# all_less150 <- all_less150_prox %>%
#   left_join(all_less150_shots, by = c("Distance.to.Hole", "Shot.Location"))
# 
# saveRDS(all_less150, "all_less150.rds")

##########################################

# Function for Shiny 
wedge_analysis <- function(player, location){
  
  if(player == "None"){
    
    all_less150_filter <- all_less150 %>%
      filter(Shot.Location == location)
    
    scatter_plotly <- plot_ly()
    
    scatter_plotly <- add_trace(scatter_plotly,
                                type = "scatter",
                                mode = "lines+markers",
                                x = all_less150_filter$Distance.to.Hole,
                                y = all_less150_filter$Proximity.After.Shot,
                                name = location,
                                hovertemplate = paste("Distance to Hole: %{x}yds",
                                                      "<br>Proximity to Hole: %{y}ft"))
    
    scatter_plotly <- scatter_plotly %>%
      layout(title = "Approach Distance to Hole vs Proximity After Shot",
             xaxis = list(title = "Distance to Hole (YDS)"),
             yaxis = list(title = "Proximity (FT)"))
    
    return(scatter_plotly)
    
  } else {
    
    all_less150_filter <- all_less150 %>%
      filter(Shot.Location == location)
    
    df_app_arg_less150_player <- df_app_arg_less150 %>%
      filter(Name == player & From.Location == location)
    
    all_less150_prox_player <- aggregate(df_app_arg_less150_player$Distance.to.Hole.after.the.Shot.ft,
                                         list(df_app_arg_less150_player$Distance.to.Pin.yds,
                                              df_app_arg_less150_player$From.Location),
                                         mean)
    colnames(all_less150_prox_player) <- c("Distance.to.Hole", "Shot.Location", "Proximity.After.Shot")
    
    
    
    all_less150_shots_player <- aggregate(df_app_arg_less150_player$Year,
                                          list(df_app_arg_less150_player$Distance.to.Pin.yds,
                                               df_app_arg_less150_player$From.Location),
                                          length)
    colnames(all_less150_shots_player) <- c("Distance.to.Hole", "Shot.Location", "Number of Shots")
    
    all_less150_player <- all_less150_prox_player %>%
      left_join(all_less150_shots_player, by = c("Distance.to.Hole", "Shot.Location"))
    
    
    # scatter <- ggplot() +
    #   # geom_point(aes(x = all_less150_filter$Distance.to.Hole,
    #   #                y = all_less150_filter$Proximity.After.Shot,
    #   #                color = "blue")) +
    #   # geom_point(aes(x = all_less150_player$Distance.to.Hole,
    #   #                y = all_less150_player$Proximity.After.Shot,
    #   #                color = "red")) +
    #   geom_smooth(aes(x = all_less150_filter$Distance.to.Hole,
    #                  y = all_less150_filter$Proximity.After.Shot,
    #                  color = "blue"))+
    #   geom_smooth(aes(x = all_less150_player$Distance.to.Hole,
    #                  y = all_less150_player$Proximity.After.Shot,
    #                  color = "red")) +
    #   scale_color_manual(name = "Legend", values = c("blue" = "blue", "red" = "red"),
    #                      labels = c("All", player), guide = "legend") +
    #   ylab("Proximity.After.Shot.Ft") +
    #   xlab("Distance.to.Hole.Yds")
    
    
    # scatter_plotly <- ggplotly(scatter)
    # 
    # return(scatter_plotly)
    
    
    scatter_plotly <- plot_ly()
    
    scatter_plotly <- add_trace(scatter_plotly,
                                type = "scatter",
                                mode = "lines+markers",
                                x = all_less150_filter$Distance.to.Hole,
                                y = all_less150_filter$Proximity.After.Shot,
                                name = "All Players",
                                hovertemplate = paste("Distance to Hole: %{x}yds",
                                                      "<br>Proximity to Hole: %{y}ft"))
    
    scatter_plotly <- add_trace(scatter_plotly, 
                                type = "scatter",
                                mode = "lines+markers", 
                                x = all_less150_player$Distance.to.Hole,
                                y = all_less150_player$Proximity.After.Shot,
                                text = c(all_less150_player$`Number of Shots`),
                                name = player,
                                hovertemplate = paste("Distance to Hole: %{x}yds",
                                                      "<br>Proximity to Hole: %{y}ft"))
    
    scatter_plotly <- scatter_plotly %>%
      layout(title = "Approach Distance to Hole vs Proximity After Shot",
             xaxis = list(title = "Distance to Hole (YDS)"),
             yaxis = list(title = "Proximity (FT)"))
    
    
    
    return(scatter_plotly)
    
  }
  
  
}

wedge_analysis("Justin Thomas", "Fairway")
 

all_wedge_analysis <- function(x){
  
  all_less150 <- all_less150 %>%
    filter(Shot.Location != "Other")
  
  scatter_plotly <- plot_ly()
  
  scatter_plotly <- add_trace(scatter_plotly,
                              type = "scatter",
                              mode = "lines+markers",
                              x = all_less150$Distance.to.Hole,
                              y = all_less150$Proximity.After.Shot,
                              color = all_less150$Shot.Location,
                              hovertemplate = paste("Distance to Hole: %{x}yds",
                                                    "<br>Proximity to Hole: %{y}ft"))
  
  scatter_plotly <- scatter_plotly %>%
    layout(title = "Approach Distance to Hole vs Proximity After Shot",
           xaxis = list(title = "Distance to Hole (YDS)"),
           yaxis = list(title = "Proximity (FT)"))
  
  return(scatter_plotly)
  
}

all_wedge_analysis(x)

