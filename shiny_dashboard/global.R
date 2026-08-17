# Load Libraries 
library(shiny)
library(tidyr)
library(dplyr)
library(ggplot2)
library(shinyWidgets) 
library(forcats)
library(plotly)
library(ggiraph)
library(RColorBrewer)
library(stringr)
library(shinydashboard)
library(bslib)
library(ggtext)
library(shinycssloaders)

map_data <- readRDS("data/processed/shiny_data.rds")
full_taxa <- readRDS("data/processed/full_taxa.rds")
monterey_map <- readRDS("data/processed/monterey_map.rds")
transect_data <- readRDS("data/processed/transect_data.rds")
station_distances <- readRDS("data/processed/station_distances.rds")


# Convert to dataframe 
transect_df <- as.data.frame(transect_data) |> 
  mutate(depth = as.numeric(as.character(depth)))



# start_lon <- -121.7796
# start_lat <- 36.8117
# end_lon <- -122.5352
# end_lat <- 36.6775
# start_lon <- map_data |> 
#   filter(Station == "Elkhorn Slough") |> 
#   pull(Longitude) |> 
#   mean()
# 
# start_lat <- map_data |> 
#   filter(Station == "Elkhorn Slough") |> 
#   pull(Latitude) |> 
#   mean()
# 
# end_lon <- map_data |>
#   filter(grepl("OSWS", sample_id)) |>
#   pull(Longitude) |>
#   first()
# 
# end_lat <- map_data |>
#   filter(grepl("OSWS", sample_id)) |>
#   pull(Latitude) |>
#   first()
# 

