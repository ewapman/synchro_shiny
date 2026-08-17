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

# Load data
map_data <- readRDS("data/processed/shiny_data.rds")
full_taxa <- readRDS("data/processed/full_taxa.rds")
monterey_map <- readRDS("data/processed/monterey_map.rds")
transect_data <- readRDS("data/processed/transect_data.rds")
station_distances <- readRDS("data/processed/station_distances.rds")


# Convert to dataframe 
transect_df <- as.data.frame(transect_data) |> 
  mutate(depth = as.numeric(as.character(depth)))



