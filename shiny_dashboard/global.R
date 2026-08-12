# global.R


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

# Import data - rds from data processing qmd ---- CHANGE THIS, DONT USE HERE
# map_data <- readRDS(here("shiny_dashboard", "data", "processed", "shiny_data.rds"))
# full_taxa <- readRDS(here("shiny_dashboard", "data", "processed", "full_taxa.rds"))
# # Load pre-downloaded data
# monterey_map <- readRDS(here("shiny_dashboard", "data", "processed", "monterey_map.rds"))
# transect_data <- readRDS(here("shiny_dashboard", "data", "processed", "transect_data.rds"))

map_data <- readRDS("data/processed/shiny_data.rds")
full_taxa <- readRDS("data/processed/full_taxa.rds")
monterey_map <- readRDS("data/processed/monterey_map.rds")
transect_data <- readRDS("data/processed/transect_data.rds")
station_distances <- readRDS("data/processed/station_distances.rds")

# MOVE LATER TO SAMPLE PROCESSING QMD: FIX COORDINATE
# Fix the specific sample
# map_data <- map_data |>
#   mutate(Station = if_else(
#     sample_id == "20250724-M1-C-torp-B",  
#     "C1",
#     Station
#   ))

# Convert to dataframe (rest of your existing code)
transect_df <- as.data.frame(transect_data) |> 
  mutate(depth = as.numeric(as.character(depth)))

# Starting points

start_lon <- map_data |> 
  filter(Station == "Elkhorn Slough") |> 
  pull(Longitude) |> 
  mean()

start_lat <- map_data |> 
  filter(Station == "Elkhorn Slough") |> 
  pull(Latitude) |> 
  mean()

end_lon <- map_data |>
  filter(grepl("OSWS", sample_id)) |>
  pull(Longitude) |>
  first()

end_lat <- map_data |>
  filter(grepl("OSWS", sample_id)) |>
  pull(Latitude) |>
  first()


# Get bathymetry ----

# # Bathymetry ----------------- MOVE TO GLOBAL ----------------------
# 
# # Get start and end points for bathymetry 
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
# 
# # Add a small buffer so the points aren't on the edge
# lon_min <- min(c(start_lon, end_lon)) - 0.1
# lon_max <- max(c(start_lon, end_lon)) + 0.1
# lat_min <- min(c(start_lat, end_lat)) - 0.1
# lat_max <- max(c(start_lat, end_lat)) + 0.1
# 
# # Download the background bathymetry map using the bounding box
# monterey_map <- getNOAA.bathy(lon1 = lon_min, 
#                               lon2 = lon_max,
#                               lat1 = lat_min, 
#                               lat2 = lat_max, 
#                               resolution = 1)
# 
# # Get transect
# transect_data <- get.transect(monterey_map,
#                               x1 = start_lon,
#                               y1 = start_lat,
#                               x2 = end_lon,
#                               y2 = end_lat,
#                               distance = TRUE)
# 
# # Make a dataframe
# transect_df <- as.data.frame(transect_data) |> 
#   mutate(depth = as.numeric(as.character(depth)))



# End move to global -----------------------------------------------















########################## Test code delete later #################################


# Lantern/lampfish could be a good subset because they are in the deep ocean, so potentially easier to survey using eDNA 

# Create subset of lantern and lampfish 
# What depths/stations do we see them at? --> similar depth graph but have depth be y, station be x toggle on for either or both -- group all lanternfish together 
# How rare/common? --> 

# Plot -- show number of detections at each depth each season 
# 
# lantern <- map_data |>
#   filter(grepl("lampfish|lanternfish", common_name, ignore.case = TRUE)) |>
#   select(map_label, depth, abundance, Season, sample_id, Latitude, Longitude, upload_time, sampling.date, Family, Genus) |>
#   mutate(
#     station_label = str_extract(sample_id, "OSWS|C1|M1|M2|MARS")
#   ) |>
#   #filter(Season == "Spring 2025") |>  #Add back later
#   mutate( # Do this to combine all chlorophyll max obs
#     depth = as.numeric(as.character(depth)))
# 
# # Get min, max value and median Chla
# min_depth <-  min(lantern$depth[lantern$depth > 0], na.rm = TRUE)
# max_depth_below_150 <- max(lantern$depth[lantern$depth < 150], na.rm = TRUE)
# chl_max_median <- median(c(min_depth, max_depth_below_150))
# 
# 
# lantern <- lantern |>
#   mutate(
#     depth_plot = if_else(
#       depth > 0 & depth <= max_depth_below_150, chl_max_median, depth) )
# 
# lantern_summary <- lantern |>
#   group_by(Season, station_label, depth_plot, Latitude, Longitude, upload_time, sampling.date) |>  # add map label back if want individual sp
#   summarize(detections = n(), .groups = "drop") |>
#   # mutate(
#   #   map_label = if_else(
#   #     str_detect(map_label, "^[A-Z][a-z]+ [a-z]+ \\("),  # Two words before parentheses
#   #     str_replace(map_label, "^([A-Z])[a-z]+ ([a-z]+)", "\\1. \\2"),  # Abbreviate genus
#   #     map_label  # Keep as-is (only one word before parentheses)
#   #   )) |>
#   mutate(
#     depth_label = if_else(
#       depth_plot == chl_max_median, "Chlorophyll Max", as.character(depth_plot)
#     )
#   )
# 
# # Make graph
# lantern_summary$depth_plot <- as.numeric(lantern_summary$depth_plot)
# 
# p <- ggplot(lantern_summary, aes(x = station_label, y = depth_plot, size = detections)) +
#   annotate("rect",
#            xmin = 0.5,  # Just past y-axis
#            xmax = Inf,  # To the right edge
#            ymin = min_depth,   # Top of Chl Max zone
#            ymax = max_depth_below_150,   # Bottom of Chl Max zone
#            fill = "lightgreen",
#            alpha = 0.2) +
#   geom_point(alpha = 0.5, color = "darkblue") +  # add back _interactive and aes
#                          # aes(tooltip = paste0(
#                          #   map_label, "\n",
#                          #   "Depth: ", depth_label, "m\n",
#                          #   "Relative Abundance: ",
#                          #   if_else(relative_abundance < 0.01,
#                          #           paste0(format(relative_abundance, scientific = TRUE, digits = 2), "%"),
#                          #           sprintf("%.2f%%", relative_abundance)))))+
#   scale_size(range = c(1, 12), name="Relative Abundance") +
#   labs(x = "Stations",
#        y = "Depth (m)",
#        title = "Lanternfish and lampfish detections",
#        subtitle = paste0("Green zone = Deep Chlorophyll Max (", min_depth, "-", max_depth_below_150, ")")) +
#   theme_bw(base_size = 14) +
#   scale_y_reverse(
#     breaks = c(0, 20, 150, 300),
#     limits = c(300, 0)) +
#   # scale_y_break(c(-20, -150), scales = 0.5) +  # Break between 20-150m
#   #theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
#         #legend.position = "none") +
#   theme(axis.title.y = element_text(
#     margin = margin(r = 20),
#     vjust = 2),
#     plot.subtitle = element_text(color = "darkgreen"),
#     axis.title.x = element_text(
#       margin = margin(t = 20)
#     )
#   ) +
#   theme(
#     panel.grid.major.x = element_blank()
#   ) +
#   theme(
#     panel.grid.major.x = element_blank(),
#     panel.grid.major.y = element_line(color = "gray90"),  # Keep major gridlines
#     panel.grid.minor.y = element_blank()  # Remove minor gridlines
#   )
# 
# # Try to get bathymetry
# #starting point
# 
# # -121.858
# # start_lon <- lantern_summary |>
# #   filter(station_label == "C1") |>
# #   pull(unique(Longitude)) |> 
# #   first()
# 
# # Change to elkhorn slough
# start_lon <- map_data |> 
#   filter(Station == "Elkhorn Slough") |> 
#   select(Station, Latitude, Longitude) |> 
#   summarize(Latitude = mean(Latitude),
#             Longitude = mean(Longitude)) |> 
#   pull(unique(Longitude))
#   
# 
# # 36.8045
# # start_lat <- lantern_summary |>
# #   filter(station_label == "C1") |>
# #   pull(unique(Latitude)) |> 
# #   first()
# 
# start_lat <- map_data |> 
#   filter(Station == "Elkhorn Slough") |> 
#   select(Station, Latitude, Longitude) |> 
#   summarize(Latitude = mean(Latitude),
#             Longitude = mean(Longitude)) |> 
#   pull(unique(Latitude))
# 
# # -122.53517
# end_lon <- lantern_summary |>
#   filter(station_label == "OSWS") |>
#   pull(Longitude) |>
#   first()
# 
# # 36.6775
# end_lat <- lantern_summary |>
#   filter(station_label == "OSWS") |>
#   distinct(Latitude) |>
#    pull(Latitude)
# 
# # 
# 
# 
# 
# # 1. Calculate a dynamic bounding box based on your start and end points
# # We add a small buffer (e.g., 0.1 degrees) so the points aren't right on the edge
# lon_min <- min(c(start_lon, end_lon)) - 0.1
# lon_max <- max(c(start_lon, end_lon)) + 0.1
# lat_min <- min(c(start_lat, end_lat)) - 0.1
# lat_max <- max(c(start_lat, end_lat)) + 0.1
# 
# # 2. Download the background bathymetry map using the bounding box
# monterey_map <- getNOAA.bathy(lon1 = lon_min, 
#                               lon2 = lon_max,
#                               lat1 = lat_min, 
#                               lat2 = lat_max, 
#                               resolution = 1)
# 
# 
# 
# # Get station coords: average because soem have 2
# # Get mean coordinates for each station
# stations <- lantern_summary |> 
#   group_by(station_label) |> 
#   summarize(
#     Latitude = mean(Latitude, na.rm = TRUE),
#     Longitude = mean(Longitude, na.rm = TRUE)
#   ) |> 
#   add_row(station_label = "ES", Latitude = start_lat, Longitude = start_lon) |> 
#   arrange(Longitude)  # Order from west to east
# 
# 
# # Calculate distance along transect for each station
# station_distances <- data.frame(
#   station = character(),
#   distance = numeric(),
#   stringsAsFactors = FALSE
# )
# 
# for(i in 1:nrow(stations)) {
#   # Get transect to each station from start point
#   temp_transect <- get.transect(monterey_map,
#                                 x1 = start_lon,
#                                 y1 = start_lat,
#                                 x2 = stations$Longitude[i],
#                                 y2 = stations$Latitude[i],
#                                 distance = TRUE)
#   
#   station_distances <- rbind(station_distances,
#                              data.frame(station = stations$station_label[i],
#                                         distance = max(temp_transect$dist)))
# }
# 
# # ADD THIS: Create the main transect from Elkhorn Slough to OSWS
# transect_data <- get.transect(monterey_map,
#                               x1 = start_lon,
#                               y1 = start_lat,
#                               x2 = end_lon,
#                               y2 = end_lat,
#                               distance = TRUE)
# # Plot with custom labels
# plotProfile(transect_data,
#             ylim = c(-1000, 0))
# 
# # Add vertical lines at station positions
# abline(v = station_distances$distance, lty = 2, col = "red")
# 
# # Turn off clipping so text can go outside plot area
# par(xpd = TRUE)
# 
# # Add labels above the plot
# text(x = station_distances$distance, 
#      y = 200,  # Positive value above 0
#      labels = station_distances$station,
#      adj = 0.5,   # Left-aligned
#      col = "red",
#      font = 2)
# 
# # Turn clipping back on
# par(xpd = FALSE)
# 
# 
# # Get observations + join to station distances 
# lantern_plot <- lantern_summary |> 
#   select(station_label, detections, depth_plot, depth_label, Season, upload_time, sampling.date) |> 
#   filter(Season == "Summer 2025") |> 
#   left_join(station_distances, by = c("station_label" = "station"))
# 
# points(x = lantern_plot$distance,
#        y = -lantern_plot$depth_plot,
#        cex = lantern_plot$detections/3,
#        pch = 19,
#        col = "yellow")
# 
# 
# 
# 
# # Is it possible to make this a plotly? 
# transect_df <- as.data.frame(transect_data)
# 
# # Clean the data before plotting
# lantern_plot <- lantern_plot |> 
#   mutate(depth = as.numeric(as.character(depth_plot)))
# 
# transect_df <- transect_df |> 
#   mutate(depth = as.numeric(as.character(depth)))
# 
# # Create annotations: 
# annotations_list <- lapply(1:nrow(station_distances), function(i) {
#   
#   label <- station_distances$station[i]
#   if(label == "Elkhorn Slough") {
#     label <- "Elkhorn<br>Slough"
#   }
#   
#   list(
#     x = station_distances$distance[i],
#     y = 1,  # Position above water surface (adjust as needed)
#     yref = "paper",
#     text = paste0("<b>", station_distances$station[i], "</b>"),
#     showarrow = FALSE,
#     font = list(size = 12, color = "#1A1A40"),
#     xanchor = "center",
#     yanchor = "bottom"
#   )
# })
# 
# 
# 
# 
# # Create plotly 
# p <- plot_ly() |> 
#   # Bathymetry fill (tan area)
#   add_polygons(
#     x = c(transect_df$dist.km, rev(transect_df$dist.km)),
#     y = c(transect_df$depth, rep(min(transect_df$depth) - 100, nrow(transect_df))),
#     fillcolor = toRGB("burlywood", alpha = 0.9),
#     
#     line = list(color = 'black', width = 1),
#     hoverinfo = 'none',
#     showlegend = FALSE
#   ) |>
#   # Water layer (light blue - from surface to bathymetry)
#   add_polygons(
#     x = c(transect_df$dist.km, rev(transect_df$dist.km)),
#     y = c(transect_df$depth, rep(0, nrow(transect_df))),
#     fillcolor = "lightblue",
#     line = list(width = 0),
#     hoverinfo = 'none',
#     showlegend = FALSE
#   ) |> 
#  
#   # Lanternfish markers - NEGATE the depth values
#   add_markers(
#     x = as.numeric(lantern_plot$distance),
#     y = -as.numeric(lantern_plot$depth_plot),  # Make them negative
#    
#     #size = as.numeric(lantern_plot$detections),  # Map to your detections column
#     #sizes = c(10, 60),   # Min and max bubble sizes
#     marker = list(
#       color = as.numeric(lantern_plot$detections),
#       colorscale = list(c(0, "#FFF7BC"), c(0.5, "#FF6347"), c(1, "#8B0000")),
#       showscale = TRUE,
#       size = 10, 
#       line = list(color = 'black', width = 1), 
#       colorbar = list(title = "Detections")
#     
#     ),
#     hovertemplate = paste0("Depth: ", lantern_plot$depth_label, "<br>",
#                            "Detections: ", lantern_plot$detections,
#                            "<extra></extra>")
#     
#   ) |> 
#   layout(
#     font = list(family = "Arial, sans-serif", size = 12),
#     title = list(
#       text = "Detections of myctophidae (lampfish and lanternfish)",
#       x = 0.1
#     ),
#     
#     xaxis = list(title = "Distance from start of transect (km)",
#                  fixedrange = FALSE),
#     yaxis = list(
#       title = "Depth (m)",
#       range = c(-3000, 0),
#       fixedrange = FALSE
#     ),
#     margin = list(t = 60), # top margin
#     annotations = annotations_list,
#     dragmode = 'zoom',
#     hovermode = 'closest'
#   ) |> 
#   # Subtle horizontal lines at depth intervals
#   config(scrollZoom = TRUE,
#          displayModeBar = TRUE)
# 
# 
# 
# p
# 
# 
# ##############################################################################
# ########################## Try for rockfish ##################
# ###############################################################################
# 
# rockfish <- map_data |>
#   filter(grepl("rockfish", common_name, ignore.case = TRUE)) |>
#   select(map_label, depth, abundance, Season, sample_id, Latitude, Longitude, upload_time, sampling.date, Family, Genus) |>
#   mutate(
#     station_label = str_extract(sample_id, "OSWS|C1|M1|M2|MARS")
#   ) |>
#   #filter(Season == "Spring 2025") |>  #Add back later
#   mutate( # Do this to combine all chlorophyll max obs
#     depth = as.numeric(as.character(depth)))
# 
# # Get min, max value and median Chla
# min_depth_rock <-  min(rockfish$depth[rockfish$depth > 0], na.rm = TRUE)
# max_depth_below_150_rock <- max(rockfish$depth[rockfish$depth < 150], na.rm = TRUE)
# chl_max_median_rock <- median(c(min_depth_rock, max_depth_below_150_rock))
# 
# 
# rockfish <- rockfish |>
#   mutate(
#     depth_plot = if_else(
#       depth > 0 & depth <= max_depth_below_150_rock, chl_max_median_rock, depth) )
# 
# rock_summary <- rockfish |>
#   group_by(Season, station_label, depth_plot, Latitude, Longitude, upload_time, sampling.date) |>  # add map label back if want individual sp
#   summarize(detections = n(), .groups = "drop") |>
#   mutate(
#     depth_label = if_else(
#       depth_plot == chl_max_median_rock, "Chlorophyll Max", as.character(depth_plot)
#     )
#   ) |> 
#   drop_na() # why are there NA's? 
# 
# # Make graph
# rock_summary$depth_plot <- as.numeric(rock_summary$depth_plot)
# 
# rock_plot <- rock_summary |> 
#   select(station_label, detections, depth_plot, depth_label, Season, upload_time, sampling.date) |> 
#   filter(Season == "Autumn 2025") |> 
#   left_join(station_distances, by = c("station_label" = "station"))
# 
# 
# # rockfish plot: 
# 
# plot_ly() |> 
#   # Bathymetry fill (tan area)
#   add_polygons(
#     x = c(transect_df$dist.km, rev(transect_df$dist.km)),
#     y = c(transect_df$depth, rep(min(transect_df$depth) - 100, nrow(transect_df))),
#     fillcolor = toRGB("burlywood", alpha = 0.9),
#     
#     line = list(color = 'black', width = 1),
#     hoverinfo = 'none',
#     showlegend = FALSE
#   ) |>
#   # Water layer (light blue - from surface to bathymetry)
#   add_polygons(
#     x = c(transect_df$dist.km, rev(transect_df$dist.km)),
#     y = c(transect_df$depth, rep(0, nrow(transect_df))),
#     fillcolor = "lightblue",
#     line = list(width = 0),
#     hoverinfo = 'none',
#     showlegend = FALSE
#   ) |> 
#   
#   # Lanternfish markers - NEGATE the depth values
#   add_markers(
#     x = as.numeric(rock_plot$distance),
#     y = -as.numeric(rock_plot$depth_plot),  # Make them negative
#     
#     #size = as.numeric(lantern_plot$detections),  # Map to your detections column
#     #sizes = c(10, 60),   # Min and max bubble sizes
#     marker = list(
#       color = as.numeric(rock_plot$detections),
#       colorscale = list(c(0, "#FFF7BC"), c(0.5, "#FF6347"), c(1, "#8B0000")),
#       showscale = TRUE,
#       size = 10, 
#       line = list(color = 'black', width = 1), 
#       colorbar = list(title = "Detections")
#       
#     ),
#     hovertemplate = paste0("Depth: ", rock_plot$depth_label, "<br>",
#                            "Detections: ", rock_plot$detections,
#                            "<extra></extra>")
#     
#   ) |> 
#   layout(
#     font = list(family = "Arial, sans-serif", size = 12),
#     title = list(
#       text = "Detections of myctophidae (lampfish and lanternfish)",
#       x = 0.1
#     ),
#     
#     xaxis = list(title = "Distance from start of transect (km)",
#                  fixedrange = FALSE),
#     yaxis = list(
#       title = "Depth (m)",
#       range = c(-3000, 0),
#       fixedrange = FALSE
#     ),
#     margin = list(t = 60), # top margin
#     annotations = annotations_list,
#     dragmode = 'zoom',
#     hovermode = 'closest'
#   ) |> 
#   # Subtle horizontal lines at depth intervals
#   config(scrollZoom = TRUE,
#          displayModeBar = TRUE)
# 
# #####################################################################
# #################### Most species depths? ####################
# #####################################################################
# 
# max_d_s <- map_data |> 
#   select(depth, map_label, sample_id) |> 
#   filter(depth %in% c(150, 300)) |> 
#   group_by(map_label, depth) |> 
#   summarize(detections = n(), .groups = "drop") |> 
#   arrange(depth, desc(detections))
#   
# 
# # Look at top organisms at each depth
# top <- max_d_s |>
#   group_by(depth) |>
#   slice_max(detections, n = 10)  # top 10 at each depth
# 
# 
# 
# 
# 
# #####################################################################
# #################### Study site map for overview ####################
# #####################################################################
# 
# # # Load libraries
# library(sf)
# library(rnaturalearth)
# library(maps)
# 
# mbnms_shp <- read_sf(here("indicator_sp_shiny", "data", "raw", "mbnms_py.shp"))
# 
# # 3. Transform the projection to WGS84 (Required for Leaflet)
# mbnms_wgs84 <- st_transform(mbnms_shp, crs = 4326)
# 
# leaflet(stations) |>
#   addProviderTiles(providers$CartoDB.Positron) |>
#   addProviderTiles(providers$Esri.OceanBasemap) |>
#   setView(lng = -122.16, lat = 36.74, zoom = 7.5) |>
#   addPolygons(
#     data = mbnms_wgs84,
#     color = "#0284C7",
#     weight = 1,
#     fillColor = "#E0F2FE",
#     fillOpacity = 0.5,
#     popup = "Monterey Bay National Marine Sanctuary"
# 
#   ) |>
# 
# 
#   addCircleMarkers(~Longitude, ~ Latitude,
#                    color = "#EF4444",
#                    fillColor = "#EF4444",
#                    popup = ~station_label,
#                    fillOpacity = 0.9,
#                    opacity = 0.9,
#                    radius = 6)
# 
# 
# Static map ---
# 1. Load required libraries
# library(ggplot2)
# library(maps)
# library(ggOceanMaps)
# 
# 
# ca_county <- subset(map_data("county"), region == "california")
# 
# # 4. Build the plot and zoom into Monterey Bay coordinates
# # county lines
# map <- ggplot() +
#   geom_polygon(data = ca_county, aes(x = long, y = lat, group = group),
#                fill = "antiquewhite", color = "gray60") +
#   # Plot the coordinates
# 
#   geom_point(data = stations, aes(x = Longitude, y = Latitude),
#              color = "#EF4444", size = 3) +
#   # Add labels to the points
#   geom_text(data = stations, aes(x = Longitude, y = Latitude, label = station_label),
#             vjust = -1, fontface = "bold") +
#   # Crop map to Monterey Bay boundaries
#   coord_quickmap(xlim = c(-122.6, -121.5), ylim = c(36.4, 37.1)) +
#   # Clean up the background
#   labs(x = "Longitude", y = "Latitude") +
# 
# 
#   theme(
#     panel.background = element_rect(fill = "lightblue"),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank()
#   )
# 
# ggsave(filename = "media/study_map.jpeg", plot = map)

# 
# #   ##########################################################################
# ########################### Species stacked bar graph ########################
# ##############################################################################
# # Filter data:
# # bar_plot_data_sp <- map_data |> 
# #   filter(Season == "Spring 2025") |>  # %in% input$season_input
# #          # near(Latitude, clicked$lat),
# #          # near(Longitude, clicked$lng)) |>
# #   group_by(map_label) |> 
# #   summarize(
# #     sp_count = n(), # of sp detections at that lat/long
# #     .groups = "drop") |> 
# #   arrange(sp_count) |> # reorder 
# #   mutate(percent = (sp_count / sum(sp_count)) * 100) |> 
# #   mutate(
# #     percent_bin = cut(percent,
# #                       breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
# #                       labels = c("<1%", "1-2%", "2-5%", "5-10%", "10-20%", "20-50%", ">50%"))
# #   ) |> 
# #   arrange(desc(percent))
# # 
# # bar_plot_data_sp <- bar_plot_data_sp %>%
# #   mutate(map_label = fct_rev(factor(map_label, levels = unique(map_label)))) 
# # 
# # # Step 3: Create palette using the FACTOR LEVELS (in order)
# # unique_labels <- levels(bar_plot_data_sp$map_label)
# # 
# # 
# # # Make palette
# # #unique_labels <- na.omit(unique(bar_plot_data_sp$map_label))
# # bar_palette_sp <- ifelse(unique_labels == "Balaenoptera physalus (Fin whale)", "#1c1554", "darkgrey")
# # names(bar_palette_sp) <- unique_labels
# #   
# #   # c("<1%" = "#43c197",
# #   #                "1-2%"= "#3da48c",
# #   #                "2-5%" = "#368881",
# #   #                "5-10%" = "#306b76",
# #   #                "10-20%" = "#294e6a",
# #   #                "20-50%" = "#23325f",
# #   #                ">50%" = "#1c1554")
# # 
# # 
# # # Render plot:
# # 
# #   plot_ly(bar_plot_data_sp,
# #           x = 1, # makes it so it can be stacked
# #           y = ~percent,
# #           type = 'bar',
# #           color = ~map_label,  # color by label not count
# #           colors = bar_palette_sp,
# #           text = ~map_label,           # Species names
# #           textposition = 'inside',      # Put text inside bars
# #           textfont = list(
# #             color = 'white',            # White text
# #             size = 10                   # Font size
# #           ),
# #           customdata = ~map_label, 
# #           marker = list(
# #             line = list(color = 'white', width = 1)
# #           ),
# #           hovertemplate = paste0(
# #             "<b>%{customdata}</b><br>",  # <-- USE customdata instead of text
# #             "Percent: %{y:.2f}%<br>",
# #             "<extra></extra>"
# #           )) %>%
# #     layout(
# #       barmode = 'stack',
# #       xaxis = list(
# #         title = "",           # Remove x-axis title
# #         showticklabels = FALSE  # Hide "rep(Species, nrow...)"
# #       ),
# #       yaxis = list(
# #         title = "Percentage of Samples (%)"  # Y-axis label
# #       ),
# #       showlegend = FALSE,
# #       hoverlabel = list(
# #         font = list(
# #           color = "white"
# #         )
# #       )
# #     )
# #   
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
