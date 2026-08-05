# # Static map
# 
# library(ggplot2)
# library(maps)
# library(ggOceanMaps)
# 
# bath <- map_data |>
#   select(map_label, Station, depth, abundance, Season, sample_id, Latitude, Longitude, upload_time, sampling.date, Family, Genus) |>
#   mutate( # Do this to combine all chlorophyll max obs
#     depth = as.numeric(as.character(depth)))
# 
# 
# # Get station coords: average because some have 2
# stations <- bath |> 
#   group_by(Station) |> 
#   summarize(
#     Latitude = mean(Latitude, na.rm = TRUE),
#     Longitude = mean(Longitude, na.rm = TRUE)
#   ) |> 
#   # add_row(Station = "ES", Latitude = start_lat, Longitude = start_lon) |> 
#   arrange(Longitude) |>  # Order from west to east
#   mutate(
#     Station = if_else(Station == "Elkhorn Slough", "ES", Station)
#   )
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
#                              data.frame(station = stations$Station[i],
#                                         distance = max(temp_transect$dist)))
# }
# 
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
#   geom_text(data = stations, aes(x = Longitude, y = Latitude, label = Station),
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
# ggsave(filename = "shiny_dashboard/www/media/study_map.jpeg", plot = map)
