# # Static map
# 
# library(ggplot2)
# library(maps)
# 
# 
# 
# 
# # Get station coords: average because some have 2
# stations <-  tribble(
#   ~station, ~station_lat, ~station_lon,
#   "ES", 36.8117, -121.7796,
#   "C1", 36.8045, -121.858,
#   "M1", 36.7502, -122.0495,
#   "MARS", 36.721, -122.1903,
#   "M2", 36.7015, -122.3743,
#   "OSWS", 36.6775, -122.5352
# )
#   
# 
# library(tigris)
# library(sf)
# 
# options(tigris_use_cache = TRUE)
# 
# # Get California counties as sf object
# ca_counties <- counties(state = "CA", cb = TRUE, class = "sf")
# 
# cities <- data.frame(
#   city = c("Santa Cruz", "Monterey"),
#   lon = c(-122.03, -121.89),
#   lat = c(36.97, 36.60)
# )
# 
# map <- ggplot() +
#   geom_sf(data = ca_counties, fill = "antiquewhite", color = "gray60") +
#   geom_point(data = stations, aes(x = station_lon, y = station_lat),
#              color = "#EF4444", size = 3) +
#   geom_text(data = stations, aes(x = station_lon, y = station_lat, label = station),
#             vjust = -1, fontface = "bold") +
#   # Add city points
#   geom_point(data = cities, aes(x = lon, y = lat),
#              color = "black", size = 1, shape = 21, fill = "black") +
#   geom_text(data = cities, aes(x = lon, y = lat, label = city),
#             fontface = "italic", size = 3.5, vjust = -1) +
#   coord_sf(xlim = c(-122.6, -121.5), ylim = c(36.4, 37.1)) +
#   labs(x = "Longitude", y = "Latitude") +
#   theme_minimal() +
#   theme(
#     panel.background = element_rect(fill = "lightblue"),
#     panel.grid.major = element_blank(),
#     panel.border = element_blank(),
#     panel.grid.minor = element_blank(),
#     axis.title.x = element_text(margin = margin(t = 15)),  # Push "Longitude" further down
#     axis.title.y = element_text(margin = margin(r = 15)),
#     panel.ontop = FALSE
#   )
# map
# 
# ggsave(filename = "study_map.jpeg", plot = map)
