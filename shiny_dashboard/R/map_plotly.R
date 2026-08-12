# This script is to try and remake the plotly graph with the correct depth partitioning 

plotly_map_new <- function(data_fn, season_fn, species_fn, depth_fn) {
  
  # Change station coordinates so they are the same for each station
  station_coords <- tribble(
    ~station, ~station_lat, ~station_lon,
    "Elkhorn Slough", 36.8117, -121.7796,
    "C1", 36.8045, -121.858,
    "M1", 36.7502, -122.0495,
    "MARS", 36.721, -122.1903,
    "M2", 36.7015, -122.3743,
    "OSWS", 36.6775, -122.5352
  )
  
  if(species_fn == "All taxa") {
    
    
    
    # Filter date for all selection ----
    # Want number of unique species at each location and depth 
    data_dcm_all <- data_fn |>
      mutate(
        depth = case_when(
          depth == 0 ~ "0",
          depth > 0 & depth < 150 ~ "DCM",
          depth == 150 ~ "150",
          depth == 300 ~ "300",
          TRUE ~ as.character(depth)
        )
      )
    
    leaflet_data_all <- data_dcm_all |>
      filter(Season %in% season_fn,
             depth %in% depth_fn) |> 
      group_by(Station) |> # Removed latitude/longitude to get one point per station
      summarize(
        indicator_count = n_distinct(map_label),
        seasons = paste(sort(unique(Season)), collapse = ", "),
        .groups = "drop"
      ) |>
      # Join station coordinates
      left_join(station_coords, by = c("Station" = "station")) #|>
      # Add jitter for display
      # mutate(
      #   display_lat = if_else(
      #     Station == "Elkhorn Slough",  # ← Fixed the space!
      #     station_lat,                   # NO vertical jitter
      #     station_lat + rnorm(n(), 0, 0.004)
      #   ),
      #   display_lon = if_else(
      #     Station == "Elkhorn Slough",
      #     station_lon + rnorm(n(), 0, 0.0025),  # Tiny horizontal jitter
      #     station_lon + rnorm(n(), 0, 0.004)
      #   )
      # )
    
    depth_envtl_table_all <- data_dcm_all |>
      filter(Season %in% season_fn,
             depth %in% depth_fn) |>
      distinct(sample_id, Station, depth, temperature, salinity, oxygen) |>
      group_by(Station, depth) |>
      summarize(
        samples = n_distinct(sample_id),
        temperature = round(mean(temperature, na.rm = TRUE), 1),
        salinity = round(mean(salinity, na.rm = TRUE), 1),
        oxygen = round(mean(oxygen, na.rm = TRUE), 1),
        .groups = "drop"
      )
    
    depth_hover_table_all <- depth_envtl_table_all |>
      mutate(
        depth = factor(depth, levels = c("0", "DCM", "150", "300"))  # ← Set order
      ) |>
      arrange(Station, depth) |>
      group_by(Station) |>
      summarize(
        hover_table = paste0(
          "<b>By Depth (m):</b><br>",
          paste0(
            "<b>", depth, ":</b> ", 
            "Temp: ", ifelse(is.na(temperature), "NA", paste0(temperature, "°C")), " | ",  # ← Fixed
            "Salinity: ", ifelse(is.na(salinity), "NA", salinity), " | ",  # ← Fixed
            "O₂: ", ifelse(is.na(oxygen), "NA", oxygen),  # ← Fixed
            collapse = "<br>"
          )
        ),
        .groups = "drop"
      )
    
    # Join back to subset data:
    leaflet_data_all <- leaflet_data_all |>
      left_join(depth_hover_table_all, by = c("Station"))
    
    
    
    # Plot All taxa ----
    
    # Make plotly graph ---
    
    p <- plot_ly(leaflet_data_all,
                 lat = ~station_lat,
                 lon = ~station_lon,
                 customdata = ~Station,
                 #key = ~paste(Latitude, Longitude, sep = "_"),
                 type = "scattermapbox",
                 mode = "markers",
                 source = "indicator_map", 
                 marker = list(
                   size = 15,
                   color = ~indicator_count,
                   colorscale = "YlOrRd",
                   reversescale = TRUE,
                   showscale = TRUE,
                   colorbar = list(
                     title = "Unique<br>Taxa",
                     orientation = "h",
                     x = 0,
                     xanchor = "left",
                     y = 0,
                     len = 0.5,
                     thickness = 15,
                     bgcolor = "rgba(255,255,255,0)",  # ← Transparent background
                     borderwidth = 0,                   # ← No border
                     outlinewidth = 0,
                     tickmode = "array",                    # ← Add this
                     tickvals = seq(
                       min(leaflet_data_all$indicator_count, na.rm = TRUE),
                       max(leaflet_data_all$indicator_count, na.rm = TRUE),
                       length.out = 5
                     ),
                     ticktext = round(seq(
                       min(leaflet_data_all$indicator_count, na.rm = TRUE),
                       max(leaflet_data_all$indicator_count, na.rm = TRUE),
                       length.out = 5
                     )),
                     tickfont = list(size = 10)),             # ← Font size),                   # ← Explicitly no outline),
                   
                   opacity = 0.8,
                   line = list(color = 'white', width = 1)
                 ),
                 hovertext = ~paste0(
                   "<b>Station:</b> ", Station, "<br><br>",
                   "<b>Season(s):</b> ", seasons, "<br><br>",
                   "<b>Unique taxa detected:</b> ", indicator_count, "<br><br>",
                   hover_table
                   
                   # if_else(
                   #   is.na(temperature),
                   #   "",
                   #   paste0(
                   #     "<b>Temperature:</b> ", round(temperature, 1), "°C<br>",
                   #     "<b>Salinity:</b> ", round(salinity, 1), " psu<br>",
                   #     "<b>Oxygen:</b> ", round(oxygen, 2), " ml/L"
                   #   )
                   # )
                 ),
                 hoverinfo = "text"
    ) |>
      layout(
        mapbox = list(
          style = "white-bg",
          zoom = 8,
          center = list(lon = -122.16, lat = 36.74),
          layers = list(list(
            below = 'traces',
            sourcetype = "raster",
            source = list("https://services.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}")
          ))
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        hoverlabel = list(
          align = "left",           # Push it to the side
          bgcolor = "rgba(255,255,255,0.9)",  # Slightly transparent
          bordercolor = "black",
          font = list(size = 11,
                      color = "black")
        )
      ) 
    p
    # Register the click event
    event_register(p, "plotly_click")
    
    return(p)
    
    
    
  } # End if statement
  
  
  
  else {
    
    
    
    # Filter species and date ----
    # Want number of SAMPLES at each depth and location for that organism
    # Categorize DCM and convert to character for selection
    data_dcm <- data_fn |>
      mutate(
        depth = case_when(
          depth == 0 ~ "0",
          depth > 0 & depth < 150 ~ "DCM",
          depth == 150 ~ "150",
          depth == 300 ~ "300",
          TRUE ~ as.character(depth)
        )
      )
    
    # Number of sample with that species at that location
    leaflet_data_subset <- data_dcm |> 
      filter(Season %in% season_fn, #MAKE REACTIVE
             depth %in% depth_fn, 
             map_label == species_fn) |> #|>
      group_by(Station) |> # These will both be reactive CHANGED TO GROUP BY STATION ONLY
      summarize(
        sample_count = n_distinct(sample_id), # size circles by number of samples w/ that organism at that point
        depths_detected = paste(sort(unique(depth)), collapse = ", "),
        seasons = paste(sort(unique(Season)), collapse = ", "),
        .groups = "drop"
      ) 
    
    # Total number of samples at that location
    leaflet_data_subset_totals <- data_dcm |> 
      filter(Season %in% season_fn) |> 
      group_by(Station) |> # CHANGED TO GROUP BY STATION ONLY
      summarize(
        total_count = n_distinct(sample_id), 
        .groups = "drop"
      ) 
    
    # Join: 
    leaflet_data_subset <- leaflet_data_subset |>
      left_join(leaflet_data_subset_totals, by = c("Station")) |> # REMOVED LATITUDE AND LONGITUDE
      mutate(
        relative_abundance = (sample_count / total_count) * 100
      ) |> 
      # Join station coordinates
      left_join(station_coords, by = c("Station" = "station")) #|>
      # Add jitter
      # mutate(
      #   display_lat = if_else(
      #     Station == "Elkhorn Slough",  # ← Fixed the space!
      #     station_lat,                   # NO vertical jitter
      #     station_lat + rnorm(n(), 0, 0.004)
      #   ),
      #   display_lon = if_else(
      #     Station == "Elkhorn Slough",
      #     station_lon + rnorm(n(), 0, 0.0025),  # Tiny horizontal jitter
      #     station_lon + rnorm(n(), 0, 0.004)
      #   )
      # )
    
    
    # Make table for hover: 
    depth_envtl_table <- data_dcm |> 
      filter(Season %in% season_fn,
             depth %in% depth_fn,
             map_label == species_fn) |>
      distinct(sample_id, Station, depth, temperature, salinity, oxygen) |> 
      group_by(Station, depth) |> # CHANGE FROM LATITUDE LONGITUDE
      summarize(
        samples = n_distinct(sample_id),
        temperature = round(mean(temperature, na.rm = TRUE), 1),
        salinity = round(mean(salinity, na.rm = TRUE), 1),
        oxygen = round(mean(oxygen, na.rm = TRUE), 1),
        .groups = "drop"
      ) 
    
    
    # Make html table:
    # Replace the hover_table creation with this simpler format:
    depth_hover_tables <- depth_envtl_table |>
      mutate(
        depth = factor(depth, levels = c("0", "DCM", "150", "300"))  # ← Set order
      ) |>
      arrange(Station, depth) |> # change to station
      group_by(Station) |> #change to station
      summarize(
        hover_table = paste0(
          "<b>By Depth (m):</b><br>",
          paste0(
            "<b>", depth, ":</b> ", samples, " samples | ",
            "Temp: ", ifelse(is.na(temperature), "NA", paste0(temperature, "°C")), " | ",  # ← Fixed
            "Salinity: ", ifelse(is.na(salinity), "NA", salinity), " | ",  # ← Fixed
            "O₂: ", ifelse(is.na(oxygen), "NA", oxygen),  # ← Fixed
            collapse = "<br>"
          )
        ),
        .groups = "drop"
      )
    
    # Join back to subset data: 
    leaflet_data_subset <- leaflet_data_subset |>
      left_join(depth_hover_tables, by = c("Station")) # Change to station
    
    
    # Create palette with minimum range to avoid weird scaling ----
    # actual_min <- min(leaflet_data_subset$relative_abundance, na.rm = TRUE)
    # actual_max <- max(leaflet_data_subset$relative_abundance, na.rm = TRUE)
    # 
    # 
    # range_width <- actual_max - actual_min
    # min_range_width <- 4  # Adjust this value as needed
    # 
    # if(range_width < min_range_width) {
    #   # Expand range to minimum width
    #   domain_min <- 1
    #   domain_max <- max(actual_max, min_range_width + 1)
    # } else {
    #   # Use actual range
    #   domain_min <- as.integer(actual_min)
    #   domain_max <- as.integer(actual_max)
    # }
    # 
    # # Fix for single values - make them appear light
    # if(actual_min == actual_max) {
    #   plot_min <- actual_min
    #   plot_max <- actual_min + 10  # Creates range so single value is light
    # } else {
    #   plot_min <- domain_min
    #   plot_max <- domain_max
    # }
    # 
    # 
    # 
    
    
    # Plot species subset ----
    # Check if there's only 1 row of data
    #single_point <- nrow(leaflet_data_subset) == 1
    
    # Have to create dummy points so that taxa with one point will map to scale correctly: 
    if(nrow(leaflet_data_subset) == 1) {
      dummy_points <- data.frame(
        Station = c(NA, NA),  # Add this
        #Season = c(NA, NA),  # Add this
        station_lat = c(0, 0),
        station_lon = c(0, 0), 
        # display_lat = c(0, 0),  # Add this
        # display_lon = c(0, 0),
        relative_abundance = c(0, 100),
        sample_count = c(NA, NA),
        total_count = c(NA, NA),
        depths_detected = c(NA, NA),
        hover_table = c(NA, NA),
        seasons = c(NA, NA)
      )
      leaflet_data_subset <- bind_rows(leaflet_data_subset, dummy_points)
    }
    
    p <- plot_ly(leaflet_data_subset,
                 lat = ~station_lat,
                 lon = ~station_lon,
                 customdata = ~Station,
                 #key = ~paste(Latitude, Longitude, sep = "_"),
                 type = "scattermapbox",
                 mode = "markers",
                 source = "indicator_map", 
                 
                 
                 marker = list(
                   size = 15,
                   # color = if(single_point && actual_min == actual_max) "#FFFFCC" else ~relative_abundance,  # ← Force color for single point
                   # colorscale = if(actual_min == actual_max) list(c(0, "#FFFFCC"), c(1, "#FFFFCC")) else "Viridis",
                   color = ~relative_abundance,
                   colorscale = "Viridis",
                   #reversescale = if(actual_min == actual_max) FALSE else TRUE,
                   reversescale = TRUE,
                   showscale = TRUE,
                   cauto = FALSE,
                   colorbar = list(
                     title = "% of<br>Station Samples", 
                     orientation = "h",
                     x = 0.05,
                     xanchor = "left",
                     y = 0,
                     yanchor = "bottom",
                     len = 0.5,
                     thickness = 15,
                     bgcolor = "rgba(255,255,255,0)",
                     borderwidth = 0,
                     outlinewidth = 0,
                     tickmode = "array",
                     tickvals = pretty(c(0, 100), n = 5),
                     ticktext = as.character(pretty(c(0, 100), n = 5)),
                     tickfont = list(size = 10), 
                     tickangle = 0
                     
                   ),                             
                   opacity = 0.8,
                   line = list(color = 'white', width = 1),
                   cmin = 0, #if(actual_min == actual_max) actual_min else domain_min,
                   cmax = 100 #if(actual_min == actual_max) actual_min else domain_max
                 ),
                 hovertemplate = ~paste0(
                   "<b>Station:</b> ", Station, "<br><br>",
                   "<b>Unique taxa: </b> 1 ", "<br><br>",
                   "<b>Season:</b> ", seasons, "<br><br>", 
                   "<b>Samples: </b>", sample_count," of ", total_count, " total samples at this station", " (", round(relative_abundance, 2), "%", ")", "<br><br>",
                   hover_table,
                   "<extra></extra>"
                 ) 
                 
    ) |>
      
      layout(
        mapbox = list(
          style = "white-bg",
          zoom = 8,
          center = list(lon = -122.16, lat = 36.74),
          layers = list(list(
            below = 'traces',
            sourcetype = "raster",
            source = list("https://services.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}")
          ))
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        hoverlabel = list(
          align = "left",           # Push it to the side
          bgcolor = "rgba(255,255,255,0.9)",  # Slightly transparent
          bordercolor = "black",
          font = list(size = 11,
                      color = "black")
        )
      ) 
    p
    # Register the click event
    event_register(p, "plotly_click")
    
    return(p)
    
  } # End else
  
  
}

