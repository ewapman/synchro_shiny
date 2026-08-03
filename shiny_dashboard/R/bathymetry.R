bath_graph <- function(data_fn, season_fn, species_fn) {
  
  
  bath <- data_fn |>
    select(map_label, Station, depth, abundance, Season, sample_id, Latitude, Longitude, upload_time, sampling.date, Family, Genus) |>
    mutate( # Do this to combine all chlorophyll max obs
      depth = as.numeric(as.character(depth)))
  
  # Get min, max value and median Chla
  min_depth <-  min(bath$depth[bath$depth > 0], na.rm = TRUE)
  max_depth_below_150 <- max(bath$depth[bath$depth < 150], na.rm = TRUE)
  chl_max_median <- median(c(min_depth, max_depth_below_150))
  
  # If depth is greater than zero but less than 150, note the depth at the chl_max_median for plotting
  bath <- bath |>
    mutate(
      depth_plot = if_else(
        depth > 0 & depth <= max_depth_below_150, chl_max_median, depth) ) 
  
  
  # Two options: All taxa or unique taxa
  if(species_fn == "All taxa"){
    bath_summary <- bath |>
      filter(Season %in% season_fn ) |> # REACTIVE: This will be reactive + loop through the different season selections
      group_by(Station, depth_plot) |>  # Want number at each station and at each depth
      summarize(detections = n(), .groups = "drop") |>
      mutate(
        depth_label = if_else(
          depth_plot == chl_max_median, "Chlorophyll Max", as.character(depth_plot)
        )
      )
  } else {
    bath_summary <- bath |>
      filter(Season %in% season_fn ) |> # REACTIVE: This will be reactive + loop through the different season selections
      filter(map_label == species_fn) |> # REACTIVE: This will be reactive and depend on species input
      group_by(Station, depth_plot) |>  # Want number at each station and at each depth
      summarize(detections = n(), .groups = "drop") |>
      mutate(
        depth_label = if_else(
          depth_plot == chl_max_median, "Chlorophyll Max", as.character(depth_plot)
        )
      )
  }

  
  # Get station coords: average because some have 2
  stations <- bath |> 
    group_by(Station) |> 
    summarize(
      Latitude = mean(Latitude, na.rm = TRUE),
      Longitude = mean(Longitude, na.rm = TRUE)
    ) |> 
    # add_row(Station = "ES", Latitude = start_lat, Longitude = start_lon) |> 
    arrange(Longitude) |>  # Order from west to east
    mutate(
      Station = if_else(Station == "Elkhorn Slough", "ES", Station)
    )
  
  
  # Calculate distance along transect for each station
  station_distances <- data.frame(
    station = character(),
    distance = numeric(),
    stringsAsFactors = FALSE
  )
  
  for(i in 1:nrow(stations)) {
    # Get transect to each station from start point
    temp_transect <- get.transect(monterey_map,
                                  x1 = start_lon,
                                  y1 = start_lat,
                                  x2 = stations$Longitude[i],
                                  y2 = stations$Latitude[i],
                                  distance = TRUE)
    
    station_distances <- rbind(station_distances,
                               data.frame(station = stations$Station[i],
                                          distance = max(temp_transect$dist)))
  }
  
  
  # Join distances
  bath_plot <- bath_summary |> 
    select(Station, detections, depth_plot, depth_label) |> 
    left_join(station_distances, by = c("Station" = "station")) |> 
    mutate(depth = as.numeric(as.character(depth_plot)))
  
  
  # Create annotations
  annotations_list <- lapply(1:nrow(station_distances), function(i) {
    
    label <- station_distances$station[i]
    if(label == "Elkhorn Slough") {
      label <- "ES"
    }
    
    list(
      x = station_distances$distance[i],
      y = 1,  # Position above water surface 
      yref = "paper",
      text = paste0("<b>", station_distances$station[i], "</b>"),
      showarrow = FALSE,
      font = list(size = 12, color = "#1A1A40"),
      xanchor = "center",
      yanchor = "bottom"
    )
  })
  
  
  # Fix min and max values, otherwise taxa with only one observation do not follow colorscale
  # Force a range even for single values
  min_det <- min(bath_plot$detections)
  max_det <- max(bath_plot$detections)

  # Create plotly basemap 
p <- plot_ly() |> 
    # Bathymetry fill (tan area)
    add_polygons(
      x = c(transect_df$dist.km, rev(transect_df$dist.km)),
      y = c(transect_df$depth, rep(min(transect_df$depth) - 100, nrow(transect_df))),
      fillcolor = toRGB("burlywood", alpha = 0.9),
      
      line = list(color = 'black', width = 1),
      hoverinfo = 'none',
      showlegend = FALSE
    ) |>
    # Water layer (light blue - from surface to bathymetry)
    add_polygons(
      x = c(transect_df$dist.km, rev(transect_df$dist.km)),
      y = c(transect_df$depth, rep(0, nrow(transect_df))),
      fillcolor = "lightblue",
      line = list(width = 0),
      hoverinfo = 'none',
      showlegend = FALSE
    ) 

# Add markers if there is data for that organism & season
if(nrow(bath_plot) > 0) {
  
  # Check if there is only one observation:
  single_obs <- (max_det == min_det)
  
  # If there is only a single observation, set to the lower color
  if(single_obs) {
    # Single observation - use fixed red color
    p <- p |> 
      add_markers(
        x = as.numeric(bath_plot$distance),
        y = -as.numeric(bath_plot$depth_plot),  
        marker = list(
          color = "#FFF7BC",  # Fixed tomato red color
          showscale = TRUE,   # Show the colorbar
          size = 10, 
          opacity = 0.8,
          line = list(color = 'black', width = 1),
          colorbar = list(
            title = "Detections",
            tickmode = "array",
            tickvals = c(0.9),  # Single tick in the middle
            ticktext = c(as.character(min_det)),  # Shows "1" or whatever the value is
            ticklabelposition = "outside top" 
            
          ),
          # Create a dummy colorscale so the bar shows the right color
          colorscale = list(c(0, "#FFF7BC"), c(1, "#FFF7BC")),  # All red
          cmin = 0,
          cmax = 1
        ),
        hovertemplate = paste0("Depth: ", bath_plot$depth_label, "<br>",
                               "Detections: ", bath_plot$detections,
                               "<extra></extra>")
      )
  } else {
    
    # Otherwise do the normal colorscale
    p <-p |> 
      add_markers(
        x = jitter(as.numeric(bath_plot$distance), amount = 0.5),
        y = -as.numeric(bath_plot$depth_plot),  
        marker = list(
          color = as.numeric(bath_plot$detections),
          colorscale = list(c(0, "#FFF7BC"), c(0.5, "#FF6347"), c(1, "#8B0000")),
          showscale = TRUE,
          size = 10, 
          opacity = 0.8,
          line = list(color = 'black', width = 1), 
          colorbar = list(title = "Detections")
        ),
        hovertemplate = paste0("Depth: ", bath_plot$depth_label, "<br>",
                               "Detections: ", bath_plot$detections,
                               "<extra></extra>")
        
      )
    
    
  }
  
} else {
  
  # Empty plot with annotation 
  p <- p |>
    add_annotations(
      text = paste("No data for", species_fn, "in selected season(s)"),
      x = 0.5, y = 0.5,
      xref = "paper", yref = "paper",
      showarrow = FALSE,
      font = list(size = 14, color = "gray50")
    )
  
}  

# Create list of selected seasons
season_list <- paste(season_fn, collapse = ", ")

# Add layout

p <- p |> 
    layout(
      font = list(family = "Arial, sans-serif", size = 12),
      title = list(
        text = paste0("Taxa selected: ", species_fn, "\n", "Seasons selected: ", season_list),
        x = 0.1
      ),
      
      xaxis = list(title = "Distance from start of transect (km)",
                   fixedrange = FALSE),
      yaxis = list(
        title = "Depth (m)",
        range = c(-3000, 0),
        fixedrange = FALSE
      ),
      margin = list(t = 100), # top margin
      annotations = annotations_list,
      dragmode = 'zoom',
      hovermode = 'closest'
    ) |> 
    # Subtle horizontal lines at depth intervals
    config(scrollZoom = TRUE,
           displayModeBar = TRUE)
  
} # End function

