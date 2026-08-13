bath_graph <- function(data_fn, season_fn, species_fn) {
  
  
  bath <- data_fn |>
    select(map_label, Station, depth, Season, sample_id, Latitude, Longitude) |>
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
  # if(species_fn == "All taxa"){
  #   bath_summary <- bath |>
  #     filter(Season %in% season_fn ) |> # REACTIVE: This will be reactive + loop through the different season selections
  #     #distinct(sample_id, Station, depth_plot) |> 
  #     group_by(Station, depth_plot) |>  # For all taxa, want # of unique species
  #     summarize(samples = n_distinct(map_label), .groups = "drop") |>
  #     mutate(
  #       depth_label = if_else(
  #         depth_plot == chl_max_median, "Deep Chlorophyll Max", as.character(depth_plot)
  #       )
  #     )
  #   
  #   bath_plot <- bath_summary |> 
  #     select(Station, samples, depth_plot, depth_label) |> 
  #     left_join(station_distances, by = c("Station" = "station")) |> 
  #     mutate(depth = as.numeric(as.character(depth_plot)),
  #            distance = if_else(Station == "Elkhorn Slough", 0, distance)) 
  #   
  # } else {
    bath_summary <- bath |>
      filter(Season %in% season_fn ) |> # REACTIVE: This will be reactive + loop through the different season selections
      filter(map_label == species_fn) |> # REACTIVE: This will be reactive and depend on species input
      distinct(sample_id, Station, depth_plot) |> # Avoid duplicate ASVID's
      group_by(Station, depth_plot) |>  # Want number at each station and at each depth
      summarize(samples = n(), .groups = "drop") |>
      mutate(
        depth_label = if_else(
          depth_plot == chl_max_median, "Deep Chlorophyll Max", as.character(depth_plot)
        )
      )
    
    # Get total number of samples at each station/depth combo (total_samples)
    total_samples_by_location <- bath |>
      filter(Season %in% season_fn) |>
      distinct(sample_id, Station, depth_plot) |>
      group_by(Station, depth_plot) |>
      summarize(total_samples = n(), .groups = "drop")
    
    # Join distances and calculate relative abundance
    bath_plot <- bath_summary |> 
      select(Station, samples, depth_plot, depth_label) |> 
      left_join(station_distances, by = c("Station" = "station")) |> 
      left_join(total_samples_by_location, by = c("Station", "depth_plot")) |> 
      mutate(depth = as.numeric(as.character(depth_plot)),
             distance = if_else(Station == "Elkhorn Slough", 0, distance)) |> 
      mutate(relative_abundance = (samples / total_samples) * 100)
  #}

  
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
if(species_fn == "All taxa") {
  # Don't show bath graph for All taxa - add message
  p <- p |>
    add_annotations(
      text = "Select a specific taxon",
      x = 0.5, y = 0.5,
      xref = "paper", yref = "paper",
      showarrow = FALSE,
      font = list(size = 14, color = "gray50")
    )
  
} else if(nrow(bath_plot) > 0) {
  
  # If there is only a single observation, create dummy points
  if(nrow(bath_plot) == 1) {
    dummy_points <- data.frame(
      Station = c("dummy1", "dummy2"),
      samples = c(NA, NA),
      depth_plot = c(0, 0),
      depth_label = c("", ""),
      distance = c(0, 0),
      total_samples = c(NA, NA),
      depth = c(0, 0),
      relative_abundance = c(0, 100)
    )
    bath_plot <- bind_rows(bath_plot, dummy_points)
  }
  
  # Set variables for specific species
  color_var <- bath_plot$relative_abundance
  color_title <- "% of samples"
  cmin_val <- 0
  cmax_val <- 100
  tick_vals <- pretty(c(0, 100), n = 5)
  tick_text <- paste0(as.character(tick_vals), "%")
  opacity_val <- ~ifelse(is.na(bath_plot$samples), 0, 0.8)
  colorscale_choice <- "Viridis"
  hover_text <- ifelse(is.na(bath_plot$samples), "",
                       paste0("Depth: ", bath_plot$depth_label, "<br>",
                              "Samples: ", bath_plot$samples, " of ", bath_plot$total_samples, 
                              " (", round(bath_plot$relative_abundance, 2), "%)"))
  
  # Add markers
  p <- p |> 
    add_markers(
      x = jitter(as.numeric(bath_plot$distance), amount = 0.5),
      y = -as.numeric(bath_plot$depth_plot),  
      marker = list(
        color = color_var,
        reversescale = TRUE,
        colorscale = colorscale_choice,
        cauto = FALSE,
        showscale = TRUE,
        size = 10, 
        opacity = opacity_val,
        line = list(color = "black", width = 1), 
        cmin = cmin_val,
        cmax = cmax_val,
        colorbar = list(
          title = color_title,
          tickmode = "array",
          tickvals = tick_vals,
          ticktext = tick_text,
          len = 0.5,
          thickness = 15
        )
      ),
      hovertemplate = paste0(hover_text, "<extra></extra>")
    )
  
} else {
  # No data for this species in selected seasons
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
                   fixedrange = FALSE,
                   autorange = "reversed"),
      yaxis = list(
        title = "Depth (m)",
        range = c(-3000, 50),
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

