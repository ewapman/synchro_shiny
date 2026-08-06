# This script is to try and remake the plotly graph with the correct depth partitioning 

plotly_map <- function(data_fn, season_fn, species_fn, depth_fn) {
  
  if(species_fn == "All taxa") {
    
    # Subset environmental data to surface and subsurface
    envtl_data <- data_fn |> 
      filter(Season %in% season_fn) |>  # All selected seasons
      mutate(depth = as.numeric(as.character(depth))) |>
      filter(
        if (depth_fn == "Surface (0-20m)") {
          depth <= 20
        } else {
          depth > 20
        }
      ) |>
      group_by(Latitude, Longitude) |>  # Average by location
      summarize(
        temperature = mean(temperature, na.rm = TRUE),
        salinity = mean(salinity, na.rm = TRUE),
        oxygen = mean(oxygen, na.rm = TRUE),
        .groups = "drop"
      ) |>
      drop_na() 
    
    # Filter date for all selection ----
    leaflet_data_all <- data_fn |>
      filter(Season %in% season_fn) |> # This will be reactive --> filter leaflet_data_all by input date in server
      group_by(Latitude, Longitude) |>
      summarize(
        indicator_count = n_distinct(map_label), # size circles by number of indicator species at that point
        indicator_species_list = paste(sort(unique(map_label)), collapse = "<br>"), # This creates a list of species at a point to plot in the popup
        .groups = "drop") |> 
      left_join(envtl_data, by = c("Latitude", "Longitude")) 
    
    
    
    # Plot All taxa ----
    
    # Make plotly graph ---
    
    p <- plot_ly(leaflet_data_all,
                 lat = ~Latitude,
                 lon = ~Longitude,
                 customdata = ~paste(Latitude, Longitude, sep = "|"),
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
                     title = "Unique<br>Species",
                     orientation = "h",
                     x = 0,
                     xanchor = "left",
                     y = 0,
                     len = 0.4,
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
                   "<b>Unique indicator species:</b> ", indicator_count, "<br>",
                   if_else(
                     is.na(temperature),
                     "",
                     paste0(
                       "<b>Temperature:</b> ", round(temperature, 1), "°C<br>",
                       "<b>Salinity:</b> ", round(salinity, 1), " psu<br>",
                       "<b>Oxygen:</b> ", round(oxygen, 2), " ml/L"
                     )
                   )
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
    # Register the click event
    event_register(p, "plotly_click")
    
    return(p)
    
    
    
  } # End if statement
  
  
  
  else {
    
    # Get valid seasons for this species
    valid_seasons <- data_fn |>
      filter(map_label == species_fn) |>
      pull(Season) |>
      unique()
    
    
    # Filter to only valid seasons
    seasons_to_use <- season_fn[season_fn %in% valid_seasons]
    
    
    if(length(seasons_to_use) == 0) {
      return(
        plot_ly() |>
          add_trace(
            type = "scattermapbox",  # ← ADD THIS
            lat = c(36.74),
            lon = c(-122.16),
            mode = "markers",
            marker = list(opacity = 0)
          ) |>
          layout(
            mapbox = list(
              style = "white-bg",
              zoom = 8,
              center = list(lon = -122.16, lat = 36.74)
            ),
            annotations = list(
              text = "No data for this species in selected seasons",
              showarrow = FALSE,
              font = list(size = 16, color = "gray")
            )
          )
      )
    }
    
    
    # Subset environmental data for this using valid seasons
    envtl_data <- data_fn |> 
      filter(Season %in% seasons_to_use) |>
      mutate(depth = as.numeric(as.character(depth))) 
    
    
    
    # Apply depth filter
    if (depth_fn == "Surface (0-20m)") {
      envtl_data <- envtl_data |> filter(depth <= 20)
    } else {
      envtl_data <- envtl_data |> filter(depth > 20)
    } 
    
    envtl_data <- envtl_data |>
      group_by(Latitude, Longitude) |>
      summarize(
        temperature = mean(temperature, na.rm = TRUE),
        salinity = mean(salinity, na.rm = TRUE),
        oxygen = mean(oxygen, na.rm = TRUE),
        .groups = "drop"
      ) |>
      drop_na()
    
    
    # Filter species and date ----
    leaflet_data_subset <- data_fn |> 
      filter(Season %in% seasons_to_use,
             map_label == species_fn) |>
      group_by(Latitude, Longitude, map_label, date) |> # These will both be reactive
      summarize(
        sp_count = n(), # size circles by number of samples w/ that organism at that point
        .groups = "drop"
      ) 
    
    # Join environmental data if it has rows
    if(nrow(envtl_data) > 0) {
      leaflet_data_subset <- leaflet_data_subset |>
        left_join(envtl_data, by = c("Latitude", "Longitude"))
    }
    
    
    
    # Not All taxa have observations for a given date -- check to avoid errors
    if(nrow(leaflet_data_subset) == 0) {
      return(
        plot_ly() |>
          add_trace(
            type = "scattermapbox",  # ← ADD THIS
            lat = c(36.74),
            lon = c(-122.16),
            mode = "markers",
            marker = list(opacity = 0)
          ) |>
          layout(
            mapbox = list(
              style = "white-bg",
              zoom = 8,
              center = list(lon = -122.16, lat = 36.74)
            ),
            annotations = list(
              text = "No data for this species and date",
              showarrow = FALSE,
              font = list(size = 16, color = "gray")
            )
          )
      )
    }
    
    
    
    # Create palette with minimum range to avoid weird scaling ----
    actual_min <- min(leaflet_data_subset$sp_count, na.rm = TRUE)
    actual_max <- max(leaflet_data_subset$sp_count, na.rm = TRUE)
    
    
    range_width <- actual_max - actual_min
    min_range_width <- 4  # Adjust this value as needed
    
    if(range_width < min_range_width) {
      # Expand range to minimum width
      domain_min <- 1
      domain_max <- max(actual_max, min_range_width + 1)
    } else {
      # Use actual range
      domain_min <- as.integer(actual_min)
      domain_max <- as.integer(actual_max)
    }
    
    # Fix for single values - make them appear light
    if(actual_min == actual_max) {
      plot_min <- actual_min
      plot_max <- actual_min + 10  # Creates range so single value is light
    } else {
      plot_min <- domain_min
      plot_max <- domain_max
    }
    
    
    
    
    
    # Plot species subset ----
    # Check if there's only 1 row of data
    single_point <- nrow(leaflet_data_subset) == 1
    
    p <- plot_ly(leaflet_data_subset,
                 lat = ~Latitude,
                 lon = ~Longitude,
                 customdata = ~paste(Latitude, Longitude, sep = "|"),
                 #key = ~paste(Latitude, Longitude, sep = "_"),
                 type = "scattermapbox",
                 mode = "markers",
                 source = "indicator_map", 
                 
                 
                 marker = list(
                   size = 15,
                   color = if(single_point && actual_min == actual_max) "#FFFFCC" else ~sp_count,  # ← Force color for single point
                   colorscale = if(actual_min == actual_max) list(c(0, "#FFFFCC"), c(1, "#FFFFCC")) else "YlOrRd",
                   reversescale = if(actual_min == actual_max) FALSE else TRUE,
                   showscale = TRUE,
                   cauto = FALSE,
                   colorbar = list(
                     title = "Detections", 
                     orientation = "h",
                     x = 0,
                     xanchor = "left",
                     y = 0,
                     yanchor = "bottom",
                     len = 0.4,
                     thickness = 15,
                     bgcolor = "rgba(255,255,255,0)",
                     borderwidth = 0,
                     outlinewidth = 0,
                     tickmode = "array",
                     tickvals = if(actual_min == actual_max) {
                       c(actual_min)
                     } else {
                       pretty(c(domain_min, domain_max), n = 3)
                     },
                     ticktext = if(actual_min == actual_max) {
                       c(as.character(actual_min))
                     } else {
                       as.character(pretty(c(domain_min, domain_max), n = 3))
                     },
                     tickfont = list(size = 10),
                     tick0 = if(actual_min == actual_max) actual_min else NULL,
                     dtick = if(actual_min == actual_max) 1 else NULL
                   ),                             
                   opacity = 0.8,
                   line = list(color = 'white', width = 1),
                   cmin = if(actual_min == actual_max) actual_min else domain_min,
                   cmax = if(actual_min == actual_max) actual_min else domain_max
                 ),
                 hovertext = ~paste0(
                   "<b>Species:</b> ", map_label, "<br>",
                   "<b>Detections:</b> ", sp_count, "<br>",
                   "<b>Date:</b> ", date, "<br>",
                   if_else(
                     is.na(temperature),
                     "",
                     paste0(
                       "<b>Temperature:</b> ", round(temperature, 1), "°C<br>",
                       "<b>Salinity:</b> ", round(salinity, 1), " psu<br>",
                       "<b>Oxygen:</b> ", round(oxygen, 2), " ml/L"
                     )
                   )
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
    
    # Register the click event
    event_register(p, "plotly_click")
    
    return(p)
    
  } # End else
  
  
}

