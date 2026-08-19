

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
    
    # Change to DCM to match data selection
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
    
    # Get unique species at each point
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
      left_join(station_coords, by = c("Station" = "station")) 
     
    
    # Get number of samples/temp info for all the depth selections 
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
    
    # Create hover table
    depth_hover_table_all <- depth_envtl_table_all |>
      mutate(
        depth = factor(depth, levels = c("0", "DCM", "150", "300"))  # Set order so DCM isn't last
      ) |>
      arrange(Station, depth) |>
      group_by(Station) |>
      summarize(
        hover_table = paste0(
          "<b>By Depth (m):</b><br>",
          paste0(
            "<b>", depth, ":</b> ", 
            "Temp: ", ifelse(is.na(temperature), "NA", paste0(temperature, "°C")), " | ",  
            "Salinity: ", ifelse(is.na(salinity), "NA", salinity), " | ",  
            "O₂: ", ifelse(is.na(oxygen), "NA", oxygen),  
            collapse = "<br>"
          )
        ),
        .groups = "drop"
      )
    
    # Join hover table back to data:
    leaflet_data_all <- leaflet_data_all |>
      left_join(depth_hover_table_all, by = c("Station"))
    
    
    
    # Plot All taxa ----
    
    p <- plot_ly(leaflet_data_all,
                 lat = ~station_lat,
                 lon = ~station_lon,
                 customdata = ~Station,
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
                     title = list(text = "Unique<br>Taxa", font = list(family = "Arial", size = 14)),
                     orientation = "h",
                     x = 0,
                     xanchor = "left",
                     y = 0,
                     len = 0.5,
                     thickness = 15,
                     bgcolor = "rgba(255,255,255,0)",  # Transparent background
                     borderwidth = 0,                
                     outlinewidth = 0,
                     tickmode = "array",                  
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
                     tickfont = list(size = 13, family = "Arial")),                      
                   
                   opacity = 0.8,
                   line = list(color = 'white', width = 1)
                 ),
                 hovertext = ~paste0(
                   "<b>Station:</b> ", Station, "<br><br>",
                   "<b>Season(s):</b> ", seasons, "<br><br>",
                   "<b>Unique taxa detected:</b> ", indicator_count, "<br><br>",
                   hover_table
                 ),
                 hoverinfo = "text"
    ) |>
      layout(
        font = list(family = "Arial, sans-serif", size = 12),
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
          align = "left",           # Push hoverbox to the side on hover
          bgcolor = "rgba(255,255,255,0.9)",  # Slightly transparent
          bordercolor = "black",
          font = list(size = 13, family = "Arial, sans-serif", color = "black")
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
      filter(Season %in% season_fn, # Make reactive
             depth %in% depth_fn, 
             map_label == species_fn) |> 
      group_by(Station) |> # Group by station only to have one point per station
      summarize(
        sample_count = n_distinct(sample_id), # number of samples with that organism
        depths_detected = paste(sort(unique(depth)), collapse = ", "),
        seasons = paste(sort(unique(Season)), collapse = ", "),
        .groups = "drop"
      ) 
    
    # Total number of samples at that location
    leaflet_data_subset_totals <- data_dcm |> 
      filter(Season %in% season_fn) |> 
      group_by(Station) |> # CHANGED TO GROUP BY STATION ONLY (one point per station)
      summarize(
        total_count = n_distinct(sample_id), 
        .groups = "drop"
      ) 
    
    # Join: 
    leaflet_data_subset <- leaflet_data_subset |>
      left_join(leaflet_data_subset_totals, by = c("Station")) |> 
      mutate(
        relative_abundance = (sample_count / total_count) * 100
      ) |> 
      # Join station coordinates
      left_join(station_coords, by = c("Station" = "station")) 

    
    # Make table for hover: 
    depth_envtl_table <- data_dcm |> 
      filter(Season %in% season_fn,
             depth %in% depth_fn,
             map_label == species_fn) |>
      distinct(sample_id, Station, depth, temperature, salinity, oxygen) |> 
      group_by(Station, depth) |> 
      summarize(
        samples = n_distinct(sample_id),
        temperature = round(mean(temperature, na.rm = TRUE), 1),
        salinity = round(mean(salinity, na.rm = TRUE), 1),
        oxygen = round(mean(oxygen, na.rm = TRUE), 1),
        .groups = "drop"
      ) 
    
    
    # Make hover table:
    depth_hover_tables <- depth_envtl_table |>
      mutate(
        depth = factor(depth, levels = c("0", "DCM", "150", "300"))  # Set order
      ) |>
      arrange(Station, depth) |> 
      group_by(Station) |> 
      summarize(
        hover_table = paste0(
          "<b>By Depth (m):</b><br>",
          paste0(
            "<b>", depth, ":</b> ", samples, " samples | ",
            "Temp: ", ifelse(is.na(temperature), "NA", paste0(temperature, "°C")), " | ", 
            "Salinity: ", ifelse(is.na(salinity), "NA", salinity), " | ",  
            "O₂: ", ifelse(is.na(oxygen), "NA", oxygen),  
            collapse = "<br>"
          )
        ),
        .groups = "drop"
      )
    
    # Join back to subset data: 
    leaflet_data_subset <- leaflet_data_subset |>
      left_join(depth_hover_tables, by = c("Station")) # Change to station
    

    # Plot species subset ----
    
    # Create dummy points so that taxa with one point will map to scale correctly: 
    if(nrow(leaflet_data_subset) == 1) {
      dummy_points <- data.frame(
        Station = c(NA, NA),
        station_lat = c(0, 0),
        station_lon = c(0, 0), 
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
                 type = "scattermapbox",
                 mode = "markers",
                 source = "indicator_map", 
                 
                 
                 marker = list(
                   size = 15,
                   color = ~relative_abundance,
                   colorscale = "Viridis",
                   reversescale = TRUE,
                   showscale = TRUE,
                   cauto = FALSE,
                   colorbar = list(
                     title = list(text = "% of<br>Station Samples", font = list(family = "Arial", size = 14)),
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
                     
                     tickangle = 0,
                     tickfont = list(size = 13, family = "Arial")
                     
                   ),                             
                   opacity = 0.8,
                   line = list(color = 'white', width = 1),
                   cmin = 0, 
                   cmax = 100 
                 ),
                 hovertemplate = ~paste0(
                   "<b>Station:</b> ", Station, "<br><br>",
                   "<b>Unique taxa: </b> 1 ", "<br><br>",
                   "<b>Season(s):</b> ", seasons, "<br><br>", 
                   "<b>Samples: </b>", sample_count," of ", total_count, " total samples at this station", " (", round(relative_abundance, 2), "%", ")", "<br><br>",
                   hover_table,
                   "<extra></extra>"
                 ) 
                 
    ) |>
      
      layout(
        mapbox = list(
          style = "white-bg", # white background
          zoom = 8,
          center = list(lon = -122.16, lat = 36.74),
          layers = list(list(
            below = 'traces', # tiles below points
            sourcetype = "raster", # ocean basemap
            source = list("https://services.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}")
          ))
        ),
        margin = list(l = 0, r = 0, t = 0, b = 0),
        hoverlabel = list(
          align = "left",  # Push it to the side on hover
          bgcolor = "rgba(255,255,255,0.9)",  # Slightly transparent
          bordercolor = "black",
          font = list(size = 13, family = "Arial, sans-serif", color = "black")
        )
      ) 
    p
    # Register the click event
    event_register(p, "plotly_click")
    
    return(p)
    
  } # End else
  
  
}

