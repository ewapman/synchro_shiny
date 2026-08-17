leaflet_map <- function(data_fn, season_fn, species_fn, depth_fn) {
  
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
    
    
    # Define palette ---- color by number of indicator taxa detected
    pal_1 <- colorNumeric(palette = "YlOrRd", domain = leaflet_data_all$indicator_count)
    
    # Plot All taxa ----
    
    # Make plotly graph ---
    
    # p <- plot_ly(leaflet_data_all,
    #              lat = ~Latitude,
    #              lon = ~Longitude,
    #              customdata = ~paste(Latitude, Longitude, sep = "|"),
    #              #key = ~paste(Latitude, Longitude, sep = "_"),
    #              type = "scattermapbox",
    #              mode = "markers",
    #              source = "indicator_map", 
    #              marker = list(
    #                size = 15,
    #                color = ~indicator_count,
    #                colorscale = "YlOrRd",
    #                reversescale = TRUE,
    #                showscale = TRUE,
    #                colorbar = list(
    #                  title = "Unique<br>Species",
    #                  orientation = "h",
    #                  x = 0,
    #                  xanchor = "left",
    #                  y = 0,
    #                  len = 0.3,
    #                  thickness = 15,
    #                  outlinecolor = "black",
    #                  outlinewidth = 1),
    #                
    #                opacity = 0.8,
    #                line = list(color = 'white', width = 1)
    #              ),
    #              hovertext = ~paste0(
    #                "<b>Unique indicator species:</b> ", indicator_count, "<br>",
    #                if_else(
    #                  is.na(temperature),
    #                  "",
    #                  paste0(
    #                    "<b>Temperature:</b> ", round(temperature, 1), "°C<br>",
    #                    "<b>Salinity:</b> ", round(salinity, 1), " psu<br>",
    #                    "<b>Oxygen:</b> ", round(oxygen, 2), " ml/L"
    #                  )
    #                )
    #              ),
    #              hoverinfo = "text"
    # ) |>
    #   layout(
    #     mapbox = list(
    #       style = "white-bg",
    #       zoom = 9.5,
    #       center = list(lon = -122.16, lat = 36.74),
    #       layers = list(list(
    #         below = 'traces',
    #         sourcetype = "raster",
    #         source = list("https://services.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}")
    #       ))
    #     ),
    #     margin = list(l = 0, r = 0, t = 0, b = 0),
    #     hoverlabel = list(
    #       align = "left",           # Push it to the side
    #       bgcolor = "rgba(255,255,255,0.9)",  # Slightly transparent
    #       bordercolor = "black",
    #       font = list(size = 11,
    #                   color = "black")
    #     )
    #   ) 
    # # Register the click event
    # event_register(p, "plotly_click")
    # 
    # return(p)
    # 
    
    
    # Make leaflet graph ---
    leaflet(leaflet_data_all) |>
      addProviderTiles(providers$Esri.OceanBasemap) |>
      addCircleMarkers(
        ~Longitude, ~Latitude,
        color = ~pal_1(indicator_count),
        popup = ~paste0("<b>Unique indicator species detections: </b>", indicator_count, "<br>",
                        if_else(
                          is.na(temperature),
                          "",
                          paste0(
                            "<b>Environmental data (avg):</b><br>",
                            "<b>Temperature: </b>", round(temperature, 1), "°C<br>",
                            "<b>Salinity: </b>", round(salinity, 1), " psu<br>",
                            "<b>Oxygen: </b>", round(oxygen, 2), " ml/L")
                        )),
        layerId = ~paste(Latitude, Longitude, sep = "_"),
        fillOpacity = 0.8,
        opacity = 1,
        radius = 5
      ) |>

      addLegendNumeric(
        pal = pal_1,
        values = ~indicator_count,
        #opacity = 0.7,
        title = "Number of unique indicator species detected",
        orientation = "horizontal",
        width = 150,
        height = 15,
        position = "bottomleft"

      ) # End Legend

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
          layout(
            title = "No data for this species in selected seasons",
            mapbox = list(
              style = "white-bg",
              zoom = 9.5,
              center = list(lon = -122.16, lat = 36.74)
            )
          )
      )
    }
    
    #If no valid seasons, show empty map temporarily -- do i need to do render leaflet here?
    if(length(seasons_to_use) == 0) {
      return(
        leaflet() |>
          addProviderTiles(providers$Esri.OceanBasemap) |>
          setView(lng = -122.16, lat = 36.74, zoom = 10)
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
    
    
    
    # # Not All taxa have observations for a given date -- check to avoid errors
    # if(nrow(leaflet_data_subset) == 0) {
    #   return(
    #     plot_ly() |>
    #       layout(
    #         title = "No data for this species and date",
    #         mapbox = list(
    #           style = "white-bg",
    #           zoom = 9.5,
    #           center = list(lon = -122.16, lat = 36.74)
    #         )
    #       )
    #   )
    # }
    if(nrow(leaflet_data_subset) == 0) {
      return(
        leaflet() |>
          addTiles() |>
          setView(lng = -122.16, lat = 36.74, zoom = 10) |>
          addPopups(lng = -122.16, lat = 36.74,
                    popup = "No data for this species and date.")
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
    
    
    pal_2 <- colorNumeric(palette = "YlOrRd", domain = c(domain_min, domain_max))
    
    # Jitter points to avoid direct overlap
    
    # leaflet_data_subset <- leaflet_data_subset |>
    #   arrange(sp_count) |>
    #   mutate(
    #     Longitude_jitter = jitter(Longitude, amount = 0.01),  # Adjust amount as needed
    #     Latitude_jitter = jitter(Latitude, amount = 0.01)
    #    
    #   ) 
    # Store original for layerId
    
    
    # Plot species subset ----
    
  #   p <- plot_ly(leaflet_data_subset,
  #                lat = ~Latitude,
  #                lon = ~Longitude,
  #                customdata = ~paste(Latitude, Longitude, sep = "|"),
  #                #key = ~paste(Latitude, Longitude, sep = "_"),
  #                type = "scattermapbox",
  #                mode = "markers",
  #                source = "indicator_map", 
  #                marker = list(
  #                  size = 15,
  #                  color = ~sp_count,
  #                  colorscale = "YlOrRd",
  #                  reversescale = TRUE,
  #                  showscale = TRUE,
  #                  colorbar = list(title = "Detections", 
  #                                  orientation = "h",
  #                                  x = 0,              # ← Left side (was 0.5)
  #                                  xanchor = "left",   # ← Anchor to left (was center)
  #                                  y = 0,              # ← Bottom
  #                                  yanchor = "bottom",
  #                                  len = 0.3,
  #                                  thickness = 15,
  #                                  outlinecolor = "black",
  #                                  outlinewidth = 1),
  #                  opacity = 0.8,
  #                  line = list(color = 'white', width = 1),
  #                  cmin = domain_min,
  #                  cmax = domain_max
  #                ),
  #                hovertext = ~paste0(
  #                  "<b>Species:</b> ", map_label, "<br>",
  #                  "<b>Detections:</b> ", sp_count, "<br>",
  #                  "<b>Date:</b> ", date, "<br>",
  #                  if_else(
  #                    is.na(temperature),
  #                    "",
  #                    paste0(
  #                      "<b>Temperature:</b> ", round(temperature, 1), "°C<br>",
  #                      "<b>Salinity:</b> ", round(salinity, 1), " psu<br>",
  #                      "<b>Oxygen:</b> ", round(oxygen, 2), " ml/L"
  #                    )
  #                  )
  #                ),
  #                hoverinfo = "text"
  #   ) |>
  #     layout(
  #       mapbox = list(
  #         style = "white-bg",
  #         zoom = 9.5,
  #         center = list(lon = -122.16, lat = 36.74),
  #         layers = list(list(
  #           below = 'traces',
  #           sourcetype = "raster",
  #           source = list("https://services.arcgisonline.com/ArcGIS/rest/services/Ocean/World_Ocean_Base/MapServer/tile/{z}/{y}/{x}")
  #         ))
  #       ),
  #       margin = list(l = 0, r = 0, t = 0, b = 0),
  #       hoverlabel = list(
  #         align = "left",           # Push it to the side
  #         bgcolor = "rgba(255,255,255,0.9)",  # Slightly transparent
  #         bordercolor = "black",
  #         font = list(size = 11,
  #                     color = "black")
  #       )
  #     ) 
  #   
  #   # Register the click event
  #   event_register(p, "plotly_click")
  #   
  #   return(p)
  #   
  # } # End else
    leaflet(leaflet_data_subset) |>
      addProviderTiles(providers$Esri.OceanBasemap) |>
      #setView(lng = -122.16, lat = 36.74, zoom = 10) |>
      addCircleMarkers(
        ~Longitude, ~Latitude,
        #radius = ~sp_count,
        color = ~pal_2(sp_count),
        fillOpacity = 0.8,
        opacity = 1,
        radius = 5,
       # layerId = ~lat_lon_id,  # ← ADD THIS - use original coords as ID
        popup = ~paste0(
          "<b>Species:</b> ", map_label, "<br>",
          "<b>Detections:</b> ", sp_count, "<br>",
          "<b>Date:</b> ", date, "<br>",
          if_else(
            is.na(temperature),
            "",
            paste0(
              "<b>Environmental data (avg):</b><br>",
              "<b>Temperature: </b>", round(temperature, 1), "°C<br>",
              "<b>Salinity: </b>", round(salinity, 1), " psu<br>",
              "<b>Oxygen: </b>", round(oxygen, 2), " ml/L"))
        )
      ) |>
      addLegendNumeric(
        pal = pal_2,
        values = ~c(domain_min, domain_max),
        title = "Number of samples with organism",
        orientation = "horizontal",
        width = 150,
        height = 15,
        position = "bottomleft",

      ) # End Legend



  } # End else
  
  
  
  
  # Make bar chart element
  
  
  
}

