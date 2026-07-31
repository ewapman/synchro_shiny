# server instructions ----
server <- function(input, output, session) {
  # Move selection here --- server is better at handling a large list so moved from ui.R after error
  
  # Define input selection options ----
  # Additional code to grey out seasons and depths that don't have data (COME BACK TO THIS) ----
  
  # Season grey out
  update_season_picker <- function(species_selection) {
    
    # Get all possible seasons
    all_seasons <- sort(unique(map_data$Season))
    
    
    # Filter data based on species selection
    if(species_selection == "All species") {
      filtered_data <- map_data # no filtering
    }
    else {
      filtered_data <- map_data |> # filter to species selected
        filter(map_label == species_selection)
    }
    
    
    # Find dates that have observations in the filtered data
    seasons_with_data <- filtered_data |>
      pull(Season) |> # pull to extract single column and put into list (drop df structure)
      unique() |> # only unique seasons
      sort() # order seasons
    
    
    # Determine which dates to disable
    seasons_to_disable <- !all_seasons %in% seasons_with_data # extract the dates in all dates that don't have data
    
    # Update the picker
    updatePickerInput(
      session,
      "season_input",
      choices = all_seasons,
      selected = if(length(seasons_with_data) > 0) seasons_with_data else NULL, # took out seasons_with_data[1]
      choicesOpt = list(
        disabled = seasons_to_disable,
        style = ifelse(seasons_to_disable, 
                       "color: #ccc;",
                       "")
      )
    )
  }
  
  
  # Depth grey out 
  update_depth_picker <- function(species_selection, season_selection) {
    
    # All possible depth options
    all_depths <- c("Surface (0-20m)", "Subsurface (>20m)")
    
    # Filter data based on species and season
    if(species_selection == "All species") {
      filtered_data <- map_data |>
        filter(Season %in% season_selection)
    } else {
      filtered_data <- map_data |>
        filter(map_label == species_selection,
               Season %in% season_selection)
    }
    
    # Check which depths have data
    filtered_data <- filtered_data |>
      mutate(depth = as.numeric(as.character(depth))) |>
      filter(!is.na(depth))
    
    has_shallow <- any(filtered_data$depth <= 20, na.rm = TRUE)
    has_deep <- any(filtered_data$depth > 20, na.rm = TRUE)
    
    depths_with_data <- c()
    if(has_shallow) depths_with_data <- c(depths_with_data, "Surface (0-20m)")
    if(has_deep) depths_with_data <- c(depths_with_data, "Subsurface (>20m)")
    
    # Determine which depths to disable
    depths_to_disable <- !all_depths %in% depths_with_data
    
    # Update the picker
    updatePickerInput(
      session,
      "depth_map_input",
      choices = all_depths,
      selected = if(length(depths_with_data) > 0) depths_with_data[1] else "Surface (0-20m)",
      choicesOpt = list(
        disabled = depths_to_disable,
        style = ifelse(depths_to_disable, 
                       "color: #ccc;",
                       "")
      )
    )
  }
  
  
  
  
  #------------------------------------------------------------------------
  # Species selection options --
  updateSelectInput(
    session, 
    "species_input",
    choices = c("All species", sort(unique(map_data$map_label)))
  )
  
  
  # Season selection options -- depth plot 
  updateSelectInput(
    session, 
    "season_depth_input",
    choices = unique(map_data$Season))
  
  
  # Species selection options -- species line graph plot
  updateSelectInput(
    session, 
    "species_season_line_input",
    choices = unique(map_data$map_label))
  
  
  # Date selection options: call function originally for All species option
  update_season_picker("All species")
  
  # Depth selection: call function originally for All species option
  update_depth_picker("All species", "Spring 2025")
  
  # Update dates when the species is changed
  observeEvent(input$species_input, {
    update_season_picker(input$species_input)
  })
  
  # Update depths when species or season changes
  observeEvent(c(input$species_input, input$season_input), {
    req(input$season_input)
    update_depth_picker(input$species_input, input$season_input)
  })
  
  
  
  #-----------------------------------------------------------------------------# 
  # Render indicator species plot
  output$indicator_sp_map <- renderLeaflet({
    
    
    
    #-------------------- Plot ---------------------------#
    if(input$species_input == "All species") {
      # Subset environmental data
      envtl_data <- map_data |> 
        filter(Season %in% input$season_input) |>  # All selected seasons
        mutate(depth = as.numeric(as.character(depth))) |>
        filter(
          if (input$depth_map_input == "Surface (0-20m)") {
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
      
      # Filter date ----
      leaflet_data_all <- #reactive({
        map_data |>
        filter(Season %in% input$season_input) |> # This will be reactive --> filter leaflet_data_all by input date in server
        group_by(Latitude, Longitude) |>
        summarize(
          indicator_count = n_distinct(map_label), # size circles by number of indicator species at that point
          indicator_species_list = paste(sort(unique(map_label)), collapse = "<br>"), # This creates a list of species at a point to plot in the popup
          .groups = "drop") |> 
        left_join(envtl_data, by = c("Latitude", "Longitude")) 
      # })
      
      # Define palette ---- SHOULD THIS BE HERE OR IN GLOBAL? (Also might not need palette )
      pal_1 <- colorNumeric(palette = "YlOrRd", domain = leaflet_data_all$indicator_count)
      
      # Plot all species ----
      leaflet(leaflet_data_all) |>
        addProviderTiles(providers$Esri.OceanBasemap) |>
        #setView(lng = -122.16, lat = 36.74, zoom = 10) |>
        addCircleMarkers(
          ~Longitude, ~Latitude,
          #radius = ~indicator_count,
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
          #"<b>Species:</b><br>",
          #indicator_species_list), 
          layerId = ~paste(Latitude, Longitude, sep = "_"),
          fillOpacity = 0.8,
          opacity = 1, 
          radius = 5
        ) |> # End addCircle Markers
        
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
      req(input$season_input, input$species_input, input$depth_map_input)
      
      # Get valid seasons for this species
      valid_seasons <- map_data |>
        filter(map_label == input$species_input) |>
        pull(Season) |>
        unique()
      
      
      # Filter to only valid seasons
      seasons_to_use <- input$season_input[input$season_input %in% valid_seasons]
      
      # If no valid seasons, show empty map temporarily
      if(length(seasons_to_use) == 0) {
        return(
          leaflet() |>
            addProviderTiles(providers$Esri.OceanBasemap) |>
            setView(lng = -122.16, lat = 36.74, zoom = 10)
        )
      }
      
      
      # Subset environmental data for THIS SPECIES using valid seasons
      envtl_data <- map_data |> 
        filter(Season %in% seasons_to_use) |>
        mutate(depth = as.numeric(as.character(depth))) 
      
      # Apply depth filter
      if (input$depth_map_input == "Surface (0-20m)") {
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
      leaflet_data_subset <- #reactive({
        map_data |> 
        filter(Season %in% seasons_to_use,
               map_label == input$species_input) |>
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
      
      
      
      #}) # End second reactive
      
      #Not all species have observations for a given date -- check to avoid errors
      if(nrow(leaflet_data_subset) == 0) {
        return(
          leaflet() |>
            addTiles() |>
            setView(lng = -122.16, lat = 36.74, zoom = 10) |>
            addPopups(lng = -122.16, lat = 36.74,
                      popup = "No data for this species and date.")
        )
        
      }
      
      # Fix palette -- avoid weird scale when values = 1
      # Make palette - handle case where all values are the same
      # sp_count_range <- range(leaflet_data_subset$sp_count, na.rm = TRUE)
      # 
      # if(sp_count_range[1] == sp_count_range[2]) {
      #   # All values are the same - create a range around that value
      #   single_value <- sp_count_range[1]
      #   pal_2 <- colorNumeric(palette = "YlOrRd", domain = c(1, single_value + 1))
      # } else {
      #   # Normal case - values have a range
      #   pal_2 <- colorNumeric(palette = "YlOrRd", domain = leaflet_data_subset$sp_count)
      # }
      
      # Make palette ---- SHOULD THIS BE HERE OR IN GLOBAL?
      #pal_2 <- colorNumeric(palette = "YlOrRd", domain = c(1, max(leaflet_data_subset$sp_count, na.rm = TRUE) + 1)) #leaflet_data_subset$sp_count)
      
      # CREATE PALETTE - ensure range covers actual data ----
      # actual_min <- min(leaflet_data_subset$sp_count, na.rm = TRUE)
      # actual_max <- max(leaflet_data_subset$sp_count, na.rm = TRUE)
      # 
      # # Set minimum range width to avoid weird colors with 1-2 observations
      # range_width <- actual_max - actual_min
      # min_range_width <- 4  # Adjust as needed
      # 
      # if(range_width < min_range_width) {
      #   # Expand range symmetrically or set to fixed minimum
      #   domain_min <- 1
      #   domain_max <- max(actual_max, min_range_width + 1)
      # } else {
      #   # Use actual range with small buffer
      #   domain_min <- actual_min
      #   domain_max <- actual_max
      # }
      
      
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
      #pal_2 <- colorNumeric(palette = "YlOrRd", domain = leaflet_data_subset$sp_count)
      
      # Plot species subset ----
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
    
    
  }) # End renderPlot - Map
  
  
  # Add the bar chart element - appear on click 
  observeEvent(input$indicator_sp_map_marker_click, {
    
    
    
    if(input$species_input == "All species") { # Only want for all species selection
      
      clicked <- input$indicator_sp_map_marker_click
      # Filter data:
      bar_plot_data <- map_data |> 
        filter(Season %in% input$season_input,
               near(Latitude, clicked$lat),
               near(Longitude, clicked$lng)) |>
        group_by(map_label) |> 
        summarize(
          sp_count = n(), # of sp detections at that lat/long
          .groups = "drop") |> 
        arrange(sp_count) |> # reorder 
        mutate(percent = (sp_count / sum(sp_count)) * 100) |> 
        mutate(
          percent_bin = cut(percent,
                            breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
                            labels = c("<1%", "1-2%", "2-5%", "5-10%", "10-20%", "20-50%", ">50%"))
        )
      # Make palette
      bar_palette <- c("<1%" = "#43c197",
                       "1-2%"= "#3da48c",
                       "2-5%" = "#368881",
                       "5-10%" = "#306b76",
                       "10-20%" = "#294e6a",
                       "20-50%" = "#23325f",
                       ">50%" = "#1c1554")
      
      
      # Render plot:
      output$species_bar_plot <- renderPlotly({
        plot_ly(bar_plot_data,
                x = 1, # makes it so it can be stacked
                y = ~percent,
                type = 'bar',
                #barnorm = 'percent',
                color = ~percent_bin,  # Color by count
                colors = bar_palette,
                text = ~map_label,           # Species names
                textposition = 'inside',      # Put text inside bars
                textfont = list(
                  color = 'white',            # White text
                  size = 10                   # Font size
                ),
                customdata = ~map_label, 
                marker = list(
                  line = list(color = 'white', width = 1)
                ),
                hovertemplate = paste0(
                  "<b>%{customdata}</b><br>",  # <-- USE customdata instead of text
                  "Percent: %{y:.2f}%<br>",
                  "<extra></extra>"
                )) %>%
          layout(
            barmode = 'stack',
            xaxis = list(
              title = "",           # Remove x-axis title
              showticklabels = FALSE  # Hide "rep(Species, nrow...)"
            ),
            yaxis = list(
              title = "Percentage of Samples (%)"  # Y-axis label
            ),
            showlegend = FALSE,
            hoverlabel = list(
              font = list(
                color = "white"
              )
            )# <-- CHANGE TO TRUE
            # legend = list(
            #   title = list(text = "Percentage Range"),
            #   orientation = "v",
            #   x = 1.02,  # Position to the right
            #   y = 1
            # )
          )
        
        
      }) # End renderPlotly 
      
      
    } # End if
    
    else {
      clicked <- input$indicator_sp_map_marker_click
      
      bar_plot_data_sp <- map_data |> 
        filter(Season %in% input$season_input, 
               near(Latitude, clicked$lat),
               near(Longitude, clicked$lng)) |>
        group_by(map_label) |> 
        summarize(
          sp_count = n(), # of sp detections at that lat/long
          .groups = "drop") |> 
        arrange(sp_count) |> # reorder 
        mutate(percent = (sp_count / sum(sp_count)) * 100) |> 
        mutate(
          percent_bin = cut(percent,
                            breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
                            labels = c("<1%", "1-2%", "2-5%", "5-10%", "10-20%", "20-50%", ">50%"))
        ) |> 
        arrange(desc(percent))
      
      bar_plot_data_sp <- bar_plot_data_sp %>%
        mutate(map_label = fct_rev(factor(map_label, levels = unique(map_label)))) 
      
      
      unique_labels <- levels(bar_plot_data_sp$map_label)
      
      
      # Make palette
      bar_palette_sp <- ifelse(unique_labels %in% input$species_input, "#3da48c", "lightgrey")
      names(bar_palette_sp) <- unique_labels
      
      
      # Render plot:
      
      output$species_bar_plot <- renderPlotly({
        plot_ly(bar_plot_data_sp,
                x = 1, # makes it so it can be stacked
                y = ~percent,
                type = 'bar',
                color = ~map_label,  # color by label not count
                colors = bar_palette_sp,
                text = ~map_label,           # Species names
                textposition = 'inside',      # Put text inside bars
                textfont = list(
                  color = 'white',            # White text
                  size = 10                   # Font size
                ),
                customdata = ~map_label, 
                marker = list(
                  line = list(color = 'white', width = 1)
                ),
                hovertemplate = paste0(
                  "<b>%{customdata}</b><br>",  # <-- USE customdata instead of text
                  "Percent: %{y:.2f}%<br>",
                  "<extra></extra>"
                )) %>%
          layout(
            barmode = 'stack',
            xaxis = list(
              title = "",           # Remove x-axis title
              showticklabels = FALSE  # Hide "rep(Species, nrow...)"
            ),
            yaxis = list(
              title = "Percentage of Samples (%)"  # Y-axis label
            ),
            showlegend = FALSE,
            hoverlabel = list(
              font = list(
                color = "white"
              )
            )
          )
      }) # End render plotly
    } # End else
    
  }) # End observeEvent
  
  # Render depth plot ----
  
  # Filter data - relative abundance at each depth
  output$depth_plot <- renderGirafe({ 
    depth_data <- map_data |> 
      select(map_label, depth, abundance, Season) |> 
      filter(Season %in% input$season_depth_input) |> 
      mutate( # Do this to combine all chlorophyll max obs
        depth = as.numeric(as.character(depth)))
    
    min_depth <-  min(depth_data$depth[depth_data$depth > 0], na.rm = TRUE)
    max_depth_below_150 <- max(depth_data$depth[depth_data$depth < 150], na.rm = TRUE)
    chl_max_median <- median(c(min_depth, max_depth_below_150))
    
    
    depth_data <- depth_data |> 
      mutate(
        depth_plot = if_else(
          depth > 0 & depth <= max_depth_below_150, chl_max_median, depth) )|> 
      group_by(map_label, depth_plot, Season) |> 
      summarize(sp_depth = sum(abundance), .groups = "drop") |> 
      group_by(depth_plot) |> 
      mutate(
        relative_abundance = (sp_depth / sum(sp_depth)) * 100) |> 
      ungroup() |> 
      mutate(
        depth_label = if_else(
          depth_plot == chl_max_median, "Chlorophyll Max", as.character(depth_plot)
        )
      ) |> 
      mutate(
        map_label = if_else(
          str_detect(map_label, "^[A-Z][a-z]+ [a-z]+ \\("),  # Two words before parentheses
          str_replace(map_label, "^([A-Z])[a-z]+ ([a-z]+)", "\\1. \\2"),  # Abbreviate genus
          map_label  # Keep as-is (only one word before parentheses)
        )) 
    
    # Calculate average depth per species (weighted by abundance)
    species_order <- depth_data |>
      group_by(map_label) |>
      summarize(avg_depth = weighted.mean(depth_plot, sp_depth), .groups = "drop") |>
      arrange(avg_depth) |>  # Surface species first
      pull(map_label)
    
    # Convert to factor with this order
    depth_data <- depth_data |>
      mutate(map_label = factor(map_label, levels = species_order))
    
    
    
    
    depth_data$depth_plot <- as.numeric(depth_data$depth_plot)
    
    p <- ggplot(depth_data, aes(x = map_label, y = depth_plot, size = relative_abundance)) +
      
      annotate("rect",
               xmin = 0.5,  # Just past y-axis
               xmax = Inf,  # To the right edge
               ymin = min_depth,   # Top of Chl Max zone 
               ymax = max_depth_below_150,   # Bottom of Chl Max zone 
               fill = "lightgreen", 
               alpha = 0.2) +
      
      
      geom_point_interactive(alpha = 0.5, color = "darkblue",  # add back _interactive and aes
                             aes(tooltip = paste0(
                               map_label, "\n",
                               "Depth: ", depth_label, "m\n",
                               "Relative Abundance: ",
                               if_else(relative_abundance < 0.01,
                                       paste0(format(relative_abundance, scientific = TRUE, digits = 2), "%"),
                                       sprintf("%.2f%%", relative_abundance)))))+
      scale_size(range = c(1, 12), name="Relative Abundance") +
      labs(x = "Indicator Species", 
           y = "Depth (m)",
           title = "Relative abundance of species at each depth",
           subtitle = paste0("Green zone = Deep Chlorophyll Max (", min_depth, "-", max_depth_below_150, ")")) +
      theme_bw(base_size = 14) +
      scale_y_reverse(
        breaks = c(0, max_depth_below_150, 150, 300),
        limits = c(300, 0)) +
      # scale_y_break(c(-20, -150), scales = 0.5) +  # Break between 20-150m
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
            legend.position = "none") +
      theme(axis.title.y = element_text(
        margin = margin(r = 20),
        vjust = 2),
        plot.subtitle = element_text(color = "darkgreen"),
        axis.title.x = element_text(
          margin = margin(t = 20)
        )
      ) +
      theme(
        #panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "gray90"),  # Keep major gridlines
        panel.grid.minor.y = element_blank(),  # Remove minor gridlines
        panel.grid.major.x = element_line(linewidth = 0.4)
        
      )
    
    
    
    girafe(ggobj = p,
           width_svg = 8,
           height_svg = 11,
           options = list(
             opts_hover(css = "fill:orange;stroke:black;"),
             opts_toolbar(saveaspng = FALSE)
           ))
    
  }) # End render depth plot
  
  # Render species line graph ----
  
  output$species_line_plot <- renderGirafe({
    
    # Plots for % of each species: relative abundance for each season (# sp out of total detections that season)----
    
    # Define season order first
    season_order <- c("Spring 2025", "Summer 2025", "Autumn 2025")
    
    
    # Data wrangling
    sp_plot_data <- map_data |>
      select(map_label, Season, abundance) |> 
      group_by(map_label, Season) |> 
      summarize(total_sp_season = sum(abundance), .groups = "drop") |> 
      complete(map_label, Season = season_order, fill = list(total_sp_season = 0)) |> 
      group_by(Season) |> 
      mutate(relative_abundance_percent = (total_sp_season / sum(total_sp_season)) * 100) |> 
      ungroup() |> 
      mutate(Season = factor(Season, levels = season_order)) |> 
      filter(map_label %in% input$species_season_line_input)   # Make interactive 
    
    
    # Graph
    
    sp_plot <- ggplot(sp_plot_data, aes(x = Season, y = relative_abundance_percent, group = 1)) +
      geom_line(color = "darkgrey") +
      geom_point_interactive(color = "black", fill = "#69b3a2", size = 3, shape = 21,
                             aes(tooltip = paste0(
                               map_label, "\n",
                               "Relative Abundance: ", round(relative_abundance_percent, 3), "%")))+ 
      theme_minimal() +
      labs(x = "Season", 
           y = "Relative abundance (%)",
           title = paste0("Relative abundance of ", sp_plot_data$map_label, " per season")) +
      theme(
        axis.title.x = element_text(margin = margin(t = 20)),
        axis.title.y = element_text(margin = margin(r = 20))
      )
    
    
    girafe(ggobj = sp_plot,
           width_svg = 8,
           height_svg = 5,
           options = list(
             opts_hover(css = "fill:orange;stroke:black;"),
             opts_toolbar(saveaspng = FALSE)
           ))
    
    
  }) 
  
  # Render lanternplot graph -----
  output$lantern_plot <- renderPlotly({
    
    # Prevent app from crashing if nothing is selected: 
    req(input$season_lantern)
    
    lantern <- map_data |>
      filter(grepl("lampfish|lanternfish", common_name, ignore.case = TRUE)) |>
      select(map_label, depth, abundance, Season, sample_id, Latitude, Longitude, upload_time, sampling.date, Family, Genus) |>
      mutate(
        station_label = str_extract(sample_id, "OSWS|C1|M1|M2|MARS")
      ) |>
      mutate( # Do this to combine all chlorophyll max obs
        depth = as.numeric(as.character(depth)))
    
    # Get min, max value and median Chla
    min_depth <-  min(lantern$depth[lantern$depth > 0], na.rm = TRUE)
    max_depth_below_150 <- max(lantern$depth[lantern$depth < 150], na.rm = TRUE)
    chl_max_median <- median(c(min_depth, max_depth_below_150))
    
    
    lantern <- lantern |>
      mutate(
        depth_plot = if_else(
          depth > 0 & depth <= max_depth_below_150, chl_max_median, depth) )
    
    lantern_summary <- lantern |>
      filter(Season %in% input$season_lantern) |> 
      group_by(station_label, depth_plot) |>  # add map label back if want individual sp
      summarize(detections = n(), .groups = "drop") |>
      mutate(
        depth_label = if_else(
          depth_plot == chl_max_median, "Chlorophyll Max", as.character(depth_plot)
        )
      )
    
    
    # Bathymetry ----------------- MOVE TO GLOBAL ----------------------
    
    # # Elkhorn slough
    # start_lon <- map_data |> 
    #   filter(Station == "Elkhorn Slough") |> 
    #   select(Station, Latitude, Longitude) |> 
    #   summarize(Latitude = mean(Latitude),
    #             Longitude = mean(Longitude)) |> 
    #   pull(unique(Longitude))
    # 
    # # Elkhorn slough
    # start_lat <- map_data |> 
    #   filter(Station == "Elkhorn Slough") |> 
    #   select(Station, Latitude, Longitude) |> 
    #   summarize(Latitude = mean(Latitude),
    #             Longitude = mean(Longitude)) |> 
    #   pull(unique(Latitude))
    # 
    # # OSWS
    # end_lon <- lantern |>
    #   filter(station_label == "OSWS") |>
    #   pull(Longitude) |>
    #   first()
    # 
    # # OSWS
    # end_lat <- lantern |>
    #   filter(station_label == "OSWS") |>
    #   distinct(Latitude) |>
    #   pull(Latitude)
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
    # End move to global -----------------------------------------------
    
    
    # Get station coords: average because some have 2
    stations <- lantern |> 
      group_by(station_label) |> 
      summarize(
        Latitude = mean(Latitude, na.rm = TRUE),
        Longitude = mean(Longitude, na.rm = TRUE)
      ) |> 
      add_row(station_label = "ES", Latitude = start_lat, Longitude = start_lon) |> 
      arrange(Longitude)  # Order from west to east
    
    
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
                                 data.frame(station = stations$station_label[i],
                                            distance = max(temp_transect$dist)))
    }
    
    # # ADD THIS: Create the main transect from Elkhorn Slough to OSWS
    # transect_data <- get.transect(monterey_map,
    #                               x1 = start_lon,
    #                               y1 = start_lat,
    #                               x2 = end_lon,
    #                               y2 = end_lat,
    #                               distance = TRUE)
    
    lantern_plot <- lantern_summary |> 
      select(station_label, detections, depth_plot, depth_label) |> 
      left_join(station_distances, by = c("station_label" = "station"))
    
    
    # Is it possible to make this a plotly? 
    # transect_df <- as.data.frame(transect_data)
    
    # Clean the data before plotting
    lantern_plot <- lantern_plot |> 
      mutate(depth = as.numeric(as.character(depth_plot)))
    
    # transect_df <- transect_df |> 
    #   mutate(depth = as.numeric(as.character(depth)))
    
    # Create annotations: 
    annotations_list <- lapply(1:nrow(station_distances), function(i) {
      
      label <- station_distances$station[i]
      if(label == "Elkhorn Slough") {
        label <- "Elkhorn<br>Slough"
      }
      
      list(
        x = station_distances$distance[i],
        y = 1,  # Position above water surface (adjust as needed)
        yref = "paper",
        text = paste0("<b>", station_distances$station[i], "</b>"),
        showarrow = FALSE,
        font = list(size = 12, color = "#1A1A40"),
        xanchor = "center",
        yanchor = "bottom"
      )
    })
    
    
    # Create plotly 
    plot_ly() |> 
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
      ) |> 
      
      # Lanternfish markers - NEGATE the depth values
      add_markers(
        x = as.numeric(lantern_plot$distance),
        y = -as.numeric(lantern_plot$depth_plot),  
        marker = list(
          color = as.numeric(lantern_plot$detections),
          colorscale = list(c(0, "#FFF7BC"), c(0.5, "#FF6347"), c(1, "#8B0000")),
          showscale = TRUE,
          size = 10, 
          opacity = 0.8,
          line = list(color = 'black', width = 1), 
          colorbar = list(title = "Detections")
          
        ),
        hovertemplate = paste0("Depth: ", lantern_plot$depth_label, "<br>",
                               "Detections: ", lantern_plot$detections,
                               "<extra></extra>")
        
      ) |> 
      layout(
        font = list(family = "Arial, sans-serif", size = 12),
        title = list(
          text = "Detections of myctophidae (lampfish and lanternfish)",
          x = 0.1
        ),
        
        xaxis = list(title = "Distance from start of transect (km)",
                     fixedrange = FALSE),
        yaxis = list(
          title = "Depth (m)",
          range = c(-3000, 0),
          fixedrange = FALSE
        ),
        margin = list(t = 60), # top margin
        annotations = annotations_list,
        dragmode = 'zoom',
        hovermode = 'closest'
      ) |> 
      # Subtle horizontal lines at depth intervals
      config(scrollZoom = TRUE,
             displayModeBar = TRUE)
    
    
  }) 
  
  
} # End server fn parenthesis 







