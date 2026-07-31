# server instructions ----
server <- function(input, output, session) {
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                      UI update: Grey out no data options
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Season grey out ------------------------------------------------------------
  update_season_picker <- function(species_selection) {
    
    # Get all possible seasons
    all_seasons <- sort(unique(map_data$Season))
    print(paste("All seasons:", paste(all_seasons, collapse = ", ")))
    
    # Filter data based on species selection
    if(species_selection == "All species") {
      filtered_data <- map_data # no filtering
    }
    else {
      filtered_data <- map_data |> # filter to species selected
        filter(map_label == species_selection)
    }
    print(paste("Filtered data rows:", nrow(filtered_data)))
    
    # Find dates that have observations in the filtered data
    seasons_with_data <- filtered_data |>
      pull(Season) |> # pull to extract single column and put into list (drop df structure)
      unique() |> # only unique seasons
      sort() # order seasons
    print(paste("Seasons with data:", paste(seasons_with_data, collapse = ", ")))
    
    # Determine which dates to disable
    seasons_to_disable <- !all_seasons %in% seasons_with_data # extract the dates in all dates that don't have data
    print(paste("Seasons to disable:", paste(all_seasons[seasons_to_disable], collapse = ", ")))
    
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
  
  #-------------------------------------------------------------------------------
  
  # Depth grey out -------------------------------------------------------------
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
  
  # ------------------------------------------------------------------------------
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                           Initial UI updates
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
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
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                           Reactive updates
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Update season when the species is changed
  observeEvent(input$species_input, {
    update_season_picker(input$species_input)
  })
  
  # Update depths when species or season changes
  observeEvent(c(input$species_input, input$season_input), {
    req(input$season_input)
    update_depth_picker(input$species_input, input$season_input)
  })
  
  # Store clicked location separately
  clicked_point <- reactiveVal(NULL)
  
  # When map marker is clicked, save it
  observeEvent(input$indicator_sp_map_marker_click, {
    clicked_point(input$indicator_sp_map_marker_click)
  })
  
  # When species changes, clear the clicked point
  observeEvent(input$species_input, {
    clicked_point(NULL)  # Reset click when species changes
  })
  
  # When season changes, clear the clicked point
  observeEvent(input$season_input, {
    clicked_point(NULL)  # Reset click when season changes
  })
  
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ##                              Render plots
  ##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # Render map plot --------------------------------------------------------------
  
  output$indicator_sp_map <- renderLeaflet({
    
    req(input$season_input, input$species_input, input$depth_map_input)
    
    leaflet_map(
      data_fn = map_data,
      season_fn = input$season_input,
      species_fn = input$species_input,
      depth_fn = input$depth_map_input
    )
  })
  
  # Render barchart plot ---------------------------------------------------------
  
  output$species_bar_plot <- renderPlotly({
    
    clicked <- clicked_point()  # Use reactiveVal instead of input
    
    if(is.null(clicked)) {
      return(
        plot_ly() %>%
          layout(
            annotations = list(
              text = "Click a point on the map",
              xref = "paper",
              yref = "paper",
              x = 0.5,
              y = 0.5,
              showarrow = FALSE,
              font = list(size = 16, color = "gray")
            ),
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      )
    }
    
    barchart(
      data_fn = map_data,
      species_fn = input$species_input,
      season_fn = input$season_input,
      clicked_lat = clicked$lat,
      clicked_lon = clicked$lng
    )
  })
  
  
  # Render value boxes ---------------------------------------------------------
  
  output$total_count <- renderValueBox({
    
    valueBox(
      value = total_detections(full_taxa),
      subtitle = "Unique Taxa Detected",
      icon = icon("fish"),
      color = "aqua"
    )
  })
  
  
  
  output$indicator_count <- renderValueBox({
    valueBox(
      value = n_unique_indicators(map_data),
      subtitle = "Unique indicator species",
      icon = icon("otter"),
      color = "teal"
    )
  })
  
  
  # Render line plot -------------------------------------------------------------
  
  output$species_line_plot <- renderGirafe({
    
    req(input$species_input)
    
    if(input$species_input == "All species"){
      return(
        girafe(
          ggobj = ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "Select a specific taxa",
                     size = 5, color = "gray50") +
            theme_void(),
          width_svg = 8,
          height_svg = 5
        )
      )
      
    }
    
    line_graph(
      data_fn = map_data,
      species_fn = input$species_input
      
    )
    
  })
  
  # Render depth plot ------------------------------------------------------------
  output$depth_plot <- renderGirafe({
    
    req(input$season_input)
    

    
    # If no season is selected, have empty plot and message to select a season
    
    if(is.null(input$season_input)) {
      return(
        girafe(
          ggobj = ggplot() +
            annotate("text", x = 0.5, y = 0.5,
                     label = "Select season(s)",
                     size = 6, color = "gray50") +
            theme_void(),
          width_svg = 8,
          height_svg = 11
        )
      )
    }
    
    depth_plot(map_data, input$season_input, input$species_input)
    
  })
  
  
  # Render bathymetry plot -------------------------------------------------------
  output$bathymetry_plot <- renderPlotly({

    req(input$season_input)

    # if(is.null(input$season_input)) {
    #   return(
    #     plot_ly() %>%
    #       layout(
    #         annotations = list(
    #           text = "Select a season(s)",
    #           xref = "paper",
    #           yref = "paper",
    #           x = 0.5,
    #           y = 0.5,
    #           showarrow = FALSE,
    #           font = list(size = 16, color = "gray")
    #         ),
    #         xaxis = list(visible = FALSE),
    #         yaxis = list(visible = FALSE)
    #       )
    #   )
    # }


    bath_graph(map_data, input$season_input, input$species_input)

  })
  
  
} # End server















