barchart <- function(data_fn, species_fn, season_fn, clicked_lat, clicked_lon){
  
  
  if(species_fn == "All taxa") { 
    
    # Filter data:
    # bar_plot_data <- data_fn |> 
    #   filter(Season %in% season_fn,
    #          near(Latitude, clicked_lat),
    #          near(Longitude, clicked_lon)) |>
    #   group_by(map_label) |> 
    #   summarize(
    #     sp_count = n(), # of sp detections at that lat/long
    #     .groups = "drop") |> 
    #   arrange(sp_count) |> # reorder 
    #   mutate(percent = (sp_count / sum(sp_count)) * 100) |> 
    #   mutate(
    #     percent_bin = cut(percent,
    #                       breaks = c(0, 1, 2, 5, 10, 20, 50, 100),
    #                       labels = c("<1%", "1-2%", "2-5%", "5-10%", "10-20%", "20-50%", ">50%"))
    #   )
    
    bar_plot_data <- map_data |> 
      # Filter season and selected lat/lon
      filter(Season %in% season_fn,
             near(Latitude, clicked_lat),
             near(Longitude, clicked_lon)) |>
      # Get total number of samples at that point
      mutate(
        total_samples = n_distinct(sample_id) 
      ) |> 
      # Get total number of samples with each species 
      distinct(map_label, sample_id, total_samples) |> # one row per sample per species
      group_by(map_label) |> 
      summarize(
        sp_count = n(), # of sp detections at that lat/long
        total_samples = first(total_samples),
        .groups = "drop") |> 
      arrange(sp_count) |> # reorder 
      mutate(percent = (sp_count / total_samples) * 100) |> 
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
        )
        
      )
    
  } # End if
  
  else {
    
    
    bar_plot_data_sp <- data_fn |> 
      filter(Season %in% season_fn, 
             near(Latitude, clicked_lat),
             near(Longitude, clicked_lon)) |>
      mutate(
        total_samples = n_distinct(sample_id) 
      ) |>
      distinct(map_label, sample_id, total_samples) |> 
      group_by(map_label) |> 
      summarize(
        sp_count = n(), # of sp detections at that lat/long
        total_samples = first(total_samples),
        .groups = "drop") |> 
      arrange(sp_count) |> # reorder 
      mutate(percent = (sp_count / total_samples) * 100) |> 
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
    bar_palette_sp <- ifelse(unique_labels %in% species_fn, "#3da48c", "lightgrey")
    names(bar_palette_sp) <- unique_labels
    
    
    # Render plot:
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
  } # End else
  
} # End function

