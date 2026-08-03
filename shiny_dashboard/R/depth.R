# Make it so that it will highlight the species selected 
# If no season is selected have it say select a season

depth_plot <- function(data_fn, season_fn, species_fn) {
  
  
  # Filter data - relative abundance at each depth
  depth_data <- map_data |> 
    select(map_label, depth, abundance, Season) |> 
    filter(Season %in% season_fn) |> 
    mutate( # Do this to combine all chlorophyll max obs
      depth = as.numeric(as.character(depth)))

  
  min_depth <-  min(depth_data$depth[depth_data$depth > 0], na.rm = TRUE)
  max_depth_below_150 <- max(depth_data$depth[depth_data$depth < 150], na.rm = TRUE)
  chl_max_median <- median(c(min_depth, max_depth_below_150))
  
  
  depth_data <- depth_data |> 
    mutate(
      depth_plot = if_else(
        depth > 0 & depth <= max_depth_below_150, chl_max_median, depth) )|> 
    group_by(map_label, depth_plot) |> 
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
      map_label_abb = if_else(
        str_detect(map_label, "^[A-Z][a-z]+ [a-z]+ \\("),  # Two words before parentheses
        str_replace(map_label, "^([A-Z])[a-z]+ ([a-z]+)", "\\1. \\2"),  # Abbreviate genus
        map_label  # Keep as-is (only one word before parentheses)
      )) 
  
  # Calculate average depth per species (weighted by abundance)
  species_order <- depth_data |>
    group_by(map_label_abb) |>
    summarize(avg_depth = weighted.mean(depth_plot, sp_depth), .groups = "drop") |>
    arrange(avg_depth) |>  # Surface species first
    pull(map_label_abb)
  
  # Convert to factor with this order
  depth_data <- depth_data |>
    mutate(map_label_abb = factor(map_label_abb, levels = species_order))
  
  
  depth_data$depth_plot <- as.numeric(depth_data$depth_plot)
  
  # Create list of selected seasons
  season_list <- paste(season_fn, collapse = ", ")
  
  # Create plot 

  p <- ggplot(depth_data, aes(x = map_label_abb, y = depth_plot, size = relative_abundance)) +
    
    annotate("rect",
             xmin = 0.5,  # Just past y-axis
             xmax = Inf,  # To the right edge
             ymin = min_depth,   # Top of Chl Max zone 
             ymax = max_depth_below_150,   # Bottom of Chl Max zone 
             fill = "lightgreen", 
             alpha = 0.2) +
    
    
    geom_point_interactive(shape = 21,
                           alpha = 0.7, 
                           color = "black",
                           stroke = 1,
                             aes(
                               fill = if_else(
                                 map_label == species_fn & species_fn != "All taxa",
                                 "darkorange", "darkblue"),
                             tooltip = paste0(
                               map_label, "\n",
                               "Depth: ", depth_label, "m\n",
                               "Relative Abundance: ",
                               if_else(relative_abundance < 0.01,
                                       paste0(format(relative_abundance, scientific = TRUE, digits = 2), "%"),
                                       sprintf("%.2f%%", relative_abundance)))))+
    scale_fill_identity() +
    scale_size(range = c(1, 12), name="Relative Abundance") +
    labs(x = "Indicator Taxa", 
         y = "Depth (m)",
         title = paste0("Seasons selected: ", season_list),
         subtitle = paste0("Green zone = Deep Chlorophyll Max (", min_depth, "-", max_depth_below_150, ")")) +
    theme_bw() +
    scale_y_reverse(
      breaks = c(0, max_depth_below_150, 150, 300),
      limits = c(300, 0)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
          legend.position = "none") +
    theme(axis.title.y = element_text(
      margin = margin(r = 20, l = 60),
      vjust = 2),
      plot.title = element_text(size = 18),
      plot.subtitle = element_text(color = "darkgreen", size = 16),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 14),
      axis.title.x = element_text(
        margin = margin(t = 20)
      )
    ) +
    theme(
      panel.grid.major.y = element_line(color = "gray90"),  # Keep major gridlines
      panel.grid.minor.y = element_blank(),  # Remove minor gridlines
      panel.grid.major.x = element_line(linewidth = 0.4)
      
    )
  
  
  
  girafe(ggobj = p,
         width_svg = 18,
         height_svg = 11,
         options = list(
           opts_hover(css = "fill:orange;stroke:black;"),
           opts_toolbar(saveaspng = FALSE)
         ))
  
}

