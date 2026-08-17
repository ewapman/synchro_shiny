

depth_plot <- function(data_fn, season_fn, species_fn) {
  
  # Get fixed depth order across all seasons ----
  all_taxa_depth_order <- data_fn |> 
    mutate(depth = as.numeric(as.character(depth))) |>
    distinct(sample_id, map_label, depth) |>
    mutate( # abbreviate scientific name
      map_label_abb = if_else(
        str_detect(map_label, "^[A-Z][a-z]+ [a-z]+ \\("),
        str_replace(map_label, "^([A-Z])[a-z]+ ([a-z]+)", "\\1. \\2"),
        map_label
      )
    ) |>
    group_by(map_label, map_label_abb) |>
    summarize(avg_depth = mean(depth, na.rm = TRUE), .groups = "drop") |>
    arrange(avg_depth) |> # arrange depending on mean depth 
    pull(map_label_abb)
  
  
  # Filter depending on season ----
  depth_data <- data_fn |> 
    filter(Season %in% season_fn) |> 
    mutate(depth = as.numeric(as.character(depth))) |>
    distinct(sample_id, map_label, depth)  # Remove ASVID duplicates
  
  min_depth <- min(depth_data$depth[depth_data$depth > 0], na.rm = TRUE)
  max_depth_below_150 <- max(depth_data$depth[depth_data$depth < 150], na.rm = TRUE)
  chl_max_median <- median(c(min_depth, max_depth_below_150)) # find median for plotting (place in middle of rectangle annotation)
  
  
  # Get number of samples of each species at that depth 
  depth_data <- depth_data |> 
    mutate(
      depth_plot = if_else( # if the depth is a DCM depth, just note as the median for plotting
        depth > 0 & depth <= max_depth_below_150, chl_max_median, depth)
    ) |>
    group_by(map_label, depth_plot) |> 
    summarize(
      sp_depth = n_distinct(sample_id),  #  Count samples (# for each organism)
      .groups = "drop"
    ) |> 
    # Get total samples at each depth and join
    left_join(
      data_fn |>  
        filter(Season %in% season_fn) |> 
        mutate(depth = as.numeric(as.character(depth))) |>
        mutate(depth_plot = if_else(depth > 0 & depth <= max_depth_below_150, chl_max_median, depth)) |>
        distinct(sample_id, depth_plot) |>  # Remove ASVID duplicates (unique for each depth only)
        group_by(depth_plot) |> 
        summarize(total_samples_at_depth = n(), .groups = "drop"),  # Count all samples at each depth
      by = "depth_plot"
    ) |> 
    mutate(
      relative_abundance = (sp_depth / total_samples_at_depth) * 100  
    ) |> 
    mutate(
      depth_label = if_else(
        depth_plot == chl_max_median, "Chlorophyll Max", as.character(depth_plot)
      )
    ) |> 
    mutate(
      map_label_abb = if_else(
        str_detect(map_label, "^[A-Z][a-z]+ [a-z]+ \\("),
        str_replace(map_label, "^([A-Z])[a-z]+ ([a-z]+)", "\\1. \\2"),
        map_label
      )
    )
  
  
  # Convert to factor with the order shallow to deep
  depth_data <- depth_data |>
    mutate(map_label_abb = factor(map_label_abb, levels = all_taxa_depth_order))
  
  # Convert depth to numeric
  depth_data$depth_plot <- as.numeric(depth_data$depth_plot)
  
  # Abbreviate selected species name to match axis labels
  species_fn_abb <- if_else(
    str_detect(species_fn, "^[A-Z][a-z]+ [a-z]+ \\("),
    str_replace(species_fn, "^([A-Z])[a-z]+ ([a-z]+)", "\\1. \\2"),
    species_fn
  )

  # Which taxa have data -- color red = selected, black = detected, grey = not detected 

  detected_taxa <- unique(depth_data$map_label_abb)
  label_colors <- ifelse(
    all_taxa_depth_order == species_fn_abb & species_fn != "All taxa",
    "red",
    ifelse(all_taxa_depth_order %in% detected_taxa, "black", "gray70")
  )
  
  
  # Create list of selected seasons for subtitle
  season_list <- paste(season_fn, collapse = ", ")
  
  # Create plot 
  
  p <- ggplot(depth_data, aes(x = map_label_abb, y = depth_plot, size = relative_abundance)) +

    # DCM annotation
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
                               "red", "darkblue"),
                             tooltip = paste0(
                               map_label, "\n",
                               "Depth: ", depth_label, "m\n",
                               "Samples: ", sp_depth, " of ", total_samples_at_depth, 
                               " (", round(relative_abundance, 2),"%", " )")
                           )) +
                               
    scale_fill_identity() +
    scale_size(range = c(1, 12), name="Relative Abundance") +
    scale_x_discrete(limits = all_taxa_depth_order, drop = FALSE) +  # Show all taxa
    labs(x = "Indicator Taxa", 
         y = "Depth (m)",
         title = paste0("All stations included", "\n", "Seasons selected: ", season_list, "\n", 
                        "Taxa selected: ", species_fn),
         subtitle = paste0("Green zone = Deep Chlorophyll Max (", min_depth, "-", max_depth_below_150, ")")) +
    theme_bw(base_size = 16) +
    scale_y_reverse(
      breaks = c(0, max_depth_below_150, 150, 300),
      limits = c(300, 0)) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1, color = label_colors),
          legend.position = "none") +
    theme(axis.title.y = element_text(
      margin = margin(r = 20, l = 80),
      vjust = 2),
      plot.title = element_text(size = 22),
      plot.subtitle = element_text(color = "darkgreen", size = 19),
      axis.title = element_text(size = 19),
      axis.text = element_text(size = 17),
      axis.title.x = element_text(
        margin = margin(t = 20)
      )
    ) +
    theme(
      panel.grid.major.y = element_line(color = "gray90"),  # Keep major gridlines
      panel.grid.minor.y = element_blank(),  # Remove minor gridlines
      panel.grid.major.x = element_line(linewidth = 0.4),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      
    )
  
  
  
  girafe(ggobj = p,
         width_svg = 20,
         height_svg = 13,
         options = list(
           opts_hover(css = "fill:orange;stroke:black;"),
           opts_toolbar(saveaspng = FALSE)
         ))
  
}




  
 
  