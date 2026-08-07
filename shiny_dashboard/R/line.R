
line_graph <- function(data_fn, species_fn) {
  
  
  # Plots for % of each species: relative abundance for each season (# sp out of total detections that season)----
  
  # Define season order first
  season_order <- c("Spring 2025", "Summer 2025", "Autumn 2025")
  
  
  # Data wrangling - calculate relative abundance: # of samples of selected organism out of total samples that season
  # sp_plot_data <- data_fn |>
  #   select(map_label, Season, sample_id) |> 
  #   filter(map_label %in% species_fn) |>   # Make interactive 
  #   distinct(sample_id) |> 
  #   mutate(
  #     total_samples = n(), .groups = "drop"
  #   ) 
  #   group_by(map_label, Season) |> 
  #   
  #   
  #   summarize(total_sp_season = sum(abundance), .groups = "drop") |> 
  #   complete(map_label, Season = season_order, fill = list(total_sp_season = 0)) |> 
  #   group_by(Season) |> 
  #   mutate(relative_abundance_percent = (total_sp_season / sum(total_sp_season)) * 100) |> 
  #   ungroup() |> 
  #   mutate(Season = factor(Season, levels = season_order)) |> 
  #   
  # 
  sp_plot_data <- data_fn |>
    select(map_label, Season, sample_id) |> 
    filter(map_label %in% species_fn) |>   # Make interactive 
    distinct(sample_id, Season, map_label) |> 
    # Number of samples w/ that species
    group_by(Season) |> 
    summarize(
      sp_samples = n(),
      .groups = "drop"
    ) |> 
    
    # Get total samples each season
    left_join(
      data_fn |> 
        distinct(sample_id, Season) |> 
        group_by(Season) |> 
        summarize(total_samples = n(),
                  .groups = "drop"),
      by = "Season"
      
    ) |> 
    mutate(relative_abundance = (sp_samples / total_samples * 100)) |> 
    complete(Season = season_order, fill = list(sp_samples = 0, total_samples = 0, relative_abundance = 0)) |>
    mutate(Season = factor(Season, levels = season_order))
  
  
  # Graph
  
  sp_plot <- ggplot(sp_plot_data, aes(x = Season, y = relative_abundance, group = 1)) +
    geom_line(color = "darkgrey") +
    geom_point_interactive(color = "black", fill = "#69b3a2", size = 3, shape = 21,
                           aes(tooltip = paste0(
                             species_fn, "\n",
                             "Relative Abundance: ", round(relative_abundance, 2), "%")))+ 
    theme_minimal() +
    labs(x = "Season", 
         y = "Relative abundance (%)",
         title = paste0("Relative abundance of ", species_fn, " per season")) +
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
  
  
}
