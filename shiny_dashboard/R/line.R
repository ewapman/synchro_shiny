line_graph <- function(data_fn, species_fn) {


  # Plots for % of each species: relative abundance for each season (# sp detections out of total detections that season)----

  # Define season order first
  season_order <- c("Spring 2025", "Summer 2025", "Autumn 2025")
  
  # Total samples that season
  total_season <- data_fn |>
    distinct(sample_id, Season) |> # Only unique samples 
    group_by(Season) |>
    summarize(total_season = n(),
              .groups = "drop")


  # Get total nmber for the selectd organism 
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
    complete(Season = season_order, fill = list(sp_samples = 0)) |> # fill in seasons with no data with 0 so they still plot
    left_join(total_season, by = "Season") |> 
    mutate(relative_abundance = ((sp_samples / total_season) * 100)) |>
    mutate(Season = factor(Season, levels = season_order)) 
    



  # Plot

  sp_plot <- ggplot(sp_plot_data, aes(x = Season, y = relative_abundance, group = 1)) +
    geom_line(color = "darkgrey") +
    geom_point_interactive(color = "black", fill = "#69b3a2", size = 3, shape = 21,
                           aes(tooltip = paste0(
                             species_fn, "\n",
                             "Samples: ", sp_samples, " of ", total_season, 
                             " (", round(relative_abundance, 2),"%", " )"))) +
                             #"% of samples: ", round(relative_abundance, 2), "%")))+
    scale_y_continuous(limits = c(0, NA)) +
    theme_minimal() +
    labs(x = "Season",
         y = "Percent of samples (%)",
         title = paste0("Percentage of ", species_fn, " per season")) +
    theme(
      axis.title.x = element_text(margin = margin(t = 20), size = 16),
      axis.title.y = element_text(margin = margin(r = 20), size = 16),
      axis.text = element_text(size = 14),
    )


  girafe(ggobj = sp_plot,
         width_svg = 8,
         height_svg = 5,
         options = list(
           opts_hover(css = "fill:orange;stroke:black;"),
           opts_toolbar(saveaspng = FALSE)
         ))


}
