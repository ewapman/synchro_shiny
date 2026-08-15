# line_graph <- function(data_fn, species_fn) {
#   
#   # Define season order first
#   season_order <- c("Spring 2025", "Summer 2025", "Autumn 2025")
#   
#   # Data wrangling
#   sp_plot_data <- data_fn |>
#     select(map_label, Season, sample_id) |> 
#     filter(map_label %in% species_fn) |>
#     distinct(sample_id, Season, map_label) |> 
#     group_by(Season) |> 
#     summarize(
#       sp_samples = n(),
#       .groups = "drop"
#     ) |> 
#     left_join(
#       data_fn |> 
#         distinct(sample_id, Season) |> 
#         group_by(Season) |> 
#         summarize(total_samples = n(),
#                   .groups = "drop"),
#       by = "Season"
#     ) |> 
#     mutate(relative_abundance = ((sp_samples / total_samples) * 100)) |> 
#     complete(Season = season_order, fill = list(sp_samples = 0, total_samples = 0, relative_abundance = 0)) |>
#     mutate(Season = factor(Season, levels = season_order))
#   
#   # Add upwelling data
#   upwelling_season <- tibble(
#     month = 4:10,
#     month_name = c("April", "May", "June", "July", "August", "September", "October"),
#     upwelling_index = c(15.568, 24.595, 22.156, 9.498, 9.498, 2.004, 2.347),
#     Season = case_when(
#       month %in% c(4, 5) ~ "Spring 2025",
#       month %in% c(6, 7, 8) ~ "Summer 2025",
#       month %in% c(9, 10) ~ "Autumn 2025"
#     )
#   ) |>
#     mutate(Season = factor(Season, levels = season_order))
#   
#   # Join with your plot data
#   sp_plot_data <- sp_plot_data |>
#     left_join(upwelling_season, by = "Season")
#   
#   # Calculate scaling factor AFTER joining data
#   scale_factor <- max(sp_plot_data$relative_abundance, na.rm = TRUE) / 
#     max(sp_plot_data$upwelling_index, na.rm = TRUE)
#   
#   # Graph
#   sp_plot <- ggplot(sp_plot_data, aes(x = Season, group = 1)) +
#     
#     # Upwelling line (scaled)
#     geom_line(aes(y = upwelling_index * scale_factor), 
#               color = "steelblue", linetype = "dashed", size = 1) +
#     geom_point(aes(y = upwelling_index * scale_factor),
#                color = "steelblue", size = 2) +
#     
#     # Species line - FIXED: relative_abundance needs aes()
#     geom_line(aes(y = relative_abundance), color = "darkgrey") +
#     geom_point_interactive(
#       aes(y = relative_abundance,  # FIXED: added aes()
#           tooltip = paste0(
#             species_fn, "\n",
#             "% of samples: ", round(relative_abundance, 2), "%\n",
#             "Upwelling: ", round(upwelling_index, 1)
#           )),
#       color = "black", fill = "#69b3a2", size = 3, shape = 21
#     ) + 
#     
#     # Two y-axes
#     scale_y_continuous(
#       name = "Percent of samples (%)",
#       limits = c(0, NA),
#       sec.axis = sec_axis(
#         ~ . / scale_factor, 
#         name = "Upwelling Index"
#       )
#     ) +
#     
#     theme_minimal() +
#     labs(
#       x = "Season",
#       title = paste0("Percentage of ", species_fn, " per season")
#     ) +
#     theme(
#       axis.title.x = element_text(margin = margin(t = 20), size = 16),
#       axis.title.y = element_text(margin = margin(r = 20), size = 16),
#       axis.title.y.right = element_text(margin = margin(l = 20), 
#                                         size = 16, color = "steelblue"),
#       axis.text = element_text(size = 14),
#       axis.text.y.right = element_text(color = "steelblue")
#     )
#   
#   girafe(ggobj = sp_plot,
#          width_svg = 8,
#          height_svg = 5,
#          options = list(
#            opts_hover(css = "fill:orange;stroke:black;"),
#            opts_toolbar(saveaspng = FALSE)
#          ))
# }


# 
line_graph <- function(data_fn, species_fn) {


  # Plots for % of each species: relative abundance for each season (# sp out of total detections that season)----

  # Define season order first
  season_order <- c("Spring 2025", "Summer 2025", "Autumn 2025")
  
  total_season <- data_fn |>
    distinct(sample_id, Season) |>
    group_by(Season) |>
    summarize(total_season = n(),
              .groups = "drop")


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
    complete(Season = season_order, fill = list(sp_samples = 0)) |>
    left_join(total_season, by = "Season") |> 

    # # Get total samples each season
    # left_join(
    #   data_fn |>
    #     distinct(sample_id, Season) |>
    #     group_by(Season) |>
    #     summarize(total_samples = n(),
    #               .groups = "drop"),
    #   by = "Season"
    # 
    # ) |>
    mutate(relative_abundance = ((sp_samples / total_season) * 100)) |>
    
    mutate(Season = factor(Season, levels = season_order)) 
    



  # Graph

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
