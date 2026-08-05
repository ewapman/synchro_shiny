total_detections <- function(full_data) {
  
  # Count number of unique species for summary ----
  # Full dataset:
  total_detections <- full_data |> 
    mutate(lowest_taxa = case_when(
      !is.na(preferred_species_for_reporting) ~ preferred_species_for_reporting,
      !is.na(Genus) ~ Genus,
      !is.na(Family) ~ Family,
      !is.na(Order) ~ Order,
      TRUE ~ "Unknown"
    )) 
  
  n_unique_total <- n_distinct(total_detections$lowest_taxa)
  
  return(n_unique_total)
  
}

# Indicators
n_unique_indicators <- function(indicator_data) {
  
  indicators <- n_distinct(indicator_data$map_label)
  
  return(indicators)
  
}




