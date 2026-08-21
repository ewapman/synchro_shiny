---

editor_options: 
  markdown: 
    wrap: sentence
---

# Synchro Shiny Dashboard

This project contains the Shiny dashboard displaying data visualizations for the Synchro eDNA project. This version uses data from the April, July, and October 2025 sampling dates. The shiny_app folder is an older version while the shiny_dashboard is the final, refined dashboard deployed to the shiny.io website. The preprocessing qmd in the Shiny folder includes all cleaning and data preparation steps for the dashboard. Open the ui.R, global.R, or server.R file from the shiny_dashboard folder and click "Run App" or run shiny::runApp() in the console to view. All figure code is in the R folder, all images in the www/media folder, and all text descriptions are in the text folder. The data folder includes both raw and processed versions. The processed RDS files are exported from the preprocessing qmd and imported into the global.R file for these visualizations.

The dashboard can also be viewed here: <https://ewapman.shinyapps.io/synchro_shiny_dashboard/>
