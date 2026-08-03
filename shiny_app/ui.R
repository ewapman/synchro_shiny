# user interface ----
ui <- navbarPage(
  title = "Synchro eDNA",
  
  # (Page 1) intro tabPanel ----
  tabPanel(title = "About", 
           fluidRow(
             column(10, offset = 1,
                    includeHTML("about.html")
             )
           )
           
  ),
  # END (Page 1) intro tabPanel
  
  # (Page 2) data viz tabPanel ----
  tabPanel(
    title = "Explore the Data",
    
    # tabsetPanel to contain tabs for data viz ----
    tabsetPanel(
      # tabPanel for indicator species ----
      tabPanel(
        title = "Monterey Bay Indicator Species",
        
        # trout sidebarLayout ----
        sidebarLayout(
          # trout sidebarPanel ----
          sidebarPanel(
            width = 3,
            # Drop down input: select species ----
            selectInput(
              inputId = "species_input",
              label = "Indicator species:",
              choices = NULL ),
            # change to NULL (Warning -- server better at handling large lists)
            # Dropdown to select date in sample date range ----
            pickerInput(
              inputId = "season_input",
              label = "Season:",
              choices = NULL,
              multiple = TRUE,
              options = pickerOptions(actionsBox = TRUE)
            ), 
            pickerInput(
              inputId = "depth_map_input",
              label = "Environmental data depth:",
              choices = NULL
            ),
            
            br(),
            
            # Info box
            div(
              style = "background-color: #e7f3ff; padding: 10px; border-left: 4px solid #2196F3; border-radius: 4px;",
              strong(paste0("Of ", n_unique_total, " unique taxonomic detections, ",
                            n_unique_indicators, " are indicator species.")),
              p("Select a species and season to explore distribution patterns.", 
                style = "margin-top: 5px; font-size: 12px;")
            ),
            
            
          ),# END sidebarPanel
          
          
          mainPanel(
            width = 9,
            fluidRow(
              column(8, 
                     leafletOutput("indicator_sp_map", height = "700px")
              ),
              column(4,
                     # conditionalPanel(
                     #   condition = "input.indicator_sp_map_marker_click && input.species_input == 'All taxa'",
                     wellPanel(
                       h5(strong("Species Composition")),
                       plotlyOutput("species_bar_plot", height = "650px")
                     )
                     # )
              )
              
            )
          ) # End mainPanel
          
          
        ), # END  sidebarLayout - map
        
        hr(), # Break
        
        # Begin depth panel ----
        # depth sidebarPanel 
        sidebarLayout(
          
          sidebarPanel(
            # Drop down input: select species ----
            selectInput(
              inputId = "season_depth_input",
              label = "Select a season:",
              choices = NULL ),
            # change to NULL (Warning -- server better at handling large lists)
            
          ),# END sidebarPanel - depth plot
          
          
          # depth mainPanel ----
          mainPanel(
            girafeOutput(outputId = "depth_plot", height = "800px")
          ) # End mainPanel - Depth plot
          
        ), # End sidebarLayout - Depth plot
        
        hr(), # Break
        
        # Begin species relative abundance/season panel 
        
        sidebarLayout(
          
          sidebarPanel(
            # Drop down input: select species ----
            selectInput(
              inputId = "species_season_line_input",
              label = "Select a species:",
              choices = NULL ),
            # change to NULL (Warning -- server better at handling large lists)
            
          ),# END sidebarPanel - depth plot
          
          # depth mainPanel ----
          mainPanel(
            girafeOutput(outputId = "species_line_plot")
          ) # End mainPanel - Depth plot
          
        ), # End sidebarLayout - species line graph 
        
        hr(), # Break
        
        # Begin Lantern panel
        
        sidebarLayout(
          
          sidebarPanel(
            # Drop down input: select species ----
            pickerInput(
              inputId = "season_lantern",
              label = "Select a season:",
              choices = unique(map_data$Season),
              selected = unique(map_data$Season),
              multiple = TRUE),
            
            # change to NULL (Warning -- server better at handling large lists)
            
          ),# END sidebarPanel - depth plot
          
          # depth mainPanel ----
          mainPanel(
            plotlyOutput(outputId = "lantern_plot")
          ) # End mainPanel - Lantern plot
          
        ) # End sidebarLayout - species line graph 
        
        
      ) # END  tabPanel - Indicator sp
      
      
    ) # END tabsetPanel - Indicator species selection tab
    
  ) # END tabPanel - Data Viz
  
) # END navbar page

