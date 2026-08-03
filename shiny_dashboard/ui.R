# user interface ----

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Dashboard Header                            ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
header <- dashboardHeader(
  
  # add title ----
  title = "Synchro eDNA Data",
  titleWidth = 400
  
) # END dashboardHeader

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                              Dashboard Sidebar                           ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
sidebar <- dashboardSidebar(
  
  # sidebarMenu ----
  sidebarMenu(
    id = "tabs",
    menuItem(text = "Welcome", tabName = "welcome", icon = icon("star")),
    menuItem(text = "Dashboard", tabName = "dashboard", icon = icon("chart-line"))
    # menuItem(text = "Depth", tabName = "depth", icon = icon("ship")),
    # menuItem(text = "Seasonal change", tabName = "season", icon = icon("leaf")),
    # menuItem(text = "Depth case study", tabName = "depth_lantern", icon = icon("fish"))
    
  ) # END sidebarMenu
  
) # END dashboardSidebar

##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                               Dashboard Body                             ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
body <- dashboardBody(
  
  # Load styling and font here ----
  
  # START tabItems ----
  tabItems(
    
    # ......................START welcome tabItem ..............................
    tabItem(tabName = "welcome",
            tags$img(class = "banner", src = "media/synchro_boat.png",
                     alt = "A landscape photo of a golden field of grass that stretches towards rolling dark green/brown hills. The sun is rising over the hilltops to the left and the sky is clear. A narrow trail weaves down the center. In the foreground, there are a few bent metal posts with barbed wire streteched between them. In front of the fence, there is a crooked metal sign reading 'Camuesa Connector Trail'."),
            
            # START fluidRow with DNA intro box ----
            fluidRow(
              
              # START eDNA about box ----
              box(width = 12,
                  title = tagList(icon("dna"), strong("DNA Background")),
                  includeMarkdown("text/dna_about.Rmd")
              ) # END intro box
            ), # End fluid row
            
            # START fluidRow with Synchro about box ----
            fluidRow(
              
              # START eDNA about box ----
              box(width = 12,
                  title = tagList(icon("ship"), strong("Synchro Background")),
                  includeMarkdown("text/synchro_about.Rmd")
              ) # END intro box
              
            ) # End fluid row
            
    ), # END welcome tabItem
    
    # ......................Start map tabItem ..................................
    tabItem(tabName = "dashboard",
            
            # Start map tabsetPanel ----
            tabsetPanel(id = "dashboard_tabsetPanel",
                        
                        # Start map tabPanel ----
                        tabPanel(title = strong("Indicator Taxa Dashboard"),
                                 
                                 # Start about fluid row ----
                                 fluidRow(
                                   box(width = 12,
                                       includeMarkdown("text/map_about.Rmd")
                                   )
                                   
                                 ),
                                 
                                 # Add space
                                 headerPanel(""),
                                 headerPanel(""),
                                 
                                 # Start summary fluid row ----
                                 fluidRow(
                                   
                                   column(
                                     width = 4,
                                     offset = 2,
                                     valueBoxOutput("total_count", width = NULL)
                                   ),
                                   
                                   column(
                                     width = 4,
                                     valueBoxOutput("indicator_count", width = NULL)
                                   ),
                                   
                                 ),
                                 
                                  headerPanel(""),
                                 
                                 # Start fluidRow inputs ----
                                 fluidRow(
                                   
                                   # Start picker column ----
                                   column( 
                                     width = 4,
                                     # Drop down input: select species
                                     selectInput(
                                       inputId = "species_input",
                                       label = "Indicator taxa:",
                                       choices = NULL )),
                                     
                                     # Dropdown to select date 
                                   column(
                                     width = 4,
                                     pickerInput(
                                       inputId = "season_input",
                                       label = "Season:",
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = pickerOptions(actionsBox = TRUE)
                                     )), 
                                     
                                     # Select depth range
                                    column(
                                      width = 4,
                                   pickerInput(
                                       inputId = "depth_map_input",
                                       label = "Environmental data depth:",
                                       choices = NULL
                                     ))
                                   ), # End fluid row inputs
                                     
                                

                                   
                                   # Map output ----
                                 fluidRow(
                                   column(8, 
                                          leafletOutput("indicator_sp_map", height = "600px")
                                   ),
                                   column(4,
                                          wellPanel(
                                            h4(strong("Taxonomic Composition")),
                                            plotlyOutput("species_bar_plot", height = "500px")
                                          )
                                   )
                                   
                                 ), # End fluidRow
                                 
                                 headerPanel(""),
                                 headerPanel(""),
                                 
                                 # START - fluid row line graph
                                 fluidRow(
                                   box(width = 4,
                                       includeMarkdown("text/line_about.Rmd")
                                   ),
                                   
                                   box( width = 8,
                                        
                                        girafeOutput(outputId = "species_line_plot", height = "500px")
                                   )
                                   
                                 ), # End fluidRow line graph
                                 
                                 headerPanel(""),
                                 headerPanel(""),
                                 # 
                                 # # START - fluid row depth about
                                 fluidRow(
                                   box(width = 12,
                                       includeMarkdown("text/depth_about.Rmd")
                                   )
                                   ), # End fluid row depth about
                                 # 
                                 headerPanel(""),
                                 
                                 # START - depth graph (all taxa) fluid row
                                 fluidRow(
                                   
                                
                                   box(width = 12,
                                       height = "700px",
                                       h4(strong("Relative abundance of taxa at each depth")),
                                       
                                       headerPanel(""),
                                       headerPanel(""),
                                       girafeOutput(outputId = "depth_plot", height = "600px")
                                   )
                                   
                                   
                                 ), # End - depth graph (all taxa) fluid row
                                 
                                 headerPanel(""),
                                 headerPanel(""),
                                 
                                 # Start fluid row bathymetry plot
                                 fluidRow(
                                   box(width = 12,
                                       includeMarkdown("text/bathymetry_about.Rmd"))
                                   ), # End fluid row bathymetry_about.rmd
                                 fluidRow(
                                   box(width = 12,
                                       h4(strong("Number of detections at each depth and station")),
                                       plotlyOutput(outputId = "bathymetry_plot", height = "500px"))
                                   
                                 ) # End fluid row bathymetry plot
                                 
                                 
                        ) # End map tabPanel
            ) # End map tabsetPanel
            
            
            
    ) # END map tabItem
    
    
  ) # END tabItems
  
) # END dashboardBody


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                         Combine into dashboardPage                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dashboardPage(header, sidebar, body)

