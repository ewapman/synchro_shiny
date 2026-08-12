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
            tags$div(
              style = "text-align: center;",
            tags$img(class = "banner", src = "media/cover.jpg", height = "500px",
                     alt = "A photo of sea water bags being filtered for eDNA on a boat.")),
            headerPanel(""),
            # START fluidRow with DNA intro box ----
            fluidRow(
              
              # START eDNA about box ----
              box(width = 12,
                  title = tagList(icon("dna"), strong("DNA Background")),
                  includeMarkdown("text/dna_about.Rmd")
              ) # END intro box
            ), # End fluid row
            
            # START fluidRow with Synchro about box 1----
            fluidRow(
              
              # START eDNA about box ----
              box(width = 12,
                  title = tagList(icon("ship"), strong("Synchro Background")),
                  includeMarkdown("text/synchro_about.Rmd")
              ) # END intro box
              
            ), # End fluid row
            
            headerPanel(""),
            
            # Add images 
            fluidRow(
              column(6, align = "center",
                     tags$img(src = "media/station.jpg", width = "80%"),
                     tags$br(),
                     tags$em("An offshore sampling station")
              ),
              column(6, align = "center",
                     tags$img(src = "media/elkhorn.jpg", width = "80%"),
                     tags$br(),
                     tags$em("Processing samples at Elkhorn Slough")
              )
              
            ),
            
            # Start fluidRow with Synchro about box 2 ----
            fluidRow(
              headerPanel(""),
              headerPanel(""),
              box(width = 5, 
                  includeMarkdown("text/synchro_about_2.Rmd")),
              column(7, align = "center",
                     tags$img(src = "media/ctd.jpg", width = "100%",
                              style = "margin-top: 40px;"),
                     tags$br(),
                     tags$em("A CTD cast")
                     
                     )
            ), # End fluid row
            
            headerPanel(""),
            
          
            # Add images
            fluidRow(
              column(6, align = "center", 
                     tags$img(src = "media/filter_bags.jpg", width = "70%"),
                     tags$br(),
                     tags$em("Active filtration on board")
              ),
              column(6, align = "center", 
                     tags$img(src = "media/torpe.jpg", width = "70%"),
                     tags$br(),
                     tags$em("Passive filtration using a TorpeDNA device")
              )
              
            ),
            
            headerPanel(""),
            headerPanel(""),
            
            # Schematic
            fluidRow(
              tags$div(
                style = "text-align: center;",
              tags$img(class = "banner", src = "media/methods.png",
                       #alt = "A photo of sea water bags being filtered for eDNA on a boat.",
                       style = "max-width: 90%; height: auto; display: block; margin: 0 auto;"),
              tags$figcaption(
                style = "font-style: italic; color: #555555; margin-top: 8px; font-size: 14px; margin-left: 30px;",
                "Diagram of sampling methods from the offshore wind station (left) to Elkhorn Slough (right)."
              )
              )
            ),
            
            headerPanel(""),
            
            fluidRow(
              tags$div(
                style = "text-align: center;",
                tags$img(class = "banner", src = "media/study_map_2.jpeg",
                       #alt = "A photo of sea water bags being filtered for eDNA on a boat.",
                       style = "max-width: 70%; height: auto; display: block; margin: 0 auto;"),
                tags$figcaption(
                  style = "font-style: italic; color: #555555; margin-top: 8px; font-size: 14px;",
                  "Map of study transect."
              ))
              
            )
            
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
                                 # fluidRow(
                                 #   
                                 #   column(
                                 #     width = 4,
                                 #     offset = 2,
                                 #     valueBoxOutput("total_count", width = NULL)
                                 #   ),
                                 #   
                                 #   column(
                                 #     width = 4,
                                 #     valueBoxOutput("indicator_count", width = NULL)
                                 #   ),
                                 #   
                                 # ),
                                 # 
                                 #  headerPanel(""),
                                 
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
                                       inputId = "depth_input",
                                       label = "Depth:",
                                       multiple = TRUE,
                                       choices = NULL
                                     ))
                                   ), # End fluid row inputs
                                     
                                

                                   
                                   # Map output ----
                                 fluidRow(
                                   column(8,
                                         # withSpinner(
                                          plotlyOutput("indicator_sp_map", height = "600px"),
                                          type = 4, 
                                          color = "#3da48c"
                                          #)
                                   ),
                                   column(4,
                                          wellPanel(
                                            h4(strong("Taxonomic composition")),
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
                                        
                                        girafeOutput(outputId = "species_line_plot", height = "400px")
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
                                       h4(strong("Percentage of samples containing each taxon by depth")),
                                       
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

