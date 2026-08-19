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
  
  # Add color styling ----
  tags$head(
    tags$style(HTML('
      /* Header - deep ocean blue */
      .skin-blue .main-header .navbar,
      .skin-blue .main-header .logo,
      .skin-blue .main-header .logo:hover {
        background-color: #1e3a5f;  /* Navy from deep water */
      }
      
      /* Sidebar - slightly darker */
      .skin-blue .left-side, .skin-blue .main-sidebar, .skin-blue .wrapper {
        background-color: #162d47;  /* Darker navy */
      }
      
      /* Active menu item - lighter blue accent */
      .skin-blue .sidebar-menu > li.active > a {
        border-left-color: #4a90e2;  /* Sky blue accent */
      }
      
    '))),
  
  # START tabItems ----
  tabItems(
    
    # ......................START welcome tabItem ..............................
    
    # Banner photo ----
    tabItem(tabName = "welcome",
            tags$div(
              style = "text-align: center;",
            tags$img(class = "banner", src = "media/cover.jpg", 
                     height = "400px",  
                     width = "100%",  
                     style = "object-fit: cover;")
            ),
          
            # Extra space
             headerPanel(""),
            
            # START fluidRow with DNA and Synchro intro box ----
            fluidRow(
              
              # START eDNA about box ----
              box(width = 6, height = "750px",
                  title = tagList(icon("dna"), strong("DNA Background")),
                  includeMarkdown("text/dna_about.Rmd")
              ), # END intro box
              
              # START Synchro about box ----
              box(width = 6, 
                  title = tagList(icon("ship"), strong("Synchro Background")),
                  includeMarkdown("text/synchro_about.Rmd"),
                  tags$div(align = "center",
                          tags$img(src = "media/study_map_2.jpeg", width = "100%"),
                          tags$br(), # Break
                          tags$em("Map of study transect") # Image of study transect
                  )
                  
              ) # END synchro about box
              
            ), # End fluid row
            
            # Extra space
            headerPanel(""),
            headerPanel(""),
            
            fluidRow(
            box(
              
              width = 12,
                  title = tagList(icon("vial"), strong("Synchro Methods")),
                  includeMarkdown("text/synchro_about_2.Rmd"),
            
            
            # Image of sampling diagram ----
            
            fluidRow(
              column(width = 9, offset = 1,  # offset pushes it slightly right
                     style = "float: none; margin: 0 auto;",  # this centers the column
                     tags$div(
                       style = "text-align: center;",
                       tags$img(class = "banner", src = "media/methods_2.png",
                                style = "max-width: 100%; height: auto; display: block; margin: 0 auto;"),
                       tags$div(
                         style = "width: 100%; text-align: center; margin-top: 10px;",
                         tags$em("Diagram of sampling methods from the offshore wind station (left) to Elkhorn Slough (right)")
                       )
                     ))
              ), # End fluid row
            
            # Extra space
            headerPanel(""),
            headerPanel(""),
            
            # Add images ----
            fluidRow(
             column(4, align = "center",
                     tags$img(src = "media/station.jpg", width = "100%"),
                     tags$br(),
                     tags$em("An offshore sampling station")
              ),
              column(4, align = "center",
                     style = "margin-top: 10px;",
                     tags$img(src = "media/ctd.jpeg", width = "100%"
                     ),
                     tags$br(),
                     tags$em("A CTD cast")),
              
            
              column(4, align = "center",
                     tags$img(src = "media/elkhorn.jpg", width = "100%"),
                     tags$br(),
                     tags$em("Processing samples at Elkhorn Slough")
              )
              ), # End fluid row
          
            # Extra space 
            headerPanel(""),
            headerPanel(""),
            
            # Add last Synchro methods box ----
            fluidRow(
              column(width = 12, 
                  includeMarkdown("text/synchro_about_3.Rmd")),
            ), # End fluid row
            
          
            # Add images ----
            fluidRow(
              
              column(4, offset = 2, align = "center", 
                     tags$img(src = "media/filter_bags.jpg", width = "70%"),
                     tags$br(),
                     tags$em("Active filtration on board")
              ),
              column(4, align = "center", 
                     tags$img(src = "media/torpe.jpg", width = "70%"),
                     tags$br(),
                     tags$em("Passive filtration using a TorpeDNA device")
              )
  
            ) # End fluid row
            )
            )

    ), # END welcome tabItem
    
    # ......................Start dashboard tabItem ............................
    tabItem(tabName = "dashboard",
            
            # Start dashboard tabsetPanel ----
            tabsetPanel(id = "dashboard_tabsetPanel",
                        
                        # Start dashboard tabPanel ----
                        tabPanel(title = strong("Indicator Taxa Dashboard"),
                                 
                                 # Start background fluid row ----
                                 fluidRow(
                                   box(width = 12,
                                       includeMarkdown("text/dashboard_about.Rmd")
                                   )
                                   
                                 ),
                                 
                                 # Add space
                                 headerPanel(""),
                                 headerPanel(""),
                                 
                                 # Start summary box fluid row ----
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

                                 # Extra space 
                                 headerPanel(""),
                                 headerPanel(""),
                                 
                                 # Start map about fluid row ----
                                 fluidRow(
                                   box(width = 12,
                                       includeMarkdown("text/map_about.Rmd")
                                   )
                                   
                                 ),

                                 # Start fluidRow inputs ----
                                 fluidRow(
                                   
                                   # Start picker column ----
                                   # Species selection
                                   column( 
                                     width = 4,
                                     selectInput(
                                       inputId = "species_input",
                                       label = "Indicator taxa:",
                                       choices = NULL )),
                                     
                                     # Season selection
                                   column(
                                     width = 4,
                                     pickerInput(
                                       inputId = "season_input",
                                       label = "Season:",
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = pickerOptions(actionsBox = TRUE)
                                     )), 
                                     
                                     # Depth selection
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
                                          plotlyOutput("indicator_sp_map", height = "600px"),
                                          type = 4, 
                                          color = "#3da48c"
                                   ),
                                   
                                   # Barplot output ----
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

                                 # START - fluid row depth about
                                 fluidRow(
                                   box(width = 12,
                                       includeMarkdown("text/depth_about.Rmd")
                                   )
                                   ), # End fluid row depth about
                                 
                                 
                                 headerPanel(""),
                                 
                                 # START - depth plot fluid row
                                 fluidRow(
                                   
                                
                                   box(width = 12,
                                       height = "700px",
                                       h4(strong("Percentage of samples containing each taxon by depth")),
                                       
                                       headerPanel(""),
                                       headerPanel(""),
                                       girafeOutput(outputId = "depth_plot", height = "600px")
                                   )
                                   
                                   
                                 ), # End - depth plot fluid row
                                 
                                 headerPanel(""),
                                 headerPanel(""),
                                 
                                 # Start fluid row bathymetry plot
                                 fluidRow(
                                   box(width = 12,
                                       includeMarkdown("text/bathymetry_about.Rmd"))
                                   ), # End fluid row bathymetry_about.rmd
                                 fluidRow(
                                   box(width = 12,
                                       h4(strong("Number of samples with indicator taxa detected")),
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

