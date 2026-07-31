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
    menuItem(text = "Map", tabName = "map", icon = icon("map")),
    menuItem(text = "Depth", tabName = "depth", icon = icon("ship")),
    menuItem(text = "Seasonal change", tabName = "season", icon = icon("leaf")),
    menuItem(text = "Depth case study", tabName = "depth_lantern", icon = icon("fish"))
    
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
    tabItem(tabName = "map",
            
            # Start map tabsetPanel ----
            tabsetPanel(id = "dashboard_tabsetPanel",
                        
                        # Start map tabPanel ----
                        tabPanel(title = "Indicator Taxa Map",
                                 
                                 # Start about fluid row ----
                                 fluidRow(
                                   box(width = 12,
                                       includeMarkdown("text/map_about.Rmd")
                                   )
                                   
                                 ),
                                 
                                 headerPanel(""),
                                 headerPanel(""),
                                 
                                 # Start fluidRow ----
                                 fluidRow(
                                   
                                   # Start picker column ----
                                   column( 
                                     width = 3,
                                     # Drop down input: select species
                                     selectInput(
                                       inputId = "species_input",
                                       label = "Indicator taxa:",
                                       choices = NULL ),
                                     
                                     # Dropdown to select date 
                                     pickerInput(
                                       inputId = "season_input",
                                       label = "Season:",
                                       choices = NULL,
                                       multiple = TRUE,
                                       options = pickerOptions(actionsBox = TRUE)
                                     ), 
                                     
                                     # Select depth range
                                     pickerInput(
                                       inputId = "depth_map_input",
                                       label = "Environmental data depth:",
                                       choices = NULL
                                     ),
                                     
                                     valueBoxOutput("total_count", width = NULL),
                                     valueBoxOutput("indicator_count", width = NULL)
                                     
                                     
                                   ),
                                   
                                   # Map output ----
                                   column(5, 
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
                                 
                                 fluidRow(
                                   box(width = 4,
                                       includeMarkdown("text/line_about.Rmd")
                                   ),
                                   
                                   box( width = 8,
                                        
                                        girafeOutput(outputId = "species_line_plot", height = "500px")
                                   )
                                   
                                 ), # End fluidRow
                                 
                                 headerPanel(""),
                                 
                                 # START - depth graph (all taxa) fluid row
                                 fluidRow(
                                   
                                
                                   box(width = 12,
                                       h4(strong("Relative abundance of taxa at each depth")),
                                       
                                       headerPanel(""),
                                       headerPanel(""),
                                       girafeOutput(outputId = "depth_plot", height = "600px")
                                   )
                                   
                                   
                                 ) # End - depth graph (all taxa) fluid row
                                 
                                 
                        ) # End map tabPanel
            ) # End map tabsetPanel
            
            
            
    ), # END map tabItem
    
    # ......................START depth tabItem ..............................
    tabItem(tabName = "depth",
            
            "depth info here"
            
    ), # END depth tabItem
    
    # ......................START season tabItem ..............................
    tabItem(tabName = "season",
            
            "season info here"
            
    ), # END season tabItem
    
    # ......................START depth case study tabItem .....................
    tabItem(tabName = "depth_lantern",
            
            "background info here"
            
    ) # END depth case study tabItem
    
  ) # END tabItems
  
) # END dashboardBody


##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
##                         Combine into dashboardPage                       ----
##~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
dashboardPage(header, sidebar, body)

#ui <- dashboardPage(title = "Synchro eDNA Data: Monterey Bay", header, sidebar, body) # title here updates how title appears in browser tab (need to define here bc title in dashboardHeader has media embedded)





#   # (Page 1) intro tabPanel ----
#   tabPanel(title = "About test dashboard", 
#            fluidRow(
#              column(10, offset = 1,
#                     includeHTML("about.html")
#              )
#            )
#            
#            ),
#   # END (Page 1) intro tabPanel
#   
#   # (Page 2) data viz tabPanel ----
#   tabPanel(
#     title = "Explore the Data",
#     
#     # tabsetPanel to contain tabs for data viz ----
#     tabsetPanel(
#       # tabPanel for indicator species ----
#       tabPanel(
#         title = "Monterey Bay Indicator Species",
#         
#         # trout sidebarLayout ----
#         sidebarLayout(
#           # trout sidebarPanel ----
#           sidebarPanel(
#             width = 3,
#             # Drop down input: select species ----
#             selectInput(
#               inputId = "species_input",
#               label = "Indicator species:",
#               choices = NULL ),
#               # change to NULL (Warning -- server better at handling large lists)
#               # Dropdown to select date in sample date range ----
#               pickerInput(
#                 inputId = "season_input",
#                 label = "Season:",
#                 choices = NULL,
#                 multiple = TRUE,
#                 options = pickerOptions(actionsBox = TRUE)
#               ), 
#             pickerInput(
#               inputId = "depth_map_input",
#               label = "Environmental data depth:",
#               choices = NULL
#             ),
#             
#             br(),
#             
#             # Info box
#             div(
#               style = "background-color: #e7f3ff; padding: 10px; border-left: 4px solid #2196F3; border-radius: 4px;",
#               strong(paste0("Of ", n_unique_total, " unique taxonomic detections, ",
#                             n_unique_indicators, " are indicator species.")),
#               p("Select a species and season to explore distribution patterns.", 
#                 style = "margin-top: 5px; font-size: 12px;")
#             ),
#           
#             
#             ),# END sidebarPanel
#             
#           
#           mainPanel(
#             width = 9,
#             fluidRow(
#               column(8, 
#                      leafletOutput("indicator_sp_map", height = "700px")
#               ),
#               column(4,
#                      # conditionalPanel(
#                      #   condition = "input.indicator_sp_map_marker_click && input.species_input == 'All species'",
#                        wellPanel(
#                          h5(strong("Species Composition")),
#                          plotlyOutput("species_bar_plot", height = "650px")
#                        )
#                     # )
#               )
#               
#             )
#           ) # End mainPanel
#       
#         
#           ), # END  sidebarLayout - map
#         
#         hr(), # Break
#         
#         # Begin depth panel ----
#         # depth sidebarPanel 
#         sidebarLayout(
#          
#           sidebarPanel(
#             # Drop down input: select species ----
#             selectInput(
#               inputId = "season_depth_input",
#               label = "Select a season:",
#               choices = NULL ),
#             # change to NULL (Warning -- server better at handling large lists)
#             
#           ),# END sidebarPanel - depth plot
#           
#           
#           # depth mainPanel ----
#           mainPanel(
#             girafeOutput(outputId = "depth_plot", height = "800px")
#           ) # End mainPanel - Depth plot
#         
#         ), # End sidebarLayout - Depth plot
#         
#         hr(), # Break
#         
#         # Begin species relative abundance/season panel 
#         
#         sidebarLayout(
#           
#           sidebarPanel(
#             # Drop down input: select species ----
#             selectInput(
#               inputId = "species_season_line_input",
#               label = "Select a species:",
#               choices = NULL ),
#             # change to NULL (Warning -- server better at handling large lists)
#             
#           ),# END sidebarPanel - depth plot
#           
#           # depth mainPanel ----
#           mainPanel(
#             girafeOutput(outputId = "species_line_plot")
#           ) # End mainPanel - Depth plot
#           
#         ), # End sidebarLayout - species line graph 
#         
#         hr(), # Break
#         
#         # Begin Lantern panel
#         
#         sidebarLayout(
#           
#           sidebarPanel(
#             # Drop down input: select species ----
#             pickerInput(
#               inputId = "season_lantern",
#               label = "Select a season:",
#               choices = unique(map_data$Season),
#               selected = unique(map_data$Season),
#               multiple = TRUE),
#               
#             # change to NULL (Warning -- server better at handling large lists)
#             
#           ),# END sidebarPanel - depth plot
#           
#           # depth mainPanel ----
#           mainPanel(
#             plotlyOutput(outputId = "lantern_plot")
#           ) # End mainPanel - Lantern plot
#           
#         ) # End sidebarLayout - species line graph 
#         
#           
#         ) # END  tabPanel - Indicator sp
#         
#         
#       ) # END tabsetPanel - Indicator species selection tab
#       
#     ) # END tabPanel - Data Viz
#     

