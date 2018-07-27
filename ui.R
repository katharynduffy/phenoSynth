# UI file for Shiny App phenoRemote
# Initiate the UI
ui = fluidPage(shinyjs::useShinyjs(), navbarPage("APIS Phenocam C.2-development phase", id="navbar",
                          
                          tabPanel("Site explorer",
    
                                   div(class="outer",
                                       
                                       tags$head(
                                         # Include our custom CSS
                                         includeCSS("styles.css"),
                                         includeScript("gomap.js")
                                       ),

                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),

                        textOutput("See Field of View (FOV)"),

                        
                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                                      width = 280, height = "auto",
                                      h2(id = 'explorerPanel', "Site explorer"),
                                      h2(uiOutput("analyzerTitle")),
                                      actionButton('siteExplorerMode', 'Back to Site Explorer'),
                                      actionButton("usZoom", "Show Contiguous US"),
                                      actionButton('showSites', 'Show all Sites'),
                                      actionButton('analyzerMode', 'Enter Analyze Mode'),
                                      selectInput("filterSites", 'Filter Sites by', site_filters, selected = 'All', multiple = FALSE),
                                      selectInput("site", "Phenocam Site Name", site_names, selected = 'acadia'),
                                      actionButton("siteZoom", "Zoom to Selected Site"),
                                      actionButton('showModisSubset', 'Show MODIS subset'),
                                      checkboxInput("drawROI", "See Field of View (FOV)", value = FALSE),
                                      checkboxInput('drawImage', "Show site phenocamImage", value = TRUE),
                                      checkboxInput("drawImageROI", "Show roi on phenocamImage", value = FALSE),
                                      sliderInput("azm", "Toggle FOV:", min = 0, max = 360, value = 0.0, step = 5),
                                      verbatimTextOutput("mouse")
                        ),
                        
                        # absolutePanel(id = "analyzerControls", class = "panel panel-default", fixed = TRUE,
                        #               draggable = TRUE, top = 50, left = "auto", right = 20, bottom = "auto",
                        #               width = 280, height = "auto", hidden = TRUE,
                        #               h2(id = 'analyzerPanel',uiOutput("analyzerTitle")),
                        #               actionButton('siteExplorerMode', 'Back to Site Explorer'),
                        #               actionButton('showModisSubset', 'Show MODIS subset'),
                        #               checkboxInput("drawROI", "See Field of View (FOV)", value = FALSE),
                        #               checkboxInput('drawImage2', "Show site phenocamImage", value = TRUE),
                        #               checkboxInput("drawImageROI2", "Show roi on phenocamImage", value = FALSE),
                        #               sliderInput("azm", "Toggle FOV:", min = 0, max = 360, value = 0.0, step = 5),
                        #               verbatimTextOutput("mouse2")
                        # ),

                        absolutePanel(id = 'currentImage', class = 'panel panel-default', #fixed = TRUE,
                                      draggable = TRUE,  top = 'auto', left = 20, right = 'auto' , bottom = 20, 
                                      width = 375, height = 225,
                                      tags$div(id = 'image')
                                      ),
                        
                        absolutePanel(id = 'plotpanel', class = 'panel panel-default', #fixed = TRUE,
                                      draggable = TRUE,  top = 'auto', left = 20, right = 'auto' , bottom = 270, 
                                      width = 375, height = 225,
                                      plotOutput("currentPlot", height = 225)
                        ),

                        tags$div(id="cite",
                                 ' ', tags$em(''), ' '#eventually we can put some APIS text here so I'm saving it for now
                        )
                    )
           ),

           
           tabPanel('pAOI Management',
                    
                    tags$div(id='pAOItab'),
                    selectInput('shapefiles', "Select Shapefile", c('None')),
                    actionButton('saveshp', 'Save Shapefile'),
                    br(),
                    br(), br(),

                    
                    # Attempting to build a chart here for the shapefiles, mihgt move it to a new tab at
                    #   some point......
                    DTOutput("pAOIchart")

           ),
           
           
           tabPanel('Phenocam Table',
                    DTOutput('x1')
           ),
           
           
           conditionalPanel("false", icon("crosshair"))
  )
)
