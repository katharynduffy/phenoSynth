# UI file for Shiny App phenoRemote
# Initiate the UI
ui = fluidPage(shinyjs::useShinyjs(), navbarPage("PhenoSynth-development phase", id="navbar",
                          
                          tabPanel("Site explorer",
    
                                   div(class="outer",
                                       
                                       tags$head(# Include custom CSS
                                                 includeCSS("styles.css"),
                                                 includeScript("gomap.js")
                                                 ),

                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),

                        textOutput("See Field of View (FOV)"),

                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = 70, left = "auto", right = 20, bottom = "auto",
                                      width = 280, height = "auto",
                                      h2(id = 'explorerTitle', "Site explorer"),
                                      h2(id = 'analyzerTitle', "Site Analyzer"),
                                      actionButton('siteExplorerMode', 'Back to Site Explorer'),
                                      actionButton("usZoom", "Show Contiguous US"),
                                      actionButton('showSites', 'Show all Sites'),
                                      actionButton("siteZoom", "Zoom to Selected Site"),
                                      selectInput("filterSites", 'Filter Sites by', site_filters, selected = 'All', multiple = FALSE),
                                      selectInput("site", "Phenocam Site Name", site_names, selected = 'acadia'),
                                      actionButton('analyzerMode', 'Enter Analyze Mode'),
                                      checkboxInput("drawROI", "See Field of View (FOV)", value = FALSE),
                                      sliderInput("azm", "Toggle FOV:", min = 0, max = 360, value = 0.0 , step = 5),
<<<<<<< HEAD
                                      checkboxInput('drawImage', "Show site PhenoCam Image", value = TRUE),
                                      checkboxInput("drawImageROI", "Show ROI on PhenoCam Image", value = FALSE),
                                      selectInput('pftSelection', 'ROI PFT selection', c('DB', 'EN', 'MX')),
=======
                                      checkboxInput('drawImage', "Show site phenocamImage", value = TRUE),
                                      checkboxInput("drawImageROI", "Show roi on phenocamImage", value = FALSE),
                                      selectInput('pftSelection', 'Roi pft selection', ''),
>>>>>>> 3ee04106723275512340443ff75859de88e7be46
                                      actionButton('showModisSubset', 'Plot MODIS subset'),
                                      actionButton('plotPhenocamGCC', 'Plot GCC')
                                      ),

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
                        
                        absolutePanel(id = 'mouseBox', class = 'panel panel-default', fixed = TRUE,
                                      draggable = FALSE,  top = 'auto', left = 'auto', right = 20 , bottom = 20, 
                                      width = 240, height = 40,
                                      verbatimTextOutput("mouse")
                                      ),
                        absolutePanel(id = 'showHidePlot', class = 'panel panel-default', fixed = TRUE,
                                      draggable = FALSE, top = 150, left = 'auto', right = 320, bottom = 'auto',
                                      actionButton('hidePlot', 'Hide Plot')
                                      ),
                        
                        absolutePanel(id = 'siteTitle', class = 'panel panel-default', fixed = FALSE,
                                      draggable = FALSE,  top = 25, left = 'auto', right = 320 , bottom = 'auto',
                                      div(id = 'analyzerHeader',uiOutput("analyzerTitle"), style = 'font-size: 40px; font-weight: 400;')
                                      ),

                        tags$div(id="cite",
                                 ' ', tags$em(''), ' '#eventually we can put some APIS text here so I'm saving it for now
                                )
                            ) # close div outer
                        ), # close tab panel

           
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
