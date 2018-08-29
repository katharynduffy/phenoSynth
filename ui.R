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
                                      width = 320, height = "auto",
                                      h2(id = 'explorerTitle', "Site explorer"),
                                      h2(id = 'analyzerTitle', "Site Analyzer"),
                                      actionButton('siteExplorerMode', 'Back to Site Explorer'),
                                      actionButton("usZoom", "Show Contiguous US"),
                                      actionButton('showSites', 'Show all Sites'),
                                      actionButton("siteZoom", "Zoom to Selected Site"),
                                      selectInput("filterSites", 'Filter Sites by', site_filters, selected = 'Type1', multiple = FALSE),
                                      selectInput("site", "Phenocam Site Name", site_names, selected = 'acadia'),
                                      actionButton('analyzerMode', 'Enter Analyze Mode'),
                                      checkboxInput("drawROI", "See Field of View (FOV)", value = FALSE),
                                      sliderInput("azm", "Toggle FOV:", min = 0, max = 360, value = 0.0 , step = 5),
                                      checkboxInput('drawImage', "Show site PhenoCam Image", value = TRUE),
                                      checkboxInput("drawImageROI", "Show ROI on PhenoCam Image", value = FALSE),
                                      selectInput('pftSelection', 'PhenoCam ROI Vegetation', ''),
                                      actionButton('showModisSubset', 'Plot MODIS subset'),
                                      actionButton('plotPhenocamGCC', 'Plot GCC'),
                                      checkboxInput("highlightPixelMode", "Highlight Pixel Mode", value = FALSE),
                                      actionButton('getAPPEEARSpoints', 'AppEEARS')

                                                                            ),

                        absolutePanel(id = 'currentImage', class = 'panel panel-default', #fixed = TRUE,
                                      draggable = TRUE,  top = 'auto', left = 250, right = 'auto' , bottom = 10,
                                      width = 375, height = 225,
                                      actionButton('showImage', '-', value=FALSE),
                                      actionButton('showROIimage', 'Overlay selected ROI'),
                                      tags$div(id = 'image')
                                      ),
                        
                        # absolutePanel(id = 'modisLegend', class = 'panel panel-default', #fixed = TRUE,
                        #               draggable = TRUE,  top = 'auto', left = 20, right = 'auto' , bottom = 270,
                        #               width = 375, height = 225,
                        #               tags$div(id = 'image2')
                        #               ),

                        absolutePanel(id = 'plotpanel', class = 'panel panel-default', #fixed = TRUE,
                                      draggable = TRUE,  top = 'auto', left = 400, right = 'auto' , bottom = 20,
                                      width = 375, height = 225,
                                      actionButton('hidePlot', '-', value=FALSE),
                                      plotOutput("currentPlot", height = 225)
                                      ),

                        absolutePanel(id = 'mouseBox', class = 'panel panel-default', fixed = TRUE,
                                      draggable = FALSE,  top = 'auto', left = 'auto', right = 20 , bottom = 85,
                                      width = 240, height = 40,
                                      verbatimTextOutput("mouse")
                                      ),

                        absolutePanel(id = 'siteTitle', class = 'panel panel-default', fixed = FALSE,
                                      draggable = FALSE,  top = 25, left = 'auto', right = 320 , bottom = 'auto',
                                      div(id = 'analyzerHeader', uiOutput("analyzerTitle"))
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
           tabPanel('User Guide',
                    includeMarkdown('UserGuide.Rmd')
                    ),


           tabPanel('Phenocam Table',
                    DTOutput('x1')
                   ),


           conditionalPanel("false", icon("crosshair"))
  )
)
