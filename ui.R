# UI file for Shiny App phenoRemote
# Initiate the UI
ui = fluidPage(shinyjs::useShinyjs(), includeCSS("./Aesthetics/styles.css"),
               mainPanel(
                 img(src='phenoSynth.png', id = 'phenoSynthLogo'),
                 bsModal("saveShpPopup",
                         "Select Plot Data", "saveShp",
                         tags$head(tags$style("#window .modal{backdrop: 'static'}")),
                         size = "medium",
                         selectInput('shapefiles', "Select Shapefile", c('None')),
                         textInput('paoiUser', 'Email or Name'),
                         textInput('paoiNotes', 'Notes or Comments'),
                         actionButton('emailShp', 'Email shapefile'),
                         br(),br(),
                         actionButton('downloadShp', 'Download shapefile')
                 ),
                 bsModal("getDataPopup",
                         "Get Data for Analysis", "getData",
                         tags$head(tags$style("#window .modal{backdrop: 'static'}")),
                         size = "medium",
                         # checkboxInput("localDownload", "Download Data Locally", value = TRUE),
                         selectInput('dataTypes_get', 'Data Types', multiple = TRUE, selected = c('GCC', 'NDVI', 'EVI','Transition Dates', 'NPN'), c('GCC', 'NDVI', 'EVI', 'Transition Dates', 'NPN')),
                         selectInput('phenocamFrequency', 'GCC Frequency', multiple = FALSE, selected = '3 day', c('1 day', '3 day')),
                         actionButton('getDataButton', 'Get Data'),
                         tags$head(tags$style("#getDataPopup .modal-footer{ display:none}")))
                 ,
                 bsModal("plotDataPopup",
                         "Select Plot Data", "plotRemoteData",
                         tags$head(tags$style("#window .modal{backdrop: 'static'}")),
                         size = "small",
                         selectInput('dataTypes_plot', 'Data Types', multiple = TRUE, selected = c('GCC', 'NDVI', 'EVI', 'NPN'), c('GCC', 'NDVI', 'EVI', 'Transition Dates', 'NPN')),
                         # selectInput('pixelTypes', 'Pixel Resolution', c('250m', '500m')),
                         sliderInput('dataDateRange', 'Date start to end',
                                     min = as.Date('2000-01-01'),
                                     max = Sys.Date(),
                                     value = c(as.Date('2000-01-01'), Sys.Date())),
                         actionButton('plotDataButton', 'Plot Data'),
                         helpText(id = 'noPixelWarning', 'No Pixels selected')
                         # actionButton('genDF', 'Download Data')
                         )
               ,
               bsModal("downloadDataPopup",
                       "Download Data from Plot", "downloadData",
                       tags$head(tags$style("#window .modal{backdrop: 'static'}")),
                       size = "medium",
                       selectInput('dataTypes_download', 'Data Types',selected = 'All Data', multiple = TRUE, c('All Data', 'Transition Dates', 'EVI', 'NDVI', 'GCC', 'NPN')),
                       downloadButton('downloadDataButton', 'Download'),
                       tags$head(tags$style("#getDataPopup .modal-footer{ display:none}")))
               ,
               navbarPage("PhenoSynth-development phase", id="navbar",

                          tabPanel("Site explorer",

                                   div(class="outer",

                                       tags$head(# Include custom CSS
                                                 includeCSS("./Aesthetics/styles.css"),
                                                 includeScript("./Aesthetics/gomap.js")
                                                 ),

                        # If not using custom CSS, set height of leafletOutput to a number instead of percent
                        leafletOutput("map", width="100%", height="100%"),

                        textOutput("See Field of View (FOV)"),

                        # Shiny versions prior to 0.11 should use class = "modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                      draggable = FALSE, top = 70, left = "auto", right = 20, bottom = "auto",
                                      width = 320, height = "auto", style="z-index:600;",
                                      h2(id = 'explorerTitle', "Site Explorer"),
                                      h2(id = 'analyzerTitle', "Site Analyzer"),
                                      actionButton('siteExplorerMode', 'Back to Site Explorer'),
                                      actionButton("usZoom", "Show Contiguous US"),
                                      actionButton('showSites', 'Show all Sites'),
                                      actionButton("siteZoom", "Zoom to Selected Site"),
                                      selectInput("filterSites", 'Filter Sites by', site_filters, selected = 'All', multiple = FALSE),
                                      selectInput("site", "Phenocam Site Name", site_names, selected = 'acadia'),
                                      actionButton('analyzerMode', 'Enter Analyze Mode'),
                                      checkboxInput("drawROI", "See PhenoCam Field of View (FOV)", value = FALSE),
                                      # sliderInput("azm", "Toggle FOV:", min = 0, max = 360, value = 0 , step = 5),
                                      numericInput('azm', 'FOV degrees (1 to 360)',value=0, min=0,max=360),
                                      # selectInput('azm', 'Toggle FOV:', 
                                      #             choices=c('N'=0,'NE'=45,'E'=90,'SE'=135,'S'=180,
                                      #                       'SW'=225, 'W'=270, 'NW'=315)),
                                      checkboxInput('drawImage', "Show site PhenoCam Image", value = TRUE),
                                      checkboxInput("drawImageROI", "Show ROI on PhenoCam Image", value = FALSE),
                                      selectInput('pftSelection', 'PhenoCam ROI Vegetation', ''),
                                      checkboxInput("highlightPixelModeNDVI", "Select MODIS Pixels (250m resolution)", value = FALSE),
                                      actionButton('getData', 'Import Data'),
                                      actionButton('plotRemoteData', 'Plot Data'),
                                      actionButton('clearPixels', 'Clear Pixels')
                                                                            ),

                        absolutePanel(id = 'currentImage', class = 'panel panel-default', #fixed = TRUE,
                                      draggable = TRUE,  top = 'auto', left = 250, right = 'auto' , bottom = 10,
                                      width = 375, height = 225, style="z-index:500;",
                                      actionButton('showImage', '-', value=FALSE),
                                      actionButton('showROIimage', 'Overlay selected ROI'),
                                      tags$div(id = 'image')
                                      ),

                        # rename this id, should be imgroipanel or something(add in server too)
                        absolutePanel(id = 'plotpanel', class = 'panel panel-default', #fixed = TRUE,
                                      draggable = TRUE,  top = 'auto', left = 400, right = 'auto' , bottom = 20,
                                      width = 375, height = 225, style="z-index:500;",
                                      actionButton('hidePlot', '-', value=FALSE),
                                      plotOutput("currentPlot", height = 225)
                                      ),

                        absolutePanel(id = 'mouseBox', class = 'panel panel-default', fixed = TRUE,
                                      draggable = FALSE,  top = 'auto', left = 'auto', right = 20 , bottom = 85,
                                      width = 240, height = 40, style="z-index:500;",
                                      verbatimTextOutput("mouse")
                                      ),

                        absolutePanel(id = 'siteTitle', class = 'panel panel-default', fixed = FALSE, style="z-index:500;",
                                      draggable = FALSE,  top = 25, left = 'auto', right = 320 , bottom = 'auto',
                                      div(id = 'analyzerHeader', uiOutput("analyzerTitle"))
                                      ),

                        tags$div(id="cite",
                                 ' ', tags$em(''), ' '#eventually we can put some APIS text here so I'm saving it for now
                                )
                            ) # close div outer
                        ), # close tab panel


           tabPanel('pAOI Management',

                    tags$div(id='pAOItab',
                    actionButton('saveShp', 'Save/Email Shapefile'),
                    br(),
                    br(), br(),


                    # Attempting to build a chart here for the shapefiles, mihgt move it to a new tab at
                    #   some point......
                    DTOutput("pAOIchart")
                    )

                   ),

           # tabPanel('User Guide',
           #          includeMarkdown('UserGuide.Rmd')
           #          ),

           tabPanel('phenoSynth User Guide',
                    # tableOutput("phenoTable")
                    includeMarkdown('../phenoSynth/Images_for_UserGuide/UserGuide.Rmd')
                   ),
           tabPanel('PhenoCam Metadata',
                    # tableOutput("phenoTable")
                    dataTableOutput('phenoTable')
           ),

           tabPanel('Plot Data', value = 'PlotPanel',
                    checkboxGroupInput("plotTheseBoxes", label = h4("Select Plots to Display"), 
                                       choices = list("GCC" = 'GCC', "All NDVI" = 'all_ndvi', "High Quality NDVI" = 'hiq_ndvi',
                                                      'All EVI' = 'all_evi', 'High Quality EVI' = 'hiq_evi',
                                                      'Transition Dates (EVI/NDVI)' = 'tds_sat'), inline=TRUE,
                                       selected=c('GCC','all_ndvi','hiq_ndvi','all_evi','hiq_evi','tds_sat')),
                    plotlyOutput("data_plot", width='100%', height = 'auto'),
                    hr(),
                    actionButton('downloadData', 'Download Dataframe'),
                    hr(),
                    h2('Selected Pixel Data'),
                    br(),
                    dataTableOutput('plotTable')
                    
           ),

           conditionalPanel("false", icon("crosshair"))
      )
  )
)
