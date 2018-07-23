# UI file for Shiny App phenoRemote

# Initiate the UI
ui = fluidPage(navbarPage("APIS Phenocam C.2", id="navbar",
                          
                          tabPanel("Home",
                                   
                                   
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
                                      draggable = FALSE, top = 40, left = "auto", right = 20, bottom = "auto",
                                      width = 280, height = "auto",
                                      uiOutput('test'),
                                      h2("Site explorer"),
                                      actionButton("usZoom", "Show Contiguous US"),
                                      actionButton('showSites', 'Show all Sites'),
                                      selectInput("site", "Phenocam Site Name", site_names, selected = 'acadia'),
                                      actionButton("siteZoom", "Zoom to Selected Site"),
                                      selectInput("layer", "Layer", layers_, selected = 'Esri.WorldTopoMap' ),
                                      selectInput("filterSites", 'Filter Sites by', site_filters, selected = 'All', multiple = FALSE),
                                      checkboxInput("drawROI", "See Field of View (FOV)", value = FALSE),
                                      checkboxInput('drawImage', "Show site phenocamImage", value = TRUE),
                                      checkboxInput("drawImageROI", "Show roi on phenocamImage", value = FALSE),
                                      sliderInput("azm", "Toggle FOV:", min = 0, max = 360, value = 0.0, step = 5),
                                      # selectInput('imageSize', 'Image Size', c('None', 'Small', 'Medium', 'Large'), selected = 'Small'),
                                      verbatimTextOutput("mouse")
                        ),
                        # absolutePanel(id = "currentImage", class = "panel panel-default", fixed = TRUE,
                        #               draggable = TRUE, top = 'auto', left = 'auto', right = 20 , bottom = 20,
                        #               width = image_sizes_w$Medium, height = image_sizes_h$Medium,
                        #               uiOutput('phenoImage')
                        # ),
                        absolutePanel(id = 'currentImage', class = 'panel panel-default', #fixed = TRUE,
                                      draggable = TRUE,  top = 'auto', left = 'auto', right = 20 , bottom = 20, 
                                      width = 375, height = 225,
                                      tags$div(id = 'image')
                                      # uiOutput('paneltest'),
                                      # uiOutput('paneltest2')
                                      # uiOutput('phenoImage')
                                      # uiOutput('phenoROI')
                                      ),
                        # uiOutput(paneltest),
                        
                        
                        tags$div(id="cite",
                                 ' ', tags$em(''), ' '#eventually we can put some APIS text here so I'm saving it for now
                        )
                    )
           ),

           # tabPanel('Site information',
           #          
           #          tags$div(id='info1',
           #                   'This is the info about our phenocamsite'),
           # 
           #          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
           #                        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
           #                        width = 330, height = "auto",
           # 
           #                        h2("Site explorer"),
           #                        actionButton("showimage", "Show Phenocam Image"),
           #                        actionButton('showroi', 'Toggle Phenocam FOV'),
           # 
           #                        selectInput("site1", "Phenocam Site Name", site_names)
           #                        # sliderInput("opacity", "Opacity:", min = 0, max = 1, value = 0.0, step = 0.1),
           #                        # selectInput("layer2", "Add Transparent Layer", layers_, 'Esri.NatGeoWorldMap')
           # 
           # 
           #          )
           #        ),

           
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
