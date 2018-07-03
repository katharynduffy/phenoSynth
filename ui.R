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
                                      draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                      width = 330, height = "auto",
                                      h2("Site explorer"),
                                      actionButton("usZoom", "Show Contiguous US"),
                                      actionButton("add_custom_polyline", "Add Polygons"),
                                      actionButton('showSites', 'Show all Sites'),
                                      selectInput("site", "Phenocam Site Name", site_names, selected = 'luckyhills'),
                                      selectInput("layer", "Layer", layers_, selected = 'Esri.WorldTopoMap' ),
                                      checkboxInput("drawROI", "See Field of View (FOV)", value = FALSE),
                                      sliderInput("azm", "Toggle FOV:", min = 0, max = 360, value = 0.0, step = 5),
                                      verbatimTextOutput("mouse")
                                      
                                      

                        ),

                        tags$div(id="cite",
                                 ' ', tags$em(''), ' '#eventually we can put some APIS text here so I'm saving it for now
                        )
                    )
           ),

           tabPanel('Site information',
                    
                    tags$div(id='info1',
                             'This is the info about our phenocamsite'),

                    absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                  draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
                                  width = 330, height = "auto",

                                  h2("Site explorer"),
                                  actionButton("showimage", "Show Phenocam Image"),
                                  actionButton('showroi', 'Toggle Phenocam FOV'),

                                  selectInput("site1", "Phenocam Site Name", site_names)
                                  # sliderInput("opacity", "Opacity:", min = 0, max = 1, value = 0.0, step = 0.1),
                                  # selectInput("layer2", "Add Transparent Layer", layers_, 'Esri.NatGeoWorldMap')


                    )
                  ),

           
           tabPanel('pAOI Management',
                    
                    tags$div(id='pAOItab',
                             'Adding shapefile data below as it is created in the Home tab'),

                    
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
