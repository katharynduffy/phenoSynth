library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(geojsonio)
library(shiny)
library(shinyjs)
library(leaflet.extras)
library(sp)
library(rvest)

server = function(input, output, session) {
  # # variables
  url <- "https://phenocam.sr.unh.edu/webcam/network/table/"
  cams <- url %>%
    read_html() %>%
    html_nodes(xpath='//*[@id="main"]/table') %>%
    html_table()

  cams_ = cams[[1]]
  site_names = cams_$Camera
  layers_ = providers[0:-1]
  
  # # Create the map
  output$map <- renderLeaflet({
    leaflet(data= cams_) %>%
      addTiles() %>%
      addProviderTiles('Esri.WorldImagery')%>%
      # addMarkers(~Lon, ~Lat, label=~Camera,
      #            labelOptions = labelOptions(noHide = F, direction = "bottom",
      #                                        style = get_marker_style())) %>%
      addDrawToolbar(
        targetGroup='draw',
        singleFeature = TRUE,
        polylineOptions=FALSE,
        rectangleOptions = FALSE,
        polygonOptions = drawPolygonOptions(
          shapeOptions = drawShapeOptions(clickable=TRUE,
                                          color='blue',
                                          fillColor='blue')),
        markerOptions = FALSE,
        circleOptions = FALSE)  %>%
      addLayersControl(overlayGroups = c('draw'), options =
                         layersControlOptions(collapsed=FALSE),
                       position = "topleft") 
      # setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  output$createROI <- renderText({
    #use the draw_stop event to detect when users finished drawing
    req(input$map_draw_stop)
    print(input$map_draw_new_feature)
    feature_type <- input$map_draw_new_feature$properties$feature_type
    
    if(feature_type %in% c("polygon")) {
      
      #get the coordinates of the polygon
      polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
      
      #transform them to an sp Polygon
      drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      
      #use over from the sp package to identify selected cities
      selected_cities <- cities_coordinates %over% SpatialPolygons(list(Polygons(list(drawn_polygon),"drawn_polygon")))
    } 
    
  })
  

  # Zoom contiguous US
  observe({
    z = input$usZoom
    print('Running Zoom to contiguous US')
    lo = -105.06
    la = 40.55
    leafletProxy("map", data = cams_) %>%
      setView(lng = lo, lat = la, zoom = 4.5)
  })
  

  # Add All sites back to map
  observe({
    showAll = input$showSites
    print('Running add All sites back to map')
    leafletProxy("map", data = cams_) %>%
      addMarkers(~Lon, ~Lat, label=~Camera, labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                                        style = get_marker_style()))
  })

  # Change Map Layer 1
  observe({
    layers = input$layer
    print('Running Change MAp Layer 1')
    print (layers)
    leafletProxy("map", data = cams_) %>%
      clearTiles() %>%
      addProviderTiles(layers)
  })

  # Change map Layer 2 (with transparency/opacity)
  observe({
    layers = input$layer2
    transparency = input$opacity
    print('Running Change map Layer 2')
    leafletProxy("map", data = cams_) %>%
      addProviderTiles(layers, options=providerTileOptions(opacity = transparency))
  })
  
  
  # Draws roi polyline for a site location
  observe({
    roi_bool = input$drawROI
    if (roi_bool == TRUE){
      site = input$site
      print('Running Draw a polyline roi for a site')
      azm = input$azm
      zoom = FALSE
      site_data = zoom_to_site(site, site_names, zoom)
      run_add_polyline_2(site_data, azm)
    }
    else if (roi_bool == FALSE){
      remove_polyline()} 
    

  })
  
  # # Draws fov polyline for a site location
  # observe({
  #   fov_bool = input$drawFOV
  #   if (fov_bool == TRUE){
  #     site = input$site
  #     print('Running Draw a polyline fov for a site')
  #     azm = input$azm
  #     zoom = FALSE
  #     site_data = zoom_to_site(site, site_names, zoom)
  #     run_add_polyline(site_data, azm)
  #   }
  #   else if (fov_bool == FALSE){
  #     remove_polyline()} 
  # })
  
  
  # Show Popup box for site when clicked
  observe({
    event = input$map_marker_click
    print('Running show a popup box for Site')
    if(is.null(event))
      return()

    isolate({
      leafletProxy("map", data = cams_) %>% clearPopups()
      site = input$site

      lat = event$lat
      lon = event$lng

      site_ = get_site_name(lat,lon)

      site_data = get_site_info(site_, site_names)

      description = site_data$`Site Description`
      elevation =site_data$`Elev(m)`
      camera = site_data$Camera

      get_site_popup(camera, lat, lon, description, elevation)
    })
  })
  
  # Zoom to selected Site
  observe({
    site = input$site
    print('Running Zoom to selected site')
    zoom = TRUE
    site_data = zoom_to_site(site, site_names, zoom)
  })


  #---------------------------------------------------------------------------------------
  #  FUNCTIONS
  #---------------------------------------------------------------------------------------

  # Radians to degrees
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  
  # Given row from sites, create points for polyline from site
  run_add_polyline = function(site_data_, azm_){
    los = .02
    lat =  site_data_$Lat
    lon =  site_data_$Lon
    dst = sqrt(los**2 + los**2)
    c = rotate_pt(lon, lat, (azm_-25), dst)
    b = rotate_pt(lon, lat, (azm_+25), dst)
    cx = c[[1]]
    cy = c[[2]]
    bx = b[[1]]
    by = b[[2]]
    
    datalon = c(lon,cx,bx,lon)
    datalat = c(lat,cy,by,lat)
    camera = site_data_$Camera
    id_ = paste('fov',camera, sep='')
    add_polyline(datalon, datalat, id_, .45, 'red')
  }
  
  run_add_polyline_2 = function(site_data_, azm_){
    
    lat =  site_data_$Lat
    lon =  site_data_$Lon 
    camera = site_data_$Camera
    # Line of site far
    los = .02
    # Line of site Near
    nlos = .001
    
    # half of the Distance across at los
    dlos = .010
    # half of the Distance across at los
    dnlos = nlos/2
    

    # closest left point
    angle = rad2deg((atan(-dlos /los)))
    hyp = los / cos(atan(-dlos / los))
    pt1 = rotate_pt(lon, lat, azm_ + angle, hyp)
    ax = pt1[[1]]
    ay = pt1[[2]]
    
    # closest right point
    angle = rad2deg((atan(dlos /los)))
    hyp = los / cos(atan(dlos / los))
    pt2 = rotate_pt(lon, lat, azm_ + angle, hyp)
    bx = pt2[[1]]
    by = pt2[[2]]
    
    # furthest right point
    angle = rad2deg((atan(dnlos / nlos)))
    hyp = nlos / cos(atan(dnlos / nlos))
    pt3 = rotate_pt(lon, lat, azm_ + angle, hyp)
    cx = pt3[[1]]
    cy = pt3[[2]]
    
    # furthest left point
    angle = rad2deg((atan(-dnlos / nlos)))
    hyp = nlos / cos(atan(-dnlos / nlos))
    pt4 = rotate_pt(lon, lat, azm_ + angle, hyp)
    dx = pt4[[1]]
    dy = pt4[[2]]
    
    datalon = c(ax,bx,cx,dx,ax)
    datalat = c(ay,by,cy,dy,ay)
    id_ = paste('roi',camera, sep='')
    add_polyline(datalon, datalat, id_, .45, 'red')
  }
  
  
  
  # Add a polyline layer to the map
  add_polyline = function(datalon_, datalat_, id_, opacity_, color_){
    leafletProxy("map", data = cams_) %>%
      # clearShapes()%>%
      addPolylines( datalon_,
                    datalat_,
                    layerId=id_,
                    opacity=opacity_,
                    color = color_)
  }
  
  # remove all polylines
  remove_polyline = function(id_){
    leafletProxy("map", data=cams_) %>%
      clearShapes()
  }
  
  # Zoom to site
  zoom_to_site = function(site_, site_names_, zoom){
    site_data = get_site_info(site_, site_names_)
    lat =  site_data$Lat
    lon =  site_data$Lon
    description = site_data$`Site Description`
    elevation =site_data$`Elev(m)`
    camera = site_data$Camera
    
    if (zoom == TRUE){
    leafletProxy('map', data = cams_) %>%
      clearPopups() %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(lng=lon,lat=lat,label=site_, labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                                          style = get_marker_style())) %>%
      setView(lng = lon, lat = lat, zoom = 15)
    }

    return (site_data)
  }
  
  
  # Rotate a point based on AZM
  rotate_pt = function(lon, lat, azm, r){
    rad = azm * (pi / 180)
    lon_ = lon + (r * sin(rad))
    lat_ = lat + (r * cos(rad))
    return (list(lon_, lat_))
  }
  

  # Get site name using lat long coordinates
  get_site_name = function(lat, lon){
    site_data_ = subset(cams_, cams_$Lon == lon & cams_$Lat == lat)
    cam = site_data_$Camera
    return (cam)
  }

  # displays the site info when a site is clicked
  get_site_popup <- function(camera, lat, lng, description, elevation) {
    website = sprintf('https://phenocam.sr.unh.edu/webcam/sites/%s/',camera)

    pop = paste0('<div class="leaflet-popup-content">',
                 '<h4>','Site name: ', camera,'</br></h4>',
                 '<strong>','lat, long: ','</strong>',lat,', ', lng,'</br>',
                '<strong>','Site Description: ','</strong>', description ,'</br>',
                '<strong>','Elevation: ','</strong>', elevation ,'</br>',
                '<a id="info" href=', website ,' style="text-indent: 0px;"
                class="action-button shiny-bound-input"
                onclick="{Shiny.onInputChange(\'info\', (Math.random() * 1000) + 1);}">',

                '<a type="submit" href=', website ,' class="button">Go to Phenocam website</a>')
                

    leafletProxy("map",data = cams_) %>% addPopups(lng, lat, popup = pop, layerId = camera)
  }

  # Get specific site data and returns lon/lat/camera/description/elevation
  get_site_info = function(site, data){
    c = 0
    for (x in data){
      c =  c+ 1
      if (x == site){
        r = c
      }
    }
    site_data = cams_ %>% slice(r)
    return (site_data)
  }

  # Style marker to add to the map when redrawing
  get_marker_style = function(){
    style = list(
      "color" = "green",
      "font-family" = "serif",
      "font-style" = "italic",
      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
      "font-size" = "12px",
      "border-color" = "rgba(0,0,0,0.5)"
    )
    return(style)
  }
}