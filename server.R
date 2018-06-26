
server = function(input, output, session) {
  counter <- reactiveValues(countervalue = 0)
  ## Create the map
  output$map <- renderLeaflet({
    leaflet(data = cams_) %>%
      addTiles() %>%
      addProviderTiles('Esri.WorldImagery')%>%
      addMarkers(~lon, ~lat, label=~site, layerId=~site,
                 labelOptions = labelOptions(noHide = F, direction = "bottom",
                                             style = get_marker_style())) %>%
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
                       position = "topleft")%>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  # output for
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
      
    } 
  })
  
  
  # # Zoom to selected Site
  observeEvent(input$site, {
    site = isolate(input$site)
    if (counter$countervalue >0){
      print ('Running Zoom to selected site')
      site_data = zoom_to_site(site, site_names, zoom=TRUE)
    }
  })
  
  #an observer for a test button for quality control
  observe({
    test = input$testbutton
    if(is.null(test))
      return()
    
    isolate({
      event = input$site
      site_data = get_site_info(event, site_names)
      lat = site_data$lat
      lon = site_data$lon
      description = site_data$site_description
      elevation =site_data$elev
      camera = site_data$site
      site_type = site_data$site_type
      nimage = site_data$nimage
      cam_orientation = as.character(site_data$camera_orientation)
      degrees = as.numeric(orientation_key[cam_orientation])
      
      print (cam_orientation)
      print (degrees)
      
      active = site_data$active
      date_end = site_data$date_end
      date_start = site_data$date_start
      })
  })


  # Zoom contiguous US
  observe({
    z = input$usZoom
    
    if (z > 0){
      isolate({
        print('Running Zoom to contiguous US')
        lo = -105.06
        la = 40.55
        leafletProxy("map", data = cams_) %>%
          setView(lng = lo, lat = la, zoom = 4.5)
    })
    }
  })
  

  # Add All sites back to map
  observe({
    showAll = input$showSites
    print('Running add All sites back to map')
    leafletProxy("map", data = cams_) %>%
      addMarkers(~lon, ~lat, label=~site, layerId=~site, labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                                        style = get_marker_style()))
    count()
  })

  # Change Map Layer 1
  observe({
    layers = input$layer
    print('Running Change Map Layer 1')
    leafletProxy("map", data = cams_) %>%
      clearTiles() %>%
      addProviderTiles(layers)
  })

  # # Change map Layer 2 (with transparency/opacity)
  # observe({
  #   layers = input$layer2
  #   transparency = input$opacity
  #   print('Running Change map Layer 2')
  #   leafletProxy("map", data = cams_) %>%
  #     addProviderTiles(layers, options=providerTileOptions(opacity = transparency))
  # })
  
  # Observer for the azm changes from 0-360 on the slider
  observe({
    azm = input$azm
    if(is.null(azm))
      return()
    isolate({
      if (input$drawROI == TRUE){
        site = input$site
        site_data = get_site_info(site, site_names)
        run_add_polyline_2(site_data, azm)
      }
    })
  })
  
  # Draws roi polyline for a site location
  observe({
    roi_bool = input$drawROI
    if (roi_bool == TRUE){
      site = isolate(input$site)
      site_data = get_site_info(site, site_names)
      cam_orientation = as.character(site_data$camera_orientation)
      degrees = as.numeric(orientation_key[cam_orientation])
      run_add_polyline_2(site_data, degrees)
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
    
    print (event$id)
    if (is.not.null(event$id)){
      zoom_to_site(event$id, site_names, zoom=TRUE)
      updateSelectInput(session, 'site', selected = event$id)
    }
    
    print('Running show a popup box for Site')
    if(is.null(event))
      return()

    isolate({
      leafletProxy("map", data = cams_) %>% clearPopups()
      site = input$site

      site_data = get_site_info(site, site_names)
      
      lat = site_data$lat
      lon = site_data$lon
      description = site_data$site_description
      elevation =site_data$elev
      camera = site_data$site
      site_type = site_data$site_type
      nimage = site_data$nimage
      cam_orientation = as.character(site_data$camera_orientation)
      degrees = as.numeric(orientation_key[cam_orientation])
      active = site_data$active
      date_end = site_data$date_end
      date_start = site_data$date_start

      get_site_popup(camera, lat, lon, description, elevation, site_type, cam_orientation, degrees , nimage,
                     active, date_end, date_start)
    })
  })


  #---------------------------------------------------------------------------------------
  #  FUNCTIONS
  #---------------------------------------------------------------------------------------

  # Radians to degrees
  rad2deg <- function(rad) {(rad * 180) / (pi)}
  
  # Given row from sites, create points for polyline from site
  run_add_polyline = function(site_data_, azm_){
    los = .02
    lat =  site_data_$lat
    lon =  site_data_$lon
    dst = sqrt(los**2 + los**2)
    c = rotate_pt(lon, lat, (azm_-25), dst)
    b = rotate_pt(lon, lat, (azm_+25), dst)
    cx = c[[1]]
    cy = c[[2]]
    bx = b[[1]]
    by = b[[2]]
    
    datalon = c(lon,cx,bx,lon)
    datalat = c(lat,cy,by,lat)
    camera = site_data_$site
    id_ = paste('fov',camera, sep='')
    add_polyline(datalon, datalat, id_, .45, 'red')
  }
  
  run_add_polyline_2 = function(site_data_, azm_){
    
    lat =  site_data_$lat
    lon =  site_data_$lon 
    camera = site_data_$site
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
    
    updateSliderInput(session, 'azm', value = azm_)
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
    description = site_data$site_description
    camera_orientation = site_data$camera_orientation
    lat = site_data$lat
    lon = site_data$lon
    cam_orientation = as.character(site_data$camera_orientation)
    degrees = as.numeric(orientation_key[cam_orientation])
    elevation = site_data$elev
    camera = site_data$site
    drawROI = FALSE
    
    if (zoom == TRUE){
    drawROI = isolate(input$drawROI)
    leafletProxy('map', data = cams_) %>%
      clearPopups() %>%
      clearMarkers() %>%
      clearShapes() %>%
      addMarkers(lng=lon,lat=lat,label=site_, labelOptions = labelOptions(noHide = F, direction = "bottom",
                                                                          style = get_marker_style())) %>%
      setView(lng = lon, lat = lat, zoom = 15)
    }
    
    if (drawROI){
      run_add_polyline_2(site_data, degrees)
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

  #displays the site info when a site is clicked
  get_site_popup <- function(camera_, lat_, lng_, description_, elevation_, site_type_, 
                             camera_orientation_, degrees_, nimage_,
                             active_, date_end_, date_start_) {
    website = sprintf('https://phenocam.sr.unh.edu/webcam/sites/%s/',camera_)
    
    

    pop = paste0('<div class="leaflet-popup-content">',
                 '<h4>','Site name: ', camera_,'</br></h4>',
                 '<strong>','lat, long: ','</strong>',lat_,', ', lng_,'</br>',
                '<strong>','Site Description: ','</strong>', description_ ,'</br>',
                '<strong>','Elevation: ','</strong>', elevation_ ,'</br>',
                
                '<strong>','Site type: ','</strong>', site_type_ ,'</br>',
                '<strong>','Orientation (direction): ','</strong>', camera_orientation_ ,'</br>',
                '<strong>','Number of Images: ','</strong>', nimage_ ,'</br>',
                '<strong>','Active: ','</strong>', active_ ,'</br>',
                '<strong>','Start date: ','</strong>', date_start_ ,'</br>',
                '<strong>','End date: ','</strong>', date_end_ ,'</br>',
                
                '<a id="info" href=', website ,' style="text-indent: 0px;"
                class="action-button shiny-bound-input"
                onclick="{Shiny.onInputChange(\'info\', (Math.random() * 1000) + 1);}">',

                '<a type="submit" href=', website ,' class="button">Go to Phenocam website</a>')
                

    leafletProxy("map",data = cams_) %>% addPopups(lng_, lat_, popup = pop, layerId = camera_)
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
  
  count = function(){
  isolate({
    counter$countervalue = counter$countervalue + 1
    print (counter$countervalue)
    })
  }
  
  
}