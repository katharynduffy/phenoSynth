# Server file for Shiny App phenoRemote

server = function(input, output, session) {
  
  counter = reactiveValues(countervalue = 0)
  data = reactiveValues(
    headers = c('Name', 'Site', 'Run', 'Notes'),
    lons = c(),
    lats = c(),
    names = c(),
    run = 0,
    run_log = c(),
    df = data.frame())
  #---------------------------------------------------------------------------------------
  #  OUTPUTS
  #---------------------------------------------------------------------------------------
  
  ## Create the Phenocam Datatable with basic info
  x = cams_
  x$Date = Sys.time() + seq_len(nrow(x))
  output$x1 = renderDT(x, selection = 'none', editable = FALSE)
  
  proxy = dataTableProxy('x1')
  
  observeEvent(input$x1_cell_edit, {
    info = input$x1_cell_edit
    str(info)
    i = info$row
    j = info$col
    v = info$value
    x[i, j] <<- DT::coerceValue(v, x[i, j])
    replaceData(proxy, x, resetPaging = FALSE)  # important
  })
  
  
  ## Create the map
  output$map = renderLeaflet({
    leaflet(data = cams_, options= leafletOptions(zoomControl=FALSE)) %>%
      addTiles() %>%
      addProviderTiles('Esri.WorldTopoMap')%>%
      addDrawToolbar(
        singleFeature = TRUE,
        polylineOptions=FALSE,
        rectangleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        circleOptions = FALSE,
        polygonOptions = drawPolygonOptions(
          shapeOptions = drawShapeOptions(clickable=TRUE,
                                          color='blue',
                                          fillColor='blue')))  %>%
      # Rendering the mouseoutput (aka lat / lon)
      onRender("function(el,x){
        this.on('mousemove', function(e) {
        var lat = e.latlng.lat;
        var lng = e.latlng.lng;
        var coord = [lat, lng];
        Shiny.onInputChange('hover_coordinates', coord)});
        this.on('mouseout', function(e) {
        Shiny.onInputChange('hover_coordinates', null)})
      }")  %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  # adds the mouse lat / lon to an output (we can change this to anything)
  output$mouse <- renderText({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      paste0("Lat: ", input$hover_coordinates[1], 
             "\nLng: ", input$hover_coordinates[2])
    }
  })

  
  #---------------------------------------------------------------------------------------
  #  OBSERVERS
  #---------------------------------------------------------------------------------------
  
  #  # This observer was used to help Jeff answer some questions about NEON sites that might overlap some of their cameras
  #  #   field of views.
  observe({
    input$add_custom_polyline
    print ('attempting to add the two polygons')
    add_polygon(c(-100.91624,-100.91493,-100.913622,-100.914982,-100.91624),c(46.770616,46.770637,46.77066,46.771534,46.770616), 'nogp_092', .45,  'green')
    add_polygon(c(-99.107953,-99.106625,-99.105297,-99.10663,-99.107953),c(47.162625,47.162628,47.162632,47.163524,47.162625), 'nogp_42', .45,  'green')

    add_polyline(c(-100.91624,-100.91493,-100.913622,-100.914982,-100.91624),c(46.770616,46.770637,46.77066,46.771534,46.770616), 'nogp_092b', .45,  'green')
    add_polyline(c(-99.107953,-99.106625,-99.105297,-99.10663,-99.107953),c(47.162625,47.162628,47.162632,47.163524,47.162625), 'nogp_042b', .45,  'green')
  })
  
  # Grab lon/lat values from s4 class -> matrix -> list
  observeEvent(input$map_draw_new_feature,{
    feature_type = isolate(input$map_draw_new_feature$properties$feature_type)
    # print (isolate(input$map_draw_new_feature))
    if (feature_type %in% c('polygon')){
      polygon_coordinates = isolate(input$map_draw_new_feature$geometry$coordinates[[1]])
      drawn_polygon = Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      coords = drawn_polygon@coords
      print (coords)
      lon = coords[,1]
      lat = coords[,2]
      # Run function that adds these lon/latitudes to a new tab
      add_polygon_table(isolate(input$site), lon, lat)
    }
  })
  
  add_polygon_table = function(site_,lon_,lat_){
    data$run = data$run + 1
    
    data$lats = c(data$lats, lat_[1])
    data$lons = c(data$lons, lon_[1])
    name_ = paste(c(site_,data$run), collapse='_')
    data$names = c(data$names, name_)
    data$run_log = c(data$run_log, data$run)
    
    data$df = data.frame(Name = data$names,Longitude = data$lons, Latitude = data$lats, Run = data$run_log)
    df = data$df[,c(1,4)]
    print (data$df)
    
    
    ## Create the Phenocam Datatable with basic info
    x = df
    x$Date = Sys.time() + seq_len(nrow(x))
    output$pAOIchart = renderDT(x, selection = 'none', editable = TRUE)
    
    proxy = dataTableProxy('pAOIchart')
      
    observeEvent(input$pAOIchart_cell_edit, {
      info = input$pAOIchart_cell_edit
      str(info)
      i = info$row
      j = info$col
      v = info$value
      x[i, j] <<- DT::coerceValue(v, x[i, j])
      replaceData(proxy, x, resetPaging = FALSE)  # important
    })
        
    # output$pAOIchart = renderUI({
    # #   # locations <- routeVehicleLocations()
    # #   # if (length(locations) == 0 || nrow(locations) == 0)
    # #   #   return(NULL)
    # #   # 
    #   # Create a Bootstrap-styled table
    #   tags$table(class = "table",
    #              tags$thead(tags$tr(
    #                tags$th("Name"),
    #                tags$th("Site"),
    #                tags$th("Run"),
    #                tags$th("Notes")
    #              )),
    #              tags$tbody(
    #                tags$tr(
    #                  tags$td(site_),
    #                  tags$td(site_),
    #                  tags$td('1'),
    #                  tags$td(''))
    #                )
    # #                tags$tr(
    # #                  tags$td(span(style = sprintf(
    # #                    "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
    # #                    dirColors[1]
    # #                  ))),
    # #                  tags$td("Southbound"),
    # #                  tags$td(nrow(locations[locations$Direction == "1",]))
    # #                ),
    # #                tags$tr(
    # #                  tags$td(span(style = sprintf(
    # #                    "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
    # #                    dirColors[2]
    # #                  ))),
    # #                  tags$td("Eastbound"),
    # #                  tags$td(nrow(locations[locations$Direction == "2",]))
    # #                ),
    # #                tags$tr(
    # #                  tags$td(span(style = sprintf(
    # #                    "width:1.1em; height:1.1em; background-color:%s; display:inline-block;",
    # #                    dirColors[3]
    # #                  ))),
    # #                  tags$td("Westbound"),
    # #                  tags$td(nrow(locations[locations$Direction == "3",]))
    # #                ),
    # #                tags$tr(class = "active",
    # #                        tags$td(),
    # #                        tags$td("Total"),
    # #                        tags$td(nrow(locations))
    # #                )
    #              # )
    #   )
    # })
    
  }
  
  
  
  
  
  
  # # Zoom to selected Site
  observeEvent(input$site, {
    site = isolate(input$site)
    if (counter$countervalue >0){
      print ('Running Zoom to selected site')
      site_data = zoom_to_site(site, site_names, zoom=TRUE)
    }
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
      addCircleMarkers(~lon, ~lat, label=~site, layerId=~site, labelOptions = labelOptions(noHide = F, direction = "bottom",
                        style = get_marker_style()), opacity = .80, fillColor = getColor(cams_), color = getColor(cams_),
                       radius = 10, fillOpacity = .20, weight=3)
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
  
  
  # Observer for the azm changes from 0-360 on the slider
  observe({
    azm = input$azm
    if(is.null(azm))
      return()
    isolate({
      if (input$drawROI == TRUE){
        site = input$site
        site_data = get_site_info(site, site_names)
        run_add_polyline(site_data, azm)
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
      run_add_polyline(site_data, degrees)
    }
    else if (roi_bool == FALSE){
      remove_polyline()}
  })
  
  
  # Show Popup box for site when clicked
  observe({
    event = input$map_marker_click
    
    print (event$id)
    if (is.not.null(event$id)){
      zoom_to_site(event$id, site_names, zoom=TRUE)
      updateSelectInput(session, 'site', selected = event$id )
    }

    print('Running show a popup box for Site')
    if(is.null(event))
      return()

    isolate({
      leafletProxy("map", data = cams_) %>% clearPopups()
      site = event$id

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
  
  
  # Given row from sites, create points for polyline from site.
  #   This function uses angle for field of view and los as the
  #   far distance of the FOV.
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
  
  
  # Draws a FOV as well but is different in that it uses a different
  #   method and creates a 4 point polygon using line of site near 
  #   and far and the distances across (left to right in image) for 
  #   the near and far.
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
  add_polyline = function(datalon_, datalat_, id_, opacity_, color_='red'){
    leafletProxy("map", data = cams_) %>%
      # clearShapes()%>%
      addPolylines( datalon_,
                    datalat_,
                    layerId=id_,
                    opacity=opacity_,
                    color = color_)
  }
  
  add_polygon = function(datalon_, datalat_, id_, opacity_, color_='red'){
    leafletProxy("map", data = cams_) %>%
      # clearShapes()%>%
      addPolygons( datalon_,
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
        # Can add differen't markers when we zoom in at some point, but for now we will use these circle markers from above

        addCircleMarkers(lng=lon,lat=lat,label=camera, layerId=camera, labelOptions = labelOptions(noHide = F, direction = "bottom",
                         style = get_marker_style()), opacity = .80, fillColor = getColor(cams=site_data), color = getColor(cams=site_data),
                         radius = 10, fillOpacity = .20, weight=3) %>%
        setView(lng = lon, lat = lat, zoom = 14)
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
    print('Running show a popup box for Site')
  
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
      "color" = "black",
      "font-family" = "serif",
      # "font-style" = "italic",
      "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
      "font-size" = "14px",
      "border-color" = "rgba(0,0,0,0.5)"
    )
    return(style)
  }
  
  # Creating a label from the phenocamsite name
  new_label = function(name_){
    label = sprintf('This is the test label/n%s',name_)
  }
  
  
  count = function(){
    isolate({
      counter$countervalue = counter$countervalue + 1
      print (counter$countervalue)
    })
  }
  
  
}