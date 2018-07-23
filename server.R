# Server file for Shiny App phenoRemote

server = function(input, output, session) {
  
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  REACTIVE VALUES
  #--------------------------------------------------------------------------------------------------------------------------------------
  
  variables = reactiveValues(
    filter = 'All',
    sites_df = cams_,
    sites = site_names)
  
  counter = reactiveValues(countervalue = 0)
  
  data = reactiveValues(
    run = 0,
    lons = c(),
    lats = c(),
    names = c(),
    df = data.frame(),
    headers = c('Name', 'Site', 'Run', 'Notes'))
  
  # Temporary holding space for lat/longs
  latlongs = reactiveValues(df2 = data.frame(Longitude = numeric(0), Latitude = numeric(0)))
  # Empty reactive spdf
  value = reactiveValues(drawnPoly = SpatialPolygonsDataFrame(SpatialPolygons(list()), 
                                                              data=data.frame(notes=character(0), stringsAsFactors = F)))
  
  
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  OUTPUTS
  #--------------------------------------------------------------------------------------------------------------------------------------

  
  ## Create the Phenocam Datatable with basic info (Tab named phenocam Table)
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
    leaflet('map', data = variables$sites_df, options= leafletOptions(zoomControl=FALSE)) %>%
      addTiles() %>%
      addProviderTiles('Esri.WorldTopoMap')%>%
      addDrawToolbar(
        targetGroup = 'drawnPoly',
        polylineOptions=FALSE,
        rectangleOptions = FALSE,
        markerOptions = FALSE,
        circleMarkerOptions = FALSE,
        circleOptions = FALSE,
        editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polygonOptions = drawPolygonOptions(
          showArea = TRUE,
          repeatMode = F,
          shapeOptions = drawShapeOptions(clickable=TRUE,
                                          color='black',
                                          fillColor='yellow')))  %>%
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
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addStyleEditor()
    })
  
  # Adds the mouse lat / lon to an output (we can change this to anything)
  output$mouse <- renderText({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      paste0("Lat: ", input$hover_coordinates[1], 
             "\nLng: ", input$hover_coordinates[2])
    }
  })
  

  
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  OBSERVERS
  #--------------------------------------------------------------------------------------------------------------------------------------
  
  
  # Event occurs when drawing a new feature starts
  observeEvent(input$map_draw_new_feature, {
      polygon_coordinates = input$map_draw_new_feature$geometry$coordinates[[1]]
      drawn_polygon = Polygon(do.call(rbind,lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])})))
      # coords = drawn_polygon@coords
      # print ('observerEvent map_draw_new_feature')
      # print (coords)
      
      data$run = data$run + 1                                        # PolygonCounter for unique name creation
      name_ = paste(c(isolate(input$site),data$run), collapse='_')   # Creationg Unique Identifier data for polygon using run+site
      
      coor = unlist(input$map_draw_new_feature$geometry$coordinates)
      
      Longitude = coor[seq(1, length(coor), 2)] 
      Latitude = coor[seq(2, length(coor), 2)]
      
      isolate(latlongs$df2<-rbind(latlongs$df2, cbind(Longitude, Latitude)))
      
      poly = Polygon(cbind(latlongs$df2$Longitude, latlongs$df2$Latitude))
      polys = Polygons(list(poly), ID = name_)
      spPolys = SpatialPolygons(list(polys))
      
      value$drawnPoly = rbind(value$drawnPoly,
                             SpatialPolygonsDataFrame(spPolys, data = data.frame(notes = NA,
                                                               row.names = row.names(spPolys))))
      c = 0
      sites = c()
      reset_polygon_values()
      for (x in value$drawnPoly@polygons){
        c = c + 1
        lon = value$drawnPoly@polygons[[c]]@Polygons[[1]]@coords[,1]
        lat = value$drawnPoly@polygons[[c]]@Polygons[[1]]@coords[,2]
        site = tail(value$drawnPoly@polygons[[c]]@ID, n=1)
        data$lats = c(data$lats, lat)
        data$lons = c(data$lons, lon)

        for (x in lon){
          sites = c(sites, site)
        }
      }
      data$names = c(data$names, sites)
      updateSelectInput(session, 'shapefiles', choices = unique(data$names))
      add_polygon_table()

      observeEvent(input$map_draw_stop, {
        print ('stop_draw')
        ###  USE THIS CODE BELOW TO ADD OPTION TO SHOW WHAT EACH SHAPEFILE IS NAMED.
        ###     SHOW/HIDE the SHAPEFILE (BECAUSE THIS CREATES A DUPLICATE)
        
        # leafletProxy('map') %>%  
        #   removeDrawToolbar(clearFeatures=TRUE) %>%
        #   removeShape('temp') %>%
        #   clearGroup('drawnPoly') %>%
        #   addPolygons(data=value$drawnPoly, popup=row.names(value$drawnPoly),   group='drawnPoly', color="red", layerId= row.names(value$drawnPoly)) %>%
        #   addDrawToolbar(targetGroup = "drawnPoly",
        #                  rectangleOptions = F,
        #                  polylineOptions = F,
        #                  markerOptions = F,
        #                  editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        #                  circleOptions=F,
        #                  polygonOptions=drawPolygonOptions(showArea=TRUE, repeatMode=F  , shapeOptions=drawShapeOptions( fillColor="red",clickable = TRUE)))
        
      })
      latlongs$df2 <- data.frame(Longitude = numeric(0), Latitude = numeric(0))   #clear df
  })
  
  
  # When edited feature gets saved
  observeEvent(input$map_draw_edited_features, {
    print ('map_draw_edited_features')
  })
  
  
  # When deleted feature gets deleted
  observeEvent(input$map_draw_deleted_features, {
    print ('map_draw_deleted_features')

    
  })
  
  # Save shapefile button
  observeEvent(input$saveshp,{
    WGScoor = data$df
    coordinates(WGScoor) = ~Longitude + Latitude
    proj4string(WGScoor) = CRS("+proj=longlat +datum=WGS84")
    LLcoor<-spTransform(WGScoor,CRS("+proj=longlat"))
    file = isolate(input$shapefiles)
    folder = get_download_folder()
    filename = paste(folder, file, sep='')
    print (filename)

    shapefile(LLcoor, filename, overwrite=TRUE)

  })
  

  # SELECT INPUT
  # Filter based on Filter Sites dropdown
  observeEvent(input$filterSites, {
    variables$filter = input$filterSites
    print ('Running Filter Sites')
    if ('All' %in% variables$filter){
      variables$sites_df = cams_
    }else{
      if ('Active' %in% variables$filter){
        variables$sites_df = subset(cams_, active == 'True')
      }
      if ('Inactive' %in% variables$filter){
        variables$sites_df = subset(cams_, active == 'False')
      }
      if ('Type1' %in% variables$filter){
        variables$sites_df = subset(cams_, site_type == 'I')
      }
      if ('Type2' %in% variables$filter){
        variables$sites_df = subset(cams_, site_type == 'II')
      }
      if ('Type3' %in% variables$filter){
        variables$sites_df = subset(cams_, site_type == 'III')
      }
      if ('NEON' %in% variables$filter){
        variables$sites_df = subset(cams_, group == 'NEON')
      }
    }
    variables$sites = variables$sites_df$site
    updateSelectInput(session, 'site', choices = variables$sites)
    show_all_sites()
  })
  
  
  # BUTTON
  # Zooms to the selected site in the Sites dropdown option with BUTTON
  observeEvent(input$siteZoom, {
    print('Running BUTTON Zoom to Selected Site')
    site = isolate(input$site)
    site_data = zoom_to_site(site, site_names, zoom=TRUE)
    
    
  })
  # BUTTON
  # Zoom to contiguous US
  observeEvent(input$usZoom, {
      print('Running Zoom to contiguous US')
      leafletProxy("map", data = variables$sites_df) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  
  # Add All sites back to map
  observeEvent(input$showSites, {
    showAll = input$showSites
    # variables$sites_df = cams_
    print('Running add All sites back to map')
    show_all_sites()
    # leafletProxy("map", data = variables$sites_df) %>%
    #   addCircleMarkers(~lon, ~lat, label=~site, layerId=~site, labelOptions = labelOptions(noHide = F, direction = "bottom",
    #                     style = get_marker_style()), opacity = .80, fillColor = getColor(variables$sites_df), color = getColor(variables$sites_df),
    #                    radius = 10, fillOpacity = .20, weight=3.5)
    count()
  })
  
  
  # Change Map Layer 1
  observeEvent(input$layer, {
    layers = input$layer
    print('Running Change Map Layer 1')
    leafletProxy("map", data = variables$sites_df) %>%
      clearTiles() %>%
      addProviderTiles(layers)
  })
  
  
  # Observer for the azm changes from 0-360 on the slider
  observeEvent(input$azm, {
    azm = input$azm
    if(is.null(azm))
      return()
    isolate({
      if (input$drawROI == TRUE){
        site = input$site
        site_data = get_site_info(site)
        run_add_polyline(site_data, azm)
      }
    })
  })
  
  
  # Draws roi polyline for a site location
  observeEvent(input$drawROI, {
    roi_bool = input$drawROI
    if (roi_bool == TRUE){
      site = isolate(input$site)
      site_data = get_site_info(site)
      cam_orientation = as.character(site_data$camera_orientation)
      degrees = as.numeric(orientation_key[cam_orientation])
      run_add_polyline(site_data, degrees)
    }
    else if (roi_bool == FALSE){
      remove_polyline()}
  })
  
  
  # Show Popup box for site when clicked
  observeEvent(input$map_marker_click, {
    event = isolate(input$map_marker_click)
    print (event$id)
    
    if (is.not.null(event$id)){
      if (isolate(input$map_zoom) < 5){
        zoom_to_site(event$id, site_names, zoom=TRUE)
      }
      updateSelectInput(session, 'site', selected = event$id )
    }

    if(is.null(event))
      return()

    isolate({
      leafletProxy("map", data = variables$sites_df) %>% clearPopups()
      site = event$id

      site_data = get_site_info(site)

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
  
  
  # CheckBox to Show image for site from dropdown
  observe({
    print ('Doing something to Image')
    draw_bool = input$drawImage
    site = input$site
    roi_bool = input$drawImageROI
    
    removeUI(selector = '#phenocamSiteImage')
    img_url = get_img_url(site)
    
    if (draw_bool == TRUE){
      insertUI(selector = '#image',
        ui = tags$div(id='phenocamSiteImage',
                      tags$img(src=img_url, class= 'img',
                               style='position: absolute; z-index: 1; top:0px; left:0px;')))}
      if (roi_bool == TRUE){
          roi_url = get_roi_url(site)
          print (roi_url)
          if (roi_url != 'Not Found'){
          insertUI(selector = '#phenocamSiteImage',
            ui =  tags$img(src=roi_url,
                       class= 'roi', style='position: absolute; z-index: 2; top:0; left:0px;'))}
      
    }else if (draw_bool == FALSE){
      removeUI(selector = '#phenocamSiteImage')
    }
  })

  
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  FUNCTIONS
  #--------------------------------------------------------------------------------------------------------------------------------------
  
  
  # Returns Downloads folder for windows/macos
  get_download_folder = function(){
    if (Sys.info()['sysname'] == 'Darwin'){
      folder = paste('/Users/', Sys.getenv('LOGNAME'),'/Downloads/', sep = '')
    }else if (Sys.info()['sysname'] == 'Windows'){
      folder = paste('C:/Downloads/', sep = '')
    }else{
      folder = ''
    }
    return (folder)
  }
  
  
  # Grabs url for the primary ROI
  get_roi_url = function(name){
    roi_url = tryCatch({
    sitename = 'harvardgarden'
    baseurl = 'https://phenocam.sr.unh.edu'
    siteurl = paste('https://phenocam.sr.unh.edu/webcam/sites/',name,'/', sep = '')
    page = read_html(siteurl)
    html = page %>% html_nodes("a") %>% html_attr('href')
    html = grep('data/archive', html, value=TRUE)[[1]]
    html = paste(baseurl, html, sep='')
    
    page2 = read_html(html)
    html2 = page2 %>% html_nodes("a") %>% html_attr('href')
    html2 = grep('data/archive', html2, value=TRUE)
    html2 = grep('.tif', html2, value=TRUE)
    
    roi = strsplit(html2, '/')[[1]]
    roi = grep('.tif', roi, value=TRUE)
    roi = strsplit(roi, name)[[1]]
    roi = grep('.tif', roi, value=TRUE)
    roi = strsplit(roi, '.tif')[[1]]
    roi_url = paste('https://phenocam.sr.unh.edu/data/archive/', 
                    name, '/ROI/', name, roi, '_overlay.png', sep = '') 
    return (roi_url)
    },error=function(cond) {message(paste('failed to get roi for sitename:'),isolate(input$site))
                            return('Not Found')})
    return (roi_url)
  }
  
  # Grabs img url from sitename
  get_img_url = function(name){
    url = paste("https://phenocam.sr.unh.edu/data/latest/", name, ".jpg",sep = '')
    return (url)
  }
  
  # add all of these sites back to the leaflet map
  show_all_sites = function(){
    leafletProxy("map", data = variables$sites_df) %>%
      clearMarkers() %>%
      addCircleMarkers(~lon, ~lat, label=~site, layerId=~site, labelOptions = labelOptions(noHide = F, direction = "bottom",
                       style = get_marker_style()), opacity = .80, fillColor = getColor(variables$sites_df), color = getColor(variables$sites_df),
                       radius = 10, fillOpacity = .20, weight=3.5)
  }
  
  # Radians to degrees
  rad2deg = function(rad) {(rad * 180) / (pi)}
  
  
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

  
  # Add a polyline layer to the map
  add_polyline = function(datalon_, datalat_, id_, opacity_, color_='red'){
    leafletProxy("map", data = variables$sites_df) %>%
      # clearShapes()%>%
      addPolylines( datalon_,
                    datalat_,
                    layerId=id_,
                    opacity=opacity_,
                    color = color_)
  }
  
  
  add_polygon = function(datalon_, datalat_, id_, opacity_, color_='red'){
    leafletProxy("map", data = variables$sites_df) %>%
      # clearShapes()%>%
      addPolygons( datalon_,
                    datalat_,
                    layerId=id_,
                    opacity=opacity_,
                    color = color_)
  }
  
  
  # remove all polylines
  remove_polyline = function(id_){
    leafletProxy("map", data = variables$sites_df) %>%
      clearShapes()
  }
  
  
  # Zoom to site
  zoom_to_site = function(site_, site_names_, zoom){
    site_data = get_site_info(site_)
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
      leafletProxy('map', data = variables$sites_df) %>%
        clearPopups() %>%
        clearMarkers() %>%
        clearShapes() %>%
        # Can add differen't markers when we zoom in at some point, but for now we will use these circle markers from above
        addCircleMarkers(lng=lon,lat=lat,label=camera, layerId=camera, labelOptions = labelOptions(noHide = F, direction = "bottom",
                         style = get_marker_style()), opacity = .80, fillColor = getColor(cams=site_data), color = getColor(cams=site_data),
                         radius = 10, fillOpacity = .20, weight=3.5) %>%
        setView(lng = lon, lat = lat, zoom = 14)
    }
    if (drawROI){
      run_add_polyline(site_data, degrees)
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
    myurl = paste("https://phenocam.sr.unh.edu/data/latest/", isolate(input$site), '.jpg', sep = '')
  
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
                 
                 '<a type="submit" href=', website ,' class="button">Go to Phenocam website</a>'
                 )
    
    
    leafletProxy("map",data = variables$sites_df) %>% addPopups(lng_, lat_, popup = pop, layerId = camera_)
  }
  
  
  # Get specific site data and returns lon/lat/camera/description/elevation
  get_site_info = function(site_name){
    site_data = subset(cams_, site == site_name)
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
  
  
  reset_polygon_values = function(){
    data$lats = c()
    data$lons = c()
    data$names = c()
  }
  
  
  add_polygon_table = function(){
    # Building dataframe from above reactive variables
    print ('trying to build df')
    data$df = data.frame(Name = data$names,Longitude = data$lons, Latitude = data$lats)
    # Slicing df to just display specific Columns
    print ('trying to aggregate df')
    df = aggregate(data$df[,c(2,3)], list(data$df$Name), max)
    ## Create the pAOI datatable
    ##   --This can potentially be editable
    print ('about to set up the chart with df')
    x = df
    # x$Date = Sys.time() + seq_len(nrow(x))
    output$pAOIchart = renderDT(x, selection = 'none', editable = TRUE)
    
    # Name of output table
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
  }
  
  
}
