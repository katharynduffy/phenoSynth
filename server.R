# Server file for Shiny App phenoRemote

server = function(input, output, session) {

  #--------------------------------------------------------------------------------------------------------------------------------------
  #  REACTIVE VALUES
  #--------------------------------------------------------------------------------------------------------------------------------------

  variables = reactiveValues(
                      filter   = 'All',
                      sites_df = cams_,
                      sites    = site_names)
  appeears = reactiveValues(
                      none = ''
  )

  # 'analyzer' or 'explorer'
  panel   = reactiveValues(mode = '')

  counter = reactiveValues(countervalue = 0)

  modis   = reactiveValues(data = data.frame(),
                         cached_ndvi = list())
  

  data    = reactiveValues(
                      draw_mode = FALSE,
                      # Colors from GIMP (prefer) - colors for pft classifications
                      c2    = c('#1b8a28', '#36d03e', '#9ecb30', '#a0f79f', '#91bb88', '#b99091', '#f0dfb8', '#d6ed9a',
                                 '#f1dc07', '#ecbb5b', '#4981b1', '#fcee72', '#fd0608', '#9b9353', '#bdbec0', '#89cae3'),
                      # Colors from MODIS / NASA (daac)
                      # c2    = c('#008000', '#00ff00', '#99cc00', '#99ff99', '#339966', '#993366', '#ffcc99', '#ccffcc',
                      #           "#ffcc00", "#ff9900", '#006699', '#ffff00', '#ff0000', '#999966', '#808080', '#000080'),
                      run   = 0,
                      names = c(),
                      df    = data.frame(),
                      veg_types = c())

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
    # str(info)
    i   = info$row
    j   = info$col
    v   = info$value
    x[i, j] <<- DT::coerceValue(v, x[i, j])
    replaceData(proxy, x, resetPaging = FALSE)  # important
  })

  ## Create the map
  output$map = renderLeaflet({
    leaflet('map', data = variables$sites_df, options= leafletOptions(zoomControl=FALSE, doubleClickZoom = FALSE)) %>%
      addTiles(
        "http://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}.jpg",
        attribution = 'Tiles &copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS, AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP, and the GIS User Community',
        group = "World Imagery"
      ) %>%
      # addWMSTiles(
      #   "http://webmap.ornl.gov/ogcbroker/wms?",
      #   layers      = "10004_31",
      #   options     = WMSTileOptions(format = "image/png", transparent = TRUE, opacity=.6),
      #   attribution = "MODIS Land Cover (MCD12Q1) &copy NASA",
      #   group       = "MODIS Land Cover"
      # ) %>%
      addProviderTiles(
        "OpenTopoMap",
        group   = "Open Topo Map",
        options = providerTileOptions(transparent=FALSE)
      ) %>%
      hideGroup("MODIS Land Cover") %>%
      addDrawToolbar(
        targetGroup         = 'drawnPoly',
        polylineOptions     = FALSE,
        rectangleOptions    = FALSE,
        markerOptions       = FALSE,
        # circleMarkerOptions = FALSE,
        circleOptions       = FALSE,
        editOptions         = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polygonOptions      = drawPolygonOptions(
                                              showArea     = TRUE,
                                              repeatMode   = F,
                                              shapeOptions = drawShapeOptions(
                                                              clickable = TRUE,
                                                              color     = 'black',
                                                              fillColor = 'blue')))  %>%
      # Rendering the mouseoutput (aka lat / lon)
      onRender("function(el,x){
               this.on('mousemove', function(e) {
               var lat   = e.latlng.lat;
               var lng   = e.latlng.lng;
               var coord = [lat, lng];
               Shiny.onInputChange('hover_coordinates', coord)});
               this.on('mouseout', function(e) {
               Shiny.onInputChange('hover_coordinates', null)})
  }")  %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4) %>%
      addStyleEditor() %>%
      addMeasure(position          = "topleft",
                 primaryLengthUnit = "meters",
                 primaryAreaUnit   = "sqmeters",
                 activeColor       = "#3D535D",
                 completedColor    = "#7D4479") %>%
      # Adds the layers options to top left of Map
      addLayersControl(
        baseGroups    = c("World Imagery", "Open Topo Map"),
        position      = c("topleft"),
        options       = layersControlOptions(collapsed = TRUE))
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

  
  #initiating with observer
  observe({
    switch_to_explorer_panel()
    panel$mode = 'explorer'
    data$pixel_df = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Id", "Site", "Lat", 'Lon', 'pft'))
    data$df = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('Name', 'Longitude', 'Latitude', 'LeafletId'))
  })

  #--------------------------------------------------------------------------------------------------------------------------------------
  #  OBSERVERS
  #--------------------------------------------------------------------------------------------------------------------------------------

  
  # # Adds Legend for MODIS Land Cover
  # observeEvent(input$map_groups,{
  #   map_layers = input$map_groups
  #   if ('MODIS Land Cover' %in% map_layers){
  #     shinyjs::show(id = 'modisLegend')
  #     insertUI(selector = '#image2',
  #              ui = tags$div(id='modisLegend_',
  #                            tags$img(src   = 'igbp-legend.png', class= 'img',
  #                                     style = "position: absolute; z-index: 3; top:0px; left:0px;")))
  #   }else{shinyjs::hide(id = 'modisLegend')}
  # })
  
  # Start of Drawing - set highlight pixel to off
  observeEvent(input$map_draw_start, {
    data$draw_mode = TRUE
    print ('Starting Draw Mode')
  })

  # Event occurs when drawing a new feature starts
  observeEvent(input$map_draw_new_feature, {
      # Leaflet ID to add to the shapefile dataframe
      id = input$map_draw_new_feature$properties$`_leaflet_id`

      # Site name combined with run # for new polygon feature
      data$run   = data$run + 1
      name_      = paste(c(isolate(input$site),data$run), collapse='_')
      data$names = c(data$names, name_)

      # Grabbing lat/lon values for new leaflet polygon
      coor      = unlist(input$map_draw_new_feature$geometry$coordinates)
      Longitude = coor[seq(1, length(coor), 2)]
      Latitude  = coor[seq(2, length(coor), 2)]

      # Building Dataframe with points from newly created leaflet feature
      c = 0
      for (x in Longitude){
        c       = c + 1
        data$df = rbind(data$df, data.frame(Name = name_, Longitude = x, Latitude = Latitude[c], LeafletId = id))}

      # Creating a SpatialPolygon that can be added to our spatial polygons dataframe (value$drawnPoly)
      poly    = Polygon(cbind(Longitude, Latitude))
      polys   = Polygons(list(poly), ID = name_)
      spPolys = SpatialPolygons(list(polys))

      # Adding new polygon to a spatial polygons dataframe
      value$drawnPoly = rbind(value$drawnPoly,
                             SpatialPolygonsDataFrame(spPolys, data = data.frame(notes = NA,
                                                               row.names = row.names(spPolys))))

      # Updating the select input for the download availability of created leaflet features
      updateSelectInput(session, 'shapefiles', choices = unique(data$df$Name))

      # Building the polygon table from the data$df dataframe containing all of the leaflet polygon data
      build_polygon_table()

      # print (data$df)
      # sets highlight pixel to on
      data$draw_mode = FALSE
      print ('Exiting Draw Mode')
      
      print (input$map_draw_stop)
      
  })


  # When edited feature gets saved
  observeEvent(input$map_draw_edited_features, {
    # Leaflet ID to edit
    id = input$map_draw_edited_features$features[[1]]$properties$`_leaflet_id`

    # Grabbing lat/lon values for new leaflet polygon
    coor      = unlist(input$map_draw_edited_features$features[[1]]$geometry$coordinates[[1]])
    Longitude = coor[seq(1, length(coor), 2)]
    Latitude  = coor[seq(2, length(coor), 2)]
    name_     = unique(subset(data$df, LeafletId == id)$Name)

    # Deletes all rows with id being edited
    data$df = subset(data$df, LeafletId != id)

    # Adds back the edited polygon to the dataframe (data$df)
    c = 0
    for (x in Longitude){
      c       = c + 1
      data$df = rbind(data$df, data.frame(Name = name_, Longitude = x, Latitude = Latitude[c], LeafletId = id))}

    # Updating the polygon table from the data$df dataframe containing all of the leaflet polygon data
    build_polygon_table()

    print (data$df)
  })


  # When feature is deleted
  observeEvent(input$map_draw_deleted_features, {
    # Leaflet ID to delete
    id      = input$map_draw_deleted_features$features[[1]]$properties$`_leaflet_id`

    # Deletes all rows with id being edited
    print (data$df)
    data$df = subset(data$df, LeafletId != id)

    # Updating the select input for the download availability of created leaflet features
    updateSelectInput(session, 'shapefiles', choices = unique(data$df$Name))

    # Updating the polygon table from the data$df dataframe containing all of the leaflet polygon data
    build_polygon_table()


    # print (data$df)
  })

  # Save shapefile button
  observeEvent(input$saveshp,{
    WGScoor              = data$df
    coordinates(WGScoor) = ~Longitude + Latitude
    proj4string(WGScoor) = CRS("+proj=longlat +datum=WGS84")
    LLcoor               <- spTransform(WGScoor,CRS("+proj=longlat"))
    file                 = isolate(input$shapefiles)
    folder               = get_download_folder()
    filename             = paste(folder, file, sep='')
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
    site      = isolate(input$site)
    site_data = zoom_to_site(site, zoom=TRUE)


  })
  # BUTTON
  # Zoom to contiguous US
  observe ({
      input$usZoom
      input$siteExplorerMode
      print('Running Zoom to contiguous US')
      leafletProxy("map", data = variables$sites_df) %>%
        setView(lng = -93.85, lat = 37.45, zoom = 4)
      showAll = isolate(input$showSites)
      print('Running add All sites back to map')
      show_all_sites()
      count()
  })


  # Add All sites back to map
  observeEvent(input$showSites, {
    showAll = input$showSites
    # variables$sites_df = cams_
    print('Running add All sites back to map')
    show_all_sites()
    count()
  })


  # # Change Map Layer 1
  # observeEvent(input$layer, {
  #   layers = input$layer
  #   print('Running Change Map Layer 1')
  #   leafletProxy("map", data = variables$sites_df) %>%
  #     clearTiles() %>%
  #     addProviderTiles(layers)
  # })


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


  # Draws fov polyline for a site location
  observeEvent(input$drawROI, {
    roi_bool = input$drawROI
    if (roi_bool == TRUE){
      site            = isolate(input$site)
      site_data       = get_site_info(site)
      cam_orientation = as.character(site_data$camera_orientation)
      degrees         = as.numeric(orientation_key[cam_orientation])
      run_add_polyline(site_data, degrees)
      shinyjs::show(id = 'azm')
      updateSliderInput(session, 'azm', value = degrees )
    }
    else if (roi_bool == FALSE){
      shinyjs::hide(id = 'azm')
      remove_polyline()}
  })


  # Show Popup box for site when clicked
  observeEvent(input$map_marker_click, {
    event = isolate(input$map_marker_click)
    print (event$id)

    if (is.not.null(event$id)){
      if (isolate(input$map_zoom) < 5){
        zoom_to_site(event$id, zoom=TRUE)
      }
      updateSelectInput(session, 'site', selected = event$id )
    }

    if(is.null(event))
      return()

    isolate({
      leafletProxy("map", data = variables$sites_df) %>% clearPopups()
      site = event$id

      site_data = get_site_info(site)

      lat             = site_data$lat
      lon             = site_data$lon
      description     = site_data$site_description
      elevation       = site_data$elev
      camera          = site_data$site
      site_type       = site_data$site_type
      nimage          = site_data$nimage
      cam_orientation = as.character(site_data$camera_orientation)
      degrees         = as.numeric(orientation_key[cam_orientation])
      active          = site_data$active
      date_end        = site_data$date_end
      date_start      = site_data$date_start

      get_site_popup(camera, lat, lon, description, elevation, site_type, cam_orientation, degrees , nimage,
                     active, date_end, date_start)
    })
  })

  # Site Explorer Mode Images
  # CheckBox to Show image for site from dropdown
  observe({
    print ('Doing something to Image')
    draw_bool = input$drawImage
    site      = input$site
    roi_bool  = input$drawImageROI

    removeUI(selector = '#phenocamSiteImage')
    img_url = get_img_url(site)

    if (draw_bool == TRUE){
      shinyjs::show(id = 'currentImage')
      insertUI(selector = '#image',
        ui = tags$div(id='phenocamSiteImage',
                      tags$img(src=img_url, class= 'img',
                               style="position: absolute; z-index: 1; top:0px; left:0px;")))}
      if (roi_bool == TRUE){
          roi_url = get_roi_url(site)
          print (roi_url)
          if (roi_url != 'Not Found'){
          insertUI(selector = '#phenocamSiteImage',
            ui =  tags$img(src=roi_url,
                       class= 'roi', style='position: absolute; z-index: 2; top:0px; left:0px;'))}

    }else if (draw_bool == FALSE){
      removeUI(selector = '#phenocamSiteImage')
      shinyjs::hide(id = 'currentImage')
    }
  })

  # Plots the modis subset
  observeEvent(input$showModisSubset,{
    # Run modis tool here on site currently selected
    print ('Running show MODIS subset tool')
    site      = input$site
    site_data = get_site_info(site)
    site_     = site_data$site[1]
    
    print (modis$cached_ndvi)
    
    if (site_ %in% modis$cached_ndvi){
      output$currentPlot <- renderPlot({ modis$cached_ndvi[site_] })
    } else{
      
      lat_       = site_data$lat[1]
      lon_       = site_data$lon[1]
      date_end   = as.Date(site_data$date_end[1])
      date_start = as.Date(site_data$date_start[1])
      subset     <- mt_subset(product = "MOD13Q1",lat = lat_,lon = lon_,band = "250m_16_days_NDVI",
                              start = date_start,end = date_end,km_lr = 1,km_ab = 1,site_name = site_,internal = TRUE)
      print (str(subset))
      
      NDVIsubset <- mt_subset(product   = "MOD13Q1",
                              lat       = lat_,
                              lon       = lon_,
                              band      = "250m_16_days_NDVI",
                              start     = date_start,
                              end       = date_end,
                              km_lr     = 1,
                              km_ab     = 1,
                              site_name = site_,
                              internal  = TRUE)
      
      QCsubset   <- mt_subset(product   = "MOD13Q1",
                              lat       = lat_,
                              lon       =   lon_,
                              band      = "250m_16_days_pixel_reliability",
                              start     = date_start,
                              end       = date_end,
                              km_lr     = 1,
                              km_ab     = 1,
                              site_name = site_,
                              internal  = TRUE)
      
      cleanNDVI      = data.frame(NDVI=NDVIsubset$data$data, QC=QCsubset$data$data, Date=as.Date.factor(QCsubset$data$calendar_date))
      cleanNDVI      = cleanNDVI%>%filter(QC<1)
      cleanNDVI$NDVI = cleanNDVI$NDVI*.0001
      
      
      p = ggplot(data = cleanNDVI, aes(x= Date, y= NDVI)) +
        geom_point() +
        scale_x_date(date_breaks = "1 month", date_minor_breaks = "1 week", date_labels = "%Y %B") +
        theme(axis.text.x = element_text(angle = 45, hjust =1))
      p  + ggthemes::theme_few()  
      
      #plot p here, where that goes in the UI we don't know yet
      modis$data = cleanNDVI
      shinyjs::show(id = 'plotpanel')
      shinyjs::show(id = 'showHidePlot')
      output$currentPlot <- renderPlot({ p })
      modis$cached_ndvi[[site_]] = p
      print (modis$cached_ndvi[site_])
      print (p)
    }
    
  })


  # Button switches to Analyzer Mode
  observeEvent(input$analyzerMode,{
    panel$mode = 'analyzer'
    site       = input$site
    site_data  = get_site_info(site)
    veg_types  = c()
    print ('Switching to Analyze Mode')
    zoom_to_site(site, TRUE)
    
    output$analyzerTitle = renderText({paste0('Site:: ', site)})
    switch_to_analyzer_panel()
    
    print (sprintf('grabbing task row from appeears for site: %s', site))
    appeears$ndvi = get_appeears_task(site)
    
    veg_idx    = is.element(roi_files$site, site)
    veg_match     = roi_files[veg_idx,]
    
    print (veg_match)
    
    if (nrow(veg_match) == 0){
      updateSelectInput(session, 'pftSelection', choices = 'No ROI Vegetation Available')
    }else{
      veg_types = c()
      for (i in c(1:nrow(veg_match))){
        print (i)
        veg.idx   = is.element(pft_df$pft_abbreviated, veg_match$roitype[i])
        veg       = pft_df$pft_expanded[veg.idx]
        add_veg   = as.character(veg[1])
        veg_types = c(veg_types, paste0(add_veg, '_', i))
      }
      data$veg_types = veg_types
      print (data$veg_types)
      updateSelectInput(session, 'pftSelection', choices = veg_types)
    }
  })
  
  # When ROI Vegetation type changes replot highlighted veg type
  observeEvent(input$pftSelection, {
    if (panel$mode == 'analyzer'){
        print ('Running pft Selection')
        
        site       = input$site
        site_data  = get_site_info(site)
        pft = input$pftSelection
    
        r      = crop_MODIS_2016_raster(site_data$lat, site_data$lon, reclassify=FALSE)
        c3 = build_pft_palette(r)
        print(c3$colors)
        print(c3$names)
        
        data$r = r
        print (pft)
        
        pft = strsplit(pft, '_')[[1]][1]
        print (pft)
        pft_key = (subset(pft_df, pft_df$pft_expanded == pft)$pft_key)
        print (pft_key)
        
        rc   = crop_MODIS_2016_raster(site_data$lat, site_data$lon, reclassify=TRUE, primary = as.numeric(pft_key))

        leafletProxy('map') %>%
          clearControls() %>%
          clearImages() %>%
          addRasterImage(data$r, opacity = .65, project=TRUE, group='MODIS Land Cover 2016', colors = c3$colors) %>%
          addRasterImage(rc, opacity = .55, project=TRUE, group= 'MODIS Reclassified 2016', colors= 'green') %>%
          addLegend(labels = c3$names, colors = c3$colors, position = "bottomleft") %>%
          addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
                           overlayGroups = c('MODIS Land Cover 2016', 'MODIS Reclassified 2016'),
                           position = c("topleft"),
                           options = layersControlOptions(collapsed = FALSE)) %>%
          hideGroup('MODIS Land Cover 2016')
    }
  })

  
  #Button switches to Site explorer mode
  observeEvent(input$siteExplorerMode,{
    print ('Switching to Explorer Mode')
    panel$mode = 'explorer'
    switch_to_explorer_panel()
  })

  # Button that plots GCC
  observeEvent(input$plotPhenocamGCC, {
    print ('Plotting GCC')
    get_site_roi_3day_csvs(input$site)
  })

  # Button that hides GCC Plot
  observeEvent(input$hidePlot, {
    print ('Hiding Plot')
    shinyjs::hide(id = 'plotpanel')
    shinyjs::hide(id = 'showHidePlot')
  })
  
  # Minimize button in image panel
  observeEvent(input$showImage, {
    shinyjs::hide(id = 'currentImage')
    updateCheckboxInput(session, inputId = 'drawImage', value=FALSE)
    updateCheckboxInput(session, inputId = 'drawImageROI', value=FALSE)
  })
  
  # Overlay button for ROI in image panel
  observeEvent(input$showROIimage, {
    updateCheckboxInput(session, inputId = 'drawImage', value=TRUE)
    if (input$drawImageROI == FALSE){
      updateCheckboxInput(session, inputId = 'drawImageROI', value=TRUE)
      }else{updateCheckboxInput(session, inputId = 'drawImageROI', value=FALSE)}
  })

  # Click on map to show popup of lat/lon
  observeEvent(input$map_click,{
        if (input$highlightPixelMode == TRUE){
          click = input$map_click
          site  = input$site
          if (panel$mode == 'analyzer'){
            if (!is.null(click)) {
              lon_ = click$lng
              lat_ = click$lat
              showpos(x = lon_ , y = lat_, site)
            }
          }
    }
  })
  
  # Overlay button for ROI in image panel (AppEEARS)
  observeEvent(input$getAPPEEARSpoints, {
    # load in netcdf for NDVI layer
    #------------------------------------------------------------------------
    file_ndvi    = download_bundle_file(appeears$ndvi$task_id, 'nc')
    file_ndvi_qa = download_bundle_file(appeears$ndvi$task_id, 'qa_csv')
    ndvi_output  = nc_open(file_ndvi)
    v6_QA_lut      = read.csv(file_ndvi_qa)
    delete_file(file_ndvi)
    delete_file(file_ndvi_qa)
    
    # netcdf manipulation
    #------------------------------------------------------------------------
    v6_NDVI = ncvar_get(ndvi_output, "_250m_16_days_NDVI")
    v6_QA   = ncvar_get(ndvi_output, "_250m_16_days_VI_Quality")
    
    # Set lat and lon arrays for NDVI data
    lat_NDVI = ncvar_get(ndvi_output, "lat")
    lon_NDVI = ncvar_get(ndvi_output, "lon")
    
    # Grab the fill value and set to NA
    fillvalue = ncatt_get(ndvi_output, "_250m_16_days_NDVI", "_FillValue")
    v6_NDVI[v6_NDVI == fillvalue$value] = NA
    
    # Define the coordinate referense system proj.4 string
    crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs+ towgs84=0,0,0")
    # crs = CRS("+proj=longlat +datum=WGS84")
    
    # Grab first observation of NDVI and Quality datasets
    v6_NDVI = raster(t(v6_NDVI[,,1]), xmn=min(lon_NDVI), xmx=max(lon_NDVI), ymn=min(lat_NDVI), ymx=max(lat_NDVI), crs=crs)
    v6_NDVI_original = v6_NDVI
    v6_QA = raster(t(v6_QA[,,1]), xmn=min(lon_NDVI), xmx=max(lon_NDVI), ymn=min(lat_NDVI), ymx=max(lat_NDVI), crs=crs)
    #------------------------------------------------------------------------
    YlGn = brewer.pal(9, "YlGn")
    leafletProxy('map') %>% addRasterImage(v6_NDVI_original, opacity = .7, group = 'test1', col = YlGn)
    
  })
  
  #--------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  FUNCTIONS
  #--------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------------------------

  # Creates a reprojection of a lat/lon WGS84 point into sinusoidal Modis projection
  get_x_y_sinu_from_wgs_pt = function(lon_,lat_){
    xy              = data.frame(matrix(c(lon_,lat_), ncol=2))
    colnames(xy)    = c('lon', 'lat')
    coordinates(xy) = ~ lon + lat
    proj4string(xy) = CRS("+proj=longlat +datum=WGS84")
    p               = spTransform(xy, CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"))
    return (p)
  }
  
  
  # Creates a reprojection of a lat/lon WGS84 point into sinusoidal Modis projection
  get_lat_lon_wgs_from_sinu_pt = function(lon_,lat_){
    print ('Reprojecting coords to WGS84')
    xy              = data.frame(matrix(c(lon_,lat_), ncol=2))
    colnames(xy)    = c('lon', 'lat')
    coordinates(xy) = ~ lon + lat
    proj4string(xy) = CRS("+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs")
    p               = spTransform(xy, CRS("+proj=longlat +datum=WGS84"))
    print (coordinates(xy))
    print (coordinates(p))
    return (p)
  }
  
  
  # Creates a polyline surrounding any MODIS 2016 500m pixel from cropped raster
  showpos = function(x=NULL, y=NULL, name) {
    # If clicked within the Raster on the leaflet map
    r_ = data$r
    cell = cellFromXY(r_, c(x, y))
    
    print (r_)

     if (!is.na(cell)) {
       # Variables we will use:  
       #   row, column
       #   resolution (aka cell size / pixel size)
       #   x (longitude) , y (latitude)
       #   ulraster (Upper Left Corner of the raster in lat/lon)
    
       xmin       = xmin(extent(r_))
       xmax       = xmax(extent(r_))
       ymin       = ymin(extent(r_))
       ymax       = ymax(extent(r_))
       nrows      = nrow(r_)
       ncols      = ncol(r_)
       totalcells = ncell(r_)
       resolution = res(r_)[1]
       vegindex   = (values(r_)[cell])
       
       # Calculate UpperLeft(UL), UpperRight(UR), LowerLeft(LL), and LowerRight(LR) 
       #   coordinates for selected cell/pixel from raster
       
       print (cell)
       print (ncols)
       
       row = ceiling(cell / ncols)
       col = cell %% ncols
       
       if (col == 0){
         col = ncols
       }
       
       print (row)
       print (col)
       
       xclose = ((col - 1) * resolution) + xmin
       xfar   = (col * resolution) + xmin
       yclose = -((row - 1) * resolution) + ymax
       yfar   = -(row * resolution) + ymax
       
       midcellx = xclose + (resolution * .5)
       midcelly = yclose - (resolution * .5)
       midcell  = c(midcellx, midcelly)
         
       datalon = c(xclose, xfar, xfar, xclose ,xclose)
       datalat = c(yclose, yclose, yfar, yfar, yclose)
       
       id_ = paste0(name, '_', row, '_', col, '_', vegindex)
       
       # Check to see if already drawn, and if so remove it from df and leaflet map
       if (id_ %in% data$pixel_df$Id){
         remove_polyline(id = id_, all = FALSE)
         data$pixel_df = subset(data$pixel_df, Id!=id_)
       }else{
         # Draw the pixel polygon on the leaflet map
         add_polyline(datalon, datalat, id_, .95, 'red')
  
         ps = paste0('--Cell Id: ', id_, ' --Cell # in Landcover: ', cell,
                     ' --Row: ', row, ' --Column: ', col, ' --Pft Number: ', vegindex,
                     ' --Middle of Cell lon: ', midcell[1], ' lat: ', midcell[2])
         print (ps)
         
         # Build Dataframe   reactive value = data$pixel_df
         data$pixel_df = rbind(data$pixel_df, data.frame(Id = id_, Site = name, Lat = midcelly, Lon = midcellx, pft = vegindex))
       }
       print (data$pixel_df)
    }
  }
  
  
  # Creates boundary box for clipping rasters using lat/lon from phenocam site
  crop_MODIS_2016_raster = function(lat_, lon_, reclassify=FALSE, primary=NULL, secondary=NULL){
    # us_pth = './www/uslandcover_modis_sinu.tif'
    us_pth = './www/uslandcover_modis.tif'

    us_r   = raster(us_pth)
    resolution = res(us_r)[1]

    # height = 5 * resolution
    # width  = 5 * resolution
    height = .03
    width  = .05
    e      = as(extent(lon_-width, lon_ + width, lat_ - height, lat_ + height), 'SpatialPolygons')
    
    crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"
    r      = crop(us_r, e)
    
    if (reclassify == FALSE){
      return (r)
      
    }else if (reclassify == TRUE){
      
      water = 17*2
      
      m = c(1,NA,
            2,NA,
            3,NA,
            4,NA,
            5,NA,
            6,NA,
            7,NA,
            8,NA,
            9,NA,
            10,NA,
            11,NA,
            12,NA,
            13,NA,
            14,NA,
            15,NA,
            16,NA,
            17,NA)
      
      
      if(!is.null(primary)){
        prim    = primary*2
        m[prim] = 1
        }
      if(!is.null(secondary)){
        sec    = secondary*2
        m[sec] = 2
        }

      print (m)
      # m[water] = 3
      
      rclmat = matrix(m, ncol=2, byrow=TRUE)
      rc     = reclassify(r, rclmat)
      
      return (rc)
    }
  }
  

  # Grabs the list of 3_day csv data from phenocam website
  get_site_roi_3day_csvs = function(name){
    url  = paste('https://phenocam.sr.unh.edu/data/archive/', name, '/ROI/', sep = '')
    page = read_html(url)

    site_hrefs = page %>% html_nodes("a") %>% html_attr("href")
    # csvs_    = site_hrefs[grep('3day.csv|1day.csv', site_hrefs)]  #How to grab both 1 and 3 day csvs
    csvs_      = site_hrefs[grep('3day.csv|gcc90', site_hrefs)]
    csvs_      = csvs_[grep('XX|.png', csvs_, invert=TRUE)]  #invert will take all strings without this
    csv        = csvs_[grep('gcc90_3day', csvs_)]
    csv        = csv[1]

    csv_url = paste('https://phenocam.sr.unh.edu/data/archive/', name, '/ROI/', csv, sep = '')
    print (csv_url)
  }


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
  get_roi_url = function(name, veg_type='None', roi_veg_level='None'){
    roi_url = tryCatch({

      if (veg_type=='None'){
        baseurl = 'https://phenocam.sr.unh.edu'
        siteurl = paste('https://phenocam.sr.unh.edu/webcam/sites/',name,'/', sep = '')
        page    = read_html(siteurl)
        html    = page %>% html_nodes("a") %>% html_attr('href')
        html    = grep('data/archive', html, value=TRUE)[[1]]
        html    = paste(baseurl, html, sep='')

        page2 = read_html(html)
        html2 = page2 %>% html_nodes("a") %>% html_attr('href')
        html2 = grep('data/archive', html2, value=TRUE)
        html2 = grep('.tif', html2, value=TRUE)

        roi     = strsplit(html2, '/')[[1]]
        roi     = grep('.tif', roi, value=TRUE)
        roi     = strsplit(roi, name)[[1]]
        roi     = grep('.tif', roi, value=TRUE)
        roi     = strsplit(roi, '.tif')[[1]]
        roi_url = paste('https://phenocam.sr.unh.edu/data/archive/',
                        name, '/ROI/', name, roi, '_overlay.png', sep = '')
      }else{
        if (roi_veg_level=='Primary'){
          roi_url = paste0(name,'_',veg_type,'_','1000_01_overlay.png')
          roi_url = paste0('https://phenocam.sr.unh.edu/data/archive/',
                           name, '/ROI/', roi_url)
        }else if(roi_veg_level=='Secondary'){
          roi_url = paste0(name,'_',veg_type,'_','0001_01_overlay.png')
          roi_url = paste0('https://phenocam.sr.unh.edu/data/archive/',
                           name, '/ROI/', roi_url)
        }
      }
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
    los = .01
    lat =  site_data_$lat
    lon =  site_data_$lon
    dst = sqrt(los**2 + los**2)
    c   = rotate_pt(lon, lat, (azm_-25), dst)
    b   = rotate_pt(lon, lat, (azm_+25), dst)
    cx  = c[[1]]
    cy  = c[[2]]
    bx  = b[[1]]
    by  = b[[2]]

    datalon = c(lon,cx,bx,lon)
    datalat = c(lat,cy,by,lat)
    camera  = site_data_$site
    id_     = paste('fov',camera, sep='')
    add_polyline(datalon, datalat, id_, .45, 'red')
  }


  # Add a polyline layer to the map
  add_polyline = function(datalon_, datalat_, id_, opacity_, color_='red'){
    leafletProxy("map", data = variables$sites_df) %>%
      # clearShapes()%>%
      addPolylines( datalon_,
                    datalat_,
                    layerId = id_,
                    opacity = opacity_,
                    color   = color_)
  }


  add_polygon = function(datalon_, datalat_, id_, opacity_, color_='red'){
    leafletProxy("map", data = variables$sites_df) %>%
      # clearShapes()%>%
      addPolygons( datalon_,
                    datalat_,
                    layerId = id_,
                    opacity = opacity_,
                    color   = color_)
  }


  # remove all polylines
  remove_polyline = function(id_=NULL, all=TRUE){
    if (all == TRUE){
      leafletProxy("map") %>%
        clearShapes()
    }else if(all == FALSE){
        leafletProxy('map') %>% removeShape(layerId = id_)
      }
  }


  # Zoom to site
  zoom_to_site = function(site_, zoom){
    site_data          = get_site_info(site_)
    description        = site_data$site_description
    camera_orientation = site_data$camera_orientation
    lat                = site_data$lat
    lon                = site_data$lon
    cam_orientation    = as.character(site_data$camera_orientation)

    degrees   = as.numeric(orientation_key[cam_orientation])
    elevation = site_data$elev
    camera    = site_data$site
    drawROI   = FALSE

    if (zoom == TRUE){
      drawROI = isolate(input$drawROI)
      leafletProxy('map', data = variables$sites_df) %>%
        clearPopups() %>%
        clearMarkers() %>%
        # clearShapes() %>%
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
    rad  = azm * (pi / 180)
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

  count = function(){
    isolate({
      counter$countervalue = counter$countervalue + 1
      print (counter$countervalue)
    })
  }

  build_polygon_table = function(){
    # Creating Dataframe with 1 record per shapefile
    if (nrow(data$df) == 0){
      df = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('Name', 'Longitude', 'Latitude'))
    }else {
      df = aggregate(data$df[,c(2,3)], list(data$df$Name), max)
    }
    x  = df
    print (x)
    # x$Date = Sys.time() + seq_len(nrow(x))
    output$pAOIchart = renderDT(x, selection = 'none', editable = TRUE)
    proxy            = dataTableProxy('pAOIchart')
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
  # is not null function
  is.not.null <- function(x) ! is.null(x)

  # not in
  '%!in%' <- function(x,y)!('%in%'(x,y))

  # custom markers created for Active/nonActive
  getColor <- function(cams) {
    sapply(cams$active, function(active) {
      if(active == 'True') {
        "blue"
      } else if(active == 'False') {
        "red"
      } else {
        "orange"
      } })
  }

  switch_to_explorer_panel = function(){
    # Ids to show:
    shinyjs::show(id = 'explorerTitle')
    shinyjs::show(id = 'usZoom')
    shinyjs::show(id = 'showSites')
    shinyjs::show(id = 'filterSites')
    shinyjs::show(id = 'site')
    shinyjs::show(id = 'siteZoom')
    shinyjs::show(id = 'drawImage')
    shinyjs::show(id = 'drawImageROI')
    shinyjs::show(id = 'analyzerMode')
    shinyjs::show(id = 'mouse')
    # Ids to hide:
    shinyjs::hide(id = 'analyzerTitle')
    shinyjs::hide(id = 'siteExplorerMode')
    shinyjs::hide(id = 'showModisSubset')
    shinyjs::hide(id = 'drawROI')
    shinyjs::hide(id = 'azm')
    shinyjs::hide(id = 'siteTitle')
    shinyjs::hide(id = 'plotPhenocamGCC')
    shinyjs::hide(id = 'pftSelection')
    shinyjs::hide(id = 'showHidePlot')
    shinyjs::hide(id = 'modisLegend')
    shinyjs::hide(id = 'plotpanel')
    shinyjs::hide(id = 'highlightPixelMode')
    shinyjs::hide(id = 'getAPPEEARSpoints')
    leafletProxy('map') %>%
      clearControls() %>%
      clearShapes() %>%
      addLegend(values = c(1,2), group = "site_markers", position = "bottomright",
                labels = c("Active sites", "Inactive sites"), colors= c("blue","red")) %>%
      addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
                     position = c("topleft"),
                     options = layersControlOptions(collapsed = TRUE)) 
    
  }
  switch_to_analyzer_panel = function(){
    # Ids to show:
    shinyjs::show(id = 'analyzerTitle')
    shinyjs::show(id = 'siteExplorerMode')
    shinyjs::show(id = 'showModisSubset')
    shinyjs::show(id = 'drawROI')
    shinyjs::show(id = 'drawImage')
    shinyjs::show(id = 'drawImageROI')
    shinyjs::show(id = 'mouse')
    shinyjs::show(id = 'siteTitle')
    shinyjs::show(id = 'plotPhenocamGCC')
    shinyjs::show(id = 'pftSelection')
    shinyjs::show(id = 'getAPPEEARSpoints')
    shinyjs::show(id = 'highlightPixelMode')
    # Ids to hide:
    shinyjs::hide(id = 'explorerTitle')
    shinyjs::hide(id = 'usZoom')
    shinyjs::hide(id = 'showSites')
    shinyjs::hide(id = 'analyzerMode')
    shinyjs::hide(id = 'filterSites')
    shinyjs::hide(id = 'site')
    shinyjs::hide(id = 'siteZoom')
    shinyjs::hide(id = 'showHidePlot')
  }
  
  # Returns site name from a cached task
  get_site_from_task = function(task_name_){
    elements = strsplit(task_name_, split = '_', fixed=TRUE)
    element_length = length(elements[[1]])
    if (element_length == 5){
      site_name_ = elements[[1]][1]
      return (site_name_)
    }else if(element_length > 5){
      num = element_length - 5
      elem = elements[[1]][1]
      for (x in c(1:num)){
        elem = paste(elem, elements[[1]][x+1], sep='_')
      }
      return (elem)
    }else{
      sprintf('This task is missing information/invalid: %s', task_name_)
      return(FALSE)
    }
  }
  
  # Given a site name, function returns the appeears task record
  get_appeears_task = function(name){
    task_pos = grep(name ,appeears_tasks$task_name)
    for (i in c(1:length(task_pos))){
      row = get_site_from_task(appeears_tasks[task_pos[i],]$task_name)
      if (row == name){
        task_ = appeears_tasks[task_pos[i],]$task_name
      }
    }
    return (subset(appeears_tasks, appeears_tasks$task_name == task_))
  }
  
  
  # Downloads file from bundle (whichever ft is set to (only nc and qa_csv))
  download_bundle_file = function(site_task_id_, ft){
    response = GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", site_task_id_, sep = ""))
    bundle_response = prettify(jsonlite::toJSON(content(response), auto_unbox = TRUE))
    document = jsonlite::fromJSON(txt=bundle_response)
    files = document$files
    if (ft == 'nc'){
      netcdf    = subset(files, file_type == 'nc')
      download_this_file = netcdf$file_id
      file_name = netcdf$file_name
    }else if(ft == 'qa_csv'){
      csvs      = subset(files, file_type == 'csv')
      qa_csv    = csvs[grep('Quality-lookup', csvs$file_name), ]$file_id
      download_this_file = qa_csv
      file_name = csvs[grep('Quality-lookup', csvs$file_name), ]$file_name
    }
    dest_dir = './www/'
    filepath = paste(dest_dir, file_name, sep = '')
    response = GET(paste("https://lpdaacsvc.cr.usgs.gov/appeears/api/bundle/", site_task_id_, '/', download_this_file, sep = ""),
                   write_disk(filepath, overwrite = TRUE), progress())
    return (filepath)
  }
  
  
  # Deletes the netcdf from input filepath
  delete_file = function(filepath_){
    if (file.exists(filepath_)) file.remove(filepath_)
  }
  
  # Builds a color palet for the modis landcover raster layer
  build_pft_palette = function(raster_){
    print ('building palet')
    colors = c()
    names  = c()
    color_list    = c('#1b8a28', '#36d03e', '#9ecb30', '#a0f79f', '#91bb88', '#b99091', '#f0dfb8', '#d6ed9a',
                      '#f1dc07', '#ecbb5b', '#4981b1', '#fcee72', '#fd0608', '#9b9353', '#bdbec0', '#bdbec0', '#89cae3')
    v = unique(values(raster_))
    remove = c(NA)
    v = v [! v %in% remove]
    v = sort(v, decreasing = FALSE)
    
    for (x in v){
      if (x == 17){
        colors = c(colors,color_list[17])
        name   = as.character(subset(pft_df, pft_df$pft_key == 0)$pft_expanded)
        names  = c(names, name)
      }else{
        colors = c(colors, color_list[x])
        name   = as.character(subset(pft_df, pft_df$pft_key == x)$pft_expanded)
        names  = c(names, name)
      }
    }
    colors_ = list('colors' = colors, 'names' = names)
    return (colors_)
  }
  
}
