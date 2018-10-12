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
                      none = '')
  
  highlighted = reactiveValues(
                      group = '')

  # 'analyzer' or 'explorer'
  panel   = reactiveValues(mode = '')

  counter = reactiveValues(countervalue = 0)

  modis   = reactiveValues(data = data.frame(),
                           cached_ndvi = list())


  data    = reactiveValues(
                      draw_mode = FALSE,
                      run   = 0,
                      names = c(),
                      df    = data.frame(),
                      veg_types = c(),
                      pixel_sps = SpatialPolygons(list()),
                      pixel_sps_500m = SpatialPolygons(list()),
                      pixel_sps_250m = SpatialPolygons(list()))

  # Empty reactive spdf
  value = reactiveValues(drawnPoly = SpatialPolygonsDataFrame(SpatialPolygons(list()),
                                                              data=data.frame()))


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
    data$df = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('Name', 'Longitude', 'Latitude', 'LeafletId'))
  })

  #--------------------------------------------------------------------------------------------------------------------------------------
  #  OBSERVERS
  #--------------------------------------------------------------------------------------------------------------------------------------

  # Start of Drawing - set highlight pixel to off
  observeEvent(input$map_draw_start, {
    data$draw_mode = TRUE
    print ('Starting Draw Mode')
  })
  
  observeEvent(input$clearPlot, {
    output$ndvi_pixels_plot = renderPlot({
      # Only plotting the first 250m pixel
      df = data.frame()
      p = ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 1)
      p
    })
  })
  
  # Turn off 500m highlighted pixels if 250m highlighted pixels
  observe({
    a = input$highlightPixelMode
    if (a == TRUE){
      leafletProxy('map') %>% showGroup('500m Highlighted Pixels')
      updateCheckboxInput(session, 'highlightPixelModeNDVI', value = FALSE)
    }
  })
  
  # Turn off 250m highlighted pixels if 500m highlighted pixels
  observe({
    b = input$highlightPixelModeNDVI
    if(b == TRUE){
      leafletProxy('map') %>% showGroup('250m Highlighted Pixels')
      updateCheckboxInput(session, 'highlightPixelMode', value = FALSE)
    }
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
    variables$sites = variables$sites_df$Sitename
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
      print ('Drawing fov for phenocam site')
      site            = isolate(input$site)
      site_data       = get_site_info(site)
      cam_orientation = as.character(site_data$camera_orientation)
      degrees         = as.numeric(orientation_key[cam_orientation])
      run_add_polyline(site_data, degrees)
      shinyjs::show(id = 'azm')
      updateSliderInput(session, 'azm', value = degrees)
    }
    else if (roi_bool == FALSE){
      shinyjs::hide(id = 'azm')
      leafletProxy('map') %>% removeShape(layerId = 'azm_')
      print ('Removing fov for phenocam site')
        }
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
      
      lat             = site_data$Lat
      lon             = site_data$Lon
      description     = site_data$site_description
      elevation       = site_data$Elev
      camera          = site_data$Sitename
      site_type       = site_data$site_type
      #nimage          = site_data$nimage
      cam_orientation = as.character(site_data$camera_orientation)
      degrees         = as.numeric(orientation_key[cam_orientation])
      active          = site_data$active
      date_end        = site_data$date_last
      date_start      = site_data$date_first
      get_site_popup(camera, lat, lon, description, elevation, site_type, cam_orientation, degrees,
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

      lat_       = site_data$Lat[1]
      lon_       = site_data$Lon[1]
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
    
    data$global_pth = './www/global_landcover_2016.tif'
    global_r   = raster::raster(data$global_pth)

    veg_types  = c()
    print ('Switching to Analyze Mode')
    zoom_to_site(site, TRUE)
    highlighted$group = paste0(site, ' Highlighted Pixels')

    output$analyzerTitle = renderText({paste0('Site:: ', site)})
    switch_to_analyzer_panel()

    print (sprintf('grabbing task row from appeears for site: %s', site))
    appeears$ndvi = get_appeears_task(site, type = 'ndvi')
    appeears$tds  = get_appeears_task(site, type = 'tds')


    veg_idx       = is.element(roi_files$site, site)
    veg_match     = roi_files[veg_idx,]
    
    if (nrow(veg_match) == 0){
      updateSelectInput(session, 'pftSelection', choices = 'No ROI Vegetation Available')
    }else{
      veg_types = c()
      for (i in c(1:nrow(veg_match))){
        veg.idx   = is.element(pft_df$pft_abbreviated, veg_match$roitype[i])
        veg       = pft_df$pft_expanded[veg.idx]
        add_veg   = as.character(veg[1])
        veg_types = c(veg_types, paste0(add_veg, '_', i))
      }
      data$veg_types = veg_types
      
      # Building Landcover layer and color pallette for specific pft composition in clipped raster
      r  = crop_raster(site_data$Lat, site_data$Lon, global_r, reclassify=FALSE)
      c3 = build_pft_palette(r)
      data$r_landcover = r

      
      updateSelectInput(session, 'pftSelection', choices = veg_types)
      print (veg_types)

      pft = strsplit(veg_types[1], '_')[[1]][1]
      print (pft)
      pft_key = (subset(pft_df, pft_df$pft_expanded == pft)$pft_key)
      print (as.numeric(pft_key))
      
      rc   = crop_raster(site_data$Lat, site_data$Lon, global_r, reclassify=TRUE, primary = as.numeric(pft_key))
      leafletProxy('map') %>%
        clearControls() %>%
        clearImages() %>%

        addRasterImage(data$r_landcover, opacity = .65, project=TRUE, group='MODIS Land Cover 2016', colors = c3$colors) %>%
        addRasterImage(rc, opacity = .2, project=TRUE, group= 'Vegetation Cover Agreement', colors= c('green','gray')) %>%

        addLegend(labels = c3$names, colors = c3$colors, position = "bottomleft", opacity = .95) %>%
        addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
                         overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '500m Highlighted Pixels'),
                         position = c("topleft"),
                         options = layersControlOptions(collapsed = FALSE))
      
      print ('Attempting to run pft selection observer')
    }
  })

  # When ROI Vegetation type changes replot highlighted veg type
  observeEvent(input$pftSelection, {
    if (panel$mode == 'analyzer'){
        print ('Running pft Selection')
        global_r   = raster::raster(data$global_pth)

        site       = input$site
        site_data  = get_site_info(site)
        pft        = input$pftSelection
        
        c3 = build_pft_palette(data$r_landcover)
        pft = strsplit(pft, '_')[[1]][1]
        pft_key = (subset(pft_df, pft_df$pft_expanded == pft)$pft_key)
        rc   = crop_raster(site_data$Lat, site_data$Lon, global_r, reclassify=TRUE, primary = as.numeric(pft_key))
        
        leafletProxy('map') %>%
          clearImages() %>%

          addRasterImage(data$r_landcover, opacity = .65, project=TRUE, group='MODIS Land Cover 2016', colors = c3$colors) %>%
          addRasterImage(rc, opacity = .35, project=TRUE, group= 'Vegetation Cover Agreement', colors= c('green','gray')) %>%

          addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
                           overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '500m Highlighted Pixels'),
                           position = c("topleft"),
                           options = layersControlOptions(collapsed = FALSE))
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
    #print (input$plotPhenocamGCC)
    #name=input$plotPhenocamGCC
    #print(name$Sitename)
    #print(site_name)
    phenoCamData=get_site_roi_3day_csvs(input$site)
    
  })
  
  # Button that plots NDVI
  observeEvent(input$plotDataButton, {
    print ('Plotting NDVI')

    # Inputs from popup
    data_type_selected  = input$dataTypes_plot
    pixel_size_selected = input$pixelTypes
    
    site        = input$site
    site_data   = get_site_info(site)
    
    #----NDVI------------------------------------------------------------------------------------------------------------------------
    if (data_type_selected == 'MODIS NDVI'){
      nc_data = data$site_nc
      dates   = ncvar_get(nc_data, 'time')
      crs = CRS("+proj=longlat +datum=WGS84")
      
      start_date_str = nc_data[[11]]$time$units
      start_date     = as.Date(strsplit(start_date_str, ' ')[[1]][3])
      
      # In date format
      date_list = dates + start_date 
      
      nc_ndvi = data$ndvi_nc  # only ndvi
      lat = ncvar_get(nc_data, "lat")
      lon = ncvar_get(nc_data, "lon")
      
      data_plot  = c()
      polys_len  = c()
      pixel_len  = c()
      ndvis_     = c()
      int_pixels = list()
      final_ndvi_list = c()
      len = length(dates)
      pixels = data$pixel_sps_250m
      
      # Setting length of polygons to select with 
      if (is.null(polys_len)){
        # Number of polygons (aka highlighted pixels) selected
        polys_len = length(pixels)
      }
      
      if (is.null(pixels@polygons[1][[1]])){
        print ('No pixels selected')
        shinyjs::show(id = 'noPixelWarning')
        
      }else{
        shinyjs::show(id = 'buildingPlot')
        
        withProgress(message = 'Buliding NDVI Plot: ', detail = paste0('Site: ', site), value = 0, {
          for (x in c(1:len)){
            incProgress(1/len)
            r_ndvi = raster(t(nc_ndvi[,,x]), xmn=min(lon), xmx=max(lon), ymn=min(lat), ymx=max(lat), crs=crs)
            values_under_polygon = extract(r_ndvi, pixels)
            
            # Setting length of pixels in each polygon
            if (length(int_pixels) == 0){
              for (xx in c(1:polys_len)){
                # Number of pixels picked up by the highlighted pixel
                pixel_len  = length(values_under_polygon[[xx]])
                int_pixels[[xx]] = pixel_len
              }
            }
            ndvi_means = c()
            # Loop through the different polygons to extract ndvi values and save to dataframe
            for (i in c(1:polys_len)){
              ndvi_ = values_under_polygon[[i]]
              ndvis_ = c(ndvis_, ndvi_)
            }
          }
          # Build out individual pixel lists as c() and do %1,2,3,4,5 (for number of pixels)
          #   to build the necessary dataframe lists.
          list_ = list()
          cols  = c()
          for (i in c(1:polys_len)){
            col = paste0('pixel_', as.character(i))
            cols = c(cols, col)
            a = c(1:(length(ndvis_)))
            b = a[seq((1 + (i-1)), length(a), polys_len)]
            print (col)
            print (b)
            list_[[col]] = ndvis_[b]
          }
          
          #Parse data with Dates
          data_df = data.frame(date = date_list, list_)
          start_ = input$dataDateRange[1]
          end_   = input$dataDateRange[2]
          
          parsed_data = subset(data_df, date >= start_ & date <= end_)

          p = ggplot(data = parsed_data, aes(x= date, y= pixel_1)) +
            geom_line()
        })
      }
    #----PHENOCAM------------------------------------------------------------------------------------------------------------------------
    }else if(data_type_selected == 'PhenoCam GCC'){
      # withProgress(message = 'Buliding Phenocam GCC Plot: ', detail = paste0('Site: ', site), value = 0, {
        # incProgress(.2)
        phenoCamData = get_site_roi_3day_csvs(site)
        # incProgress(.2)
        ##pick up here for plotting
        parsed_data$date = as.Date(parsed_data$date)
        source           = rep('MODIS', nrow(parsed_data))
        # incProgress(.1)
        parsed_data       = cbind(parsed_data, source)
        phenoCamData$date = as.Date(phenoCamData$date) ##pickup here
        # incProgress(.1)
        source = rep('PhenoCam', nrow(phenoCamData))
        sDF    = left_join(parsed_data, phenoCamData)#pick up here
        # incProgress(.1)
        phenoCamData$date = as.Date(phenoCamData$date)
        p                 = left_join(parsed_data, phenoCamData)#pick up here
      # })
    }else{      
      df = data.frame()
      p = ggplot(df) + geom_point() + xlim(0, 10) + ylim(0, 1)
    }
    
    # Plot p
    output$ndvi_pixels_plot = renderPlot({
      p
    })
    
    print (paste0(data_type_selected, ' Plotting Completed'))
    updateTabsetPanel(session, 'navbar', selected = 'PlotPanel')
    shinyjs::hide(id = 'buildingPlot')
    shinyjs::show(id = 'doneBuildingPlot')
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
          click = input$map_click
          site  = input$site
          if (panel$mode == 'analyzer'){
            if (!is.null(click)) {
              lon_ = click$lng
              lat_ = click$lat
              
              if (input$highlightPixelMode == TRUE){
                showpos(x = lon_ , y = lat_, site, data$r_landcover, '500m')
              }else if(input$highlightPixelModeNDVI == TRUE){
                showpos(x = lon_ , y = lat_, site, data$r_ndvi_cropped, '250m')
              }}}
    })
  
  # Observer for the popup
  observeEvent(input$plotRemoteData, {
      shinyjs::hide(id = 'noPixelWarning')
      shinyjs::hide(id = 'buildingPlot')
      shinyjs::hide(id = 'doneBuildingPlot')
  })
  

  # Add netcdf from AppEEARS of netcdf
  observeEvent(input$getDataButton, {
    site       = input$site
    site_data  = get_site_info(site)
    
    # leafletProxy('map') %>%
    #   showGroup('Open Topo Map')
    
    withProgress(message = 'Grabbing AppEEARS Data. ', detail = paste0('   Site: ', site, ', NDVI'), value = 0, {
      
      incProgress(.1)
      # load in netcdf for NDVI layer
      #------------------------------------------------------------------------
      file_ndvi    = download_bundle_file(appeears$ndvi$task_id, 'nc')
      incProgress(.2)
      file_ndvi_qa = download_bundle_file(appeears$ndvi$task_id, 'qa_csv')
      ndvi_output    = nc_open(file_ndvi)
      incProgress(.1)
      v6_QA_lut      = read.csv(file_ndvi_qa)
      delete_file(file_ndvi)
      delete_file(file_ndvi_qa)
      incProgress(.1)
      
      # load in netcdf for Transition Dates layer
      #------------------------------------------------------------------------
      file_tds    = download_bundle_file(appeears$tds$task_id, 'nc')
      data$tds_nc = nc_open(file_tds)
      delete_file(file_tds)
      # Loading in the Transition Date layers
      NBAR_EVI_Onset_Greenness_Maximum = ncvar_get(data$tds_nc, "NBAR_EVI_Onset_Greenness_Maximum")
      NBAR_EVI_Onset_Greenness_Minimum = ncvar_get(data$tds_nc, "NBAR_EVI_Onset_Greenness_Minimum")
      Onset_Greenness_Decrease = ncvar_get(data$tds_nc, "Onset_Greenness_Decrease")
      Onset_Greenness_Increase = ncvar_get(data$tds_nc, "Onset_Greenness_Increase")
      Onset_Greenness_Maximum = ncvar_get(data$tds_nc, "Onset_Greenness_Maximum")
      Onset_Greenness_Minimum = ncvar_get(data$tds_nc, "Onset_Greenness_Minimum")
      
      incProgress(.1)
  
      # netcdf manipulation
      #------------------------------------------------------------------------
      v6_NDVI = ncvar_get(ndvi_output, "_250m_16_days_NDVI")
      incProgress(.1)
      v6_QA   = ncvar_get(ndvi_output, "_250m_16_days_VI_Quality")
      incProgress(.1)
      
      data$site_nc = ndvi_output
  
      # Set lat and lon arrays for NDVI data
      lat_NDVI = ncvar_get(ndvi_output, "lat")
      lon_NDVI = ncvar_get(ndvi_output, "lon")
  
      # Grab the fill value and set to NA
      incProgress(.1)
      fillvalue = ncatt_get(ndvi_output, "_250m_16_days_NDVI", "_FillValue")
      incProgress(.1)
      v6_NDVI[v6_NDVI == fillvalue$value] = NA
      
      data$ndvi_nc = v6_NDVI
  
      # Define the coordinate referense system proj.4 string
      crs = CRS("+proj=longlat +datum=WGS84")
  
      # Grab first observation of NDVI and Quality datasets
      v6_NDVI = raster(t(v6_NDVI[,,1]), xmn=min(lon_NDVI), xmx=max(lon_NDVI), ymn=min(lat_NDVI), ymx=max(lat_NDVI), crs=crs)
      v6_NDVI_original = v6_NDVI
      v6_QA = raster(t(v6_QA[,,1]), xmn=min(lon_NDVI), xmx=max(lon_NDVI), ymn=min(lat_NDVI), ymx=max(lat_NDVI), crs=crs)
      #------------------------------------------------------------------------
      YlGn = brewer.pal(9, "YlGn")
      
      data$r_ndvi_cropped = crop_raster(site_data$Lat, site_data$Lon, v6_NDVI)
      
      # extracted_ndvi = extract(v6_NDVI_original, data$pixel_sps_250, snap = 'in')
      # print (extracted_ndvi)
  
      # leafletProxy('map') %>% addRasterImage(data$r_ndvi_cropped, opacity = .7, group = 'test1', col = YlGn)
      shinyjs::show(id = 'highlightPixelModeNDVI')
      
      # Build grid
      build_raster_grid(data$r_ndvi_cropped)
      incProgress(.1)
      
      shinyjs::show(id = 'plotRemoteData')
      shinyjs::hide(id = 'noPixelWarning')
      
      start_site = as.character(site_data$date_first)
      end_site   = as.character(site_data$date_last)
      print (start_site)
      print (end_site)
      updateSliderInput(session, 'dataDateRange', 
                        min = as.Date(start_site), 
                        max = as.Date(end_site), 
                        value = c(as.Date(start_site), as.Date(end_site)))
    })
    shinyjs::show(id = 'doneGetData')
  })


  #--------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  FUNCTIONS
  #--------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------------------------
#calculates optimal span for smooth
  optimal_span = function(y,
                          x = NULL,
                          weights = NULL,
                          step = 0.01,
                          label = NULL,
                          plot = FALSE){
    
    # custom AIC function which accepts loess regressions
    myAIC = function(x){
      
      if (!(inherits(x, "loess"))){
        stop("Error: argument must be a loess object")
      }
      
      # extract loess object parameters
      n = x$n
      traceL = x$trace.hat
      sigma2 = sum( x$residuals^2 ) / (n-1)
      delta1 = x$one.delta
      delta2 = x$two.delta
      enp = x$enp
      
      # calculate AICc1
      # as formulated by Clifford M. Hurvich; Jeffrey S. Simonoff; Chih-Ling Tsai (1998)
      AICc1 = n*log(sigma2) + n* ( (delta1/delta2)*(n+enp) / ((delta1^2/delta2)-2))
      
      if(is.na(AICc1) | is.infinite(AICc1)){
        return(NA)
      }else{
        return(AICc1)
      }
    }
    
    # create numerator if there is none
    if (is.null(x)){
      x = 1:length(y)
    }
    
    # return AIC for a loess function with a given span
    loessAIC = function(span){
      # check if there are weights, if so use them
      if ( is.null(weights) ){
        fit = suppressWarnings(try(stats::loess(y ~ as.numeric(x),
                                                span = span),
                                   silent = TRUE))
      } else {
        fit = suppressWarnings(try(stats::loess(y ~ as.numeric(x),
                                                span = span, 
                                                weights = weights),
                                   silent = TRUE))
      }
      
      # check if the fit failed if so return NA
      if (inherits(fit, "try-error")){
        return(NA)
      }else{
        return(myAIC(fit))
      }
    }
    
    # parameter range
    span = seq(0.01, 1, by = step)
    
    # temporary AIC matrix, lapply loop
    # (instead of for loop) cleaner syntax
    tmp = unlist(lapply(span, loessAIC))
    
    # find the optimal span as the minimal AICc1 value
    # in the calculated range (span variable)
    opt_span = span[which(tmp == min(tmp, na.rm = TRUE))][1]
    
    # plot the optimization if requested
    if (plot == TRUE){
      
      graphics::par(mfrow = c(2,1))
      plot(as.numeric(x),y,
           xlab = 'value',
           ylab = 'Gcc',
           type = 'p',
           pch = 19,
           main = label)
      
      col = grDevices::rainbow(length(span),alpha = 0.5)
      
      for (i in 1:length(span)){
        fit = stats::loess(y ~ as.numeric(x),
                           span = span[i])
        graphics::lines(fit$x,
                        fit$fitted,
                        lwd = 1,
                        col = col[i])
      }
      
      fit = stats::loess(y ~ as.numeric(x),
                         span = opt_span)
      
      graphics::lines(fit$x,
                      fit$fitted,
                      lwd = 3,
                      col = 'black',
                      lty = 1)
      
      plot(span,
           tmp,
           pch = 19,
           type = 'p',
           ylab = 'AICc1',
           col = col)
      
      graphics::abline(v = opt_span,col = 'black')
      
    }
    
    # trap error and return optimal span
    if (is.na(opt_span)) {
      return(NULL)
    } else {
      return(opt_span)
    }
  }
  #smoothes ndvi, evi or gccc data
  smooth_ts = function(data,
                       metrics = c("gcc_mean",
                                   "gcc_50",
                                   "gcc_75",
                                   "gcc_90",
                                   "rcc_mean",
                                   "rcc_50",
                                   "rcc_75",
                                   "rcc_90"),
                       force = TRUE, frequency) {
    
    
    
    # split out data from read in or provided data
    df = data
    
    # maximum allowed gap before the whole stretch is
    # flagged as too long to be reliably interpolated
    maxgap = 14
    
    # create convenient date vector
    # (static for all data)
    dates = as.Date(df$date)
    
    # create output matrix
    output = matrix(NA, length(dates), length(metrics) * 2 + 1)
    output = as.data.frame(output)
    column_names = c(sprintf("smooth_%s", metrics),
                     sprintf("smooth_ci_%s", metrics),
                     "int_flag")
    colnames(output) = column_names
    
    # loop over all metrics that need smoothing
    for (i in metrics) {
      
      # get the values to use for smoothing
      v=is.element(colnames(df), i)
      values = df[, ..v]
      
      # flag all outliers as NA
      # if the metric is gcc based
      if (grepl("gcc", i)) {
        outliers = df[, which(colnames(df) == sprintf("outlierflag_%s", i))]
        values[outliers == 1] = NA
      }
      
      # create yearly mean values and fill in time series
      # with those, keep track of which values are filled
      # using the int_flag data
      nr_years = length(unique(df$year))
      
      # find the location of the original NA values
      # to use to fill these gaps later
      na_orig = which(is.na(values))
      
      # na locations (default locations for 3-day product)
      # this to prevent inflation of the number of true
      # values in the 3-day product
      loc = seq(2,366,3)
      loc = (df$doy %in% loc)
      
      # Calculate the locations of long NA gaps.
      # (find remaining NA values after interpolation,
      # limited to 2 weeks in time)
      long_na = which(is.na(zoo::na.approx(
        values, maxgap = maxgap, na.rm = FALSE
      )))
      
      # also find the short gaps (inverse long gaps)
      # to smooth spikes
      short_na = which(!is.na(zoo::na.approx(
        values, maxgap = maxgap, na.rm = FALSE
      )))
      short_na = which(short_na %in% is.na(values))
      
      # this routine takes care of gap filling large gaps
      # using priors derived from averaging values across
      # years or linearly interpolating. The averaging over
      # years is needed to limit artifacts at the beginning
      # and end of cycles in subsequent phenophase extraction
      if (nr_years >= 2) {
        
        # used to be 3, fill values using those of the remaining year
        
        # calculate the mean values for locations
        # where there are no values across years
        fill_values = by(values,INDICES = df$doy, mean, na.rm = TRUE)
        doy_fill_values = as.numeric(names(fill_values))
        #doy_na = df$doy[na_orig]
        doy_na = df$doy[long_na]
        
        # calculate the interpolated data based on
        # the whole dataset
        int_data = unlist(lapply(doy_na,
                                 function(x,...) {
                                   fv = fill_values[which(doy_fill_values == x)]
                                   if (length(fv) == 0) {
                                     return(NA)
                                   }else{
                                     return(fv)
                                   }
                                 }))
        
        # gap fill the original dataset using
        # the interpolated values
        gap_filled_prior = values
        #gap_filled_prior[na_orig] = int_data
        gap_filled_prior[long_na] = int_data
        
        # reset NA short sections to NA and interpolate these linearly
        # only long NA periods merit using priors
        gap_filled_prior[short_na] = NA
        gap_filled_linear = zoo::na.approx(gap_filled_prior, na.rm = FALSE)
        
        # the above value should be independent of the ones used in the carry
        # forward / backward exercise
        
        # traps values stuck at the end in NA mode, use carry
        # forward and backward to fill these in! These errors
        # don't pop up when using a fitting model (see above)
        gap_filled_forward = zoo::na.locf(gap_filled_linear,
                                          na.rm = FALSE)
        gap_filled_backward = zoo::na.locf(gap_filled_linear,
                                           na.rm = FALSE,
                                           fromLast = TRUE)
        
        # drop in values at remaining NA places
        gap_filled_forward[is.na(gap_filled_forward)] = gap_filled_backward[is.na(gap_filled_forward)]
        gap_filled_backward[is.na(gap_filled_backward)] = gap_filled_forward[is.na(gap_filled_backward)]
        
        # take the mean of the carry forward and backward run
        # this should counter some high or low biases by using the
        # average of last or first value before or after an NA stretch
        gap_filled_linear = ( gap_filled_forward + gap_filled_backward ) / 2
        gap_filled = apply(cbind(gap_filled_prior,gap_filled_linear),1,max,na.rm=TRUE)
        
      }else{
        
        # for short series, where averaging over years isn't possible
        # linearly interpolate the data for gap filling
        # it's not ideal (no priors) but the best you have
        gap_filled = zoo::na.approx(values, na.rm = FALSE)
        
        # traps values stuck at the end in NA mode, use carry
        # forward and backward to fill these in! These errors
        # don't pop up when using a fitting model (see above)
        gap_filled = zoo::na.locf(gap_filled, na.rm = FALSE)
        gap_filled = zoo::na.locf(gap_filled, na.rm = FALSE, fromLast = TRUE)
      }
      
      # the gap_filled object is used in the subsequent analysis
      # to calculate the ideal fit, down weighing those areas
      # which were interpolated
      
      # create weight vector for original NA
      # values and snow flag data
      weights = rep(1,nrow(values))
      weights[na_orig] = 0.001
      #weights[df$snow_flag == 1] = 0.001
      
      # smooth input series for plotting
      # set locations to NA which would otherwise not exist in the
      # 3-day product, as not to inflate the number of measurements
      if (frequency == 3){
        
        optim_span = suppressWarnings(
          optimal_span(x = as.numeric(dates[loc]),
                       y = gap_filled[loc],
                       plot = FALSE))
        
        fit = suppressWarnings(
          stats::loess(gap_filled[loc] ~ as.numeric(dates[loc]),
                       span = optim_span,
                       weights = weights[loc]))
        
      } else { # 1-day product
        
        optim_span = suppressWarnings(
          optimal_span(x = as.numeric(dates),
                       y = gap_filled,
                       plot = FALSE))
        
        fit = suppressWarnings(
          stats::loess(gap_filled ~ as.numeric(dates),
                       span = optim_span,
                       weights = weights))
        
      }
      
      # make projections based upon the optimal fit
      fit = suppressWarnings(stats::predict(fit, as.numeric(dates), se = TRUE))
      
      # grab the smoothed series and the CI (from SE)
      # set to 0 if no SE is provided
      values_smooth = fit$fit
      
      # calculate the CI (from SE)
      values_ci = 1.96 * fit$se
      
      # cap CI values to 0.02
      values_ci[values_ci > 0.02] = 0.02
      
      # trap trailing and starting NA values
      values_smooth = zoo::na.locf(values_smooth,
                                   na.rm=FALSE)
      values_smooth = zoo::na.locf(values_smooth,
                                   fromLast = TRUE,
                                   na.rm=FALSE)
      
      # set values for long interpolated values to 0
      # these are effectively missing or inaccurate
      # (consider setting those to NA, although this
      # might mess up plotting routines)
      values_ci[long_na] = 0.02
      
      # trap values where no CI was calculated and
      # assign the fixed value
      values_ci[is.nan(fit$se)] = 0.02
      values_ci[is.na(fit$se)] = 0.02
      values_ci[is.infinite(fit$se)] = 0.02
      
      # set values to NA if interpolated
      # max gap is 'maxgap' days, to avoid flagging periods where
      # you only lack some data
      # this is redundant should only do this once (fix)
      int = zoo::na.approx(values, maxgap = maxgap, na.rm = FALSE)
      
      # put everything in the output matrix
      output$int_flag[which(is.na(int))] = 1
      output[, which(colnames(output) == sprintf("smooth_%s", i))] = round(values_smooth,5)
      output[, which(colnames(output) == sprintf("smooth_ci_%s", i))] = round(values_ci,5)
      
      cols = rep("red",length(gap_filled))
      cols[long_na] = "green"
    }
    
    # drop previously smoothed data from
    # a data frame
    # dropvar = is.element(names(df), column_names)  #maybe break here
    # df = df[,!dropvar]
    df = cbind(df, output)
    
    # put data back into the data structure
    data= df
    
    # write the data to the original data frame or the
    # original file (overwrites the data!!!)
    
    return(data) #data, 
    
  }
  
  
  # Build grid for any input raster
  build_raster_grid = function(raster_){
    r_         = raster_
    xmin       = xmin(extent(r_))
    xmax       = xmax(extent(r_))
    ymin       = ymin(extent(r_))
    ymax       = ymax(extent(r_))
    nrows      = nrow(r_)
    ncols      = ncol(r_)
    resolution = res(r_)[1]
    
    lats = c()
    lons = c()
    ids  = c()
    for (x in c(0:ncols)){
      id = x
      lat1 = ymax
      lat2 = ymin
      
      lon1 = xmin + (x * resolution)
      lon2 = xmin + (x * resolution)
      
      lats = c(lats, lat1, lat2)
      lons = c(lons, lon1, lon2)
      ids  = c(ids, id, id)
    }
    
    for (xx in c(0:nrows)){
      id = xx + x
      lat1 = ymax - (xx * resolution)
      lat2 = ymax - (xx * resolution)
      
      lon1 = xmax
      lon2 = xmin
      
      lats = c(lats, lat1, lat2)
      lons = c(lons, lon1, lon2)
      ids  = c(ids, id, id)
    }
    df.sp = data.frame(id=ids, latitude=lats, longitude=lons)
    if (class(df.sp) == 'data.frame'){
      coordinates( df.sp ) = c( "longitude", "latitude" )
      id.list = sp::split( df.sp, df.sp[["id"]] )
      id = 1
      # For each id, create a line that connects all points with that id
      for ( i in id.list ) {
        event.lines = SpatialLines(list(Lines(Line(i[1]@coords), ID = id)),
                                   proj4string = CRS("+proj=longlat +datum=WGS84 +no_defs"))
        if (id == 1){
          sp_lines  = event.lines
        } else {
          sp_lines  = rbind(sp_lines, event.lines)
        }
        id = id + 1
      }
      sp_lines
    }else{print ('already a sp object')}
    leafletProxy('map') %>% addPolylines(data = sp_lines, weight = 1.8, opacity = 1, color = 'grey', group = '250m MODIS Grid') %>%
      addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
                       overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '500m Highlighted Pixels', '250m Highlighted Pixels', '250m MODIS Grid'),
                       position = c("topleft"),
                       options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup('500m Highlighted Pixels') %>%
      showGroup('500m Highlighted Pixels') %>%
      hideGroup('250m Highlighted Pixels') %>%
      showGroup('250m Highlighted Pixels')
    updateCheckboxInput(session, 'highlightPixelModeNDVI', value = TRUE)
  }
  
  
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

  # Converts the highlighted pixel coords into a spatialpolygon class
  matrix_to_polygon = function(matrix, id, type_){
    p   = Polygon(matrix)
    ps  = Polygons(list(p), ID = id)
    sps = SpatialPolygons(list(ps))
    proj4string(sps) = CRS("+proj=longlat +datum=WGS84")
    return (sps)
  }

  # Creates a polyline surrounding any MODIS 2016 500m pixel from cropped raster
  showpos = function(x=NULL, y=NULL, name, raster_, type_) { # type = '500m' or '250m'
    # If clicked within the Raster on the leaflet map
    r_ = raster_
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

       row = ceiling(cell / ncols)
       col = cell %% ncols

       if (col == 0){
         col = ncols
       }
       
       xclose = ((col - 1) * resolution) + xmin
       xfar   = (col * resolution) + xmin
       yclose = -((row - 1) * resolution) + ymax
       yfar   = -(row * resolution) + ymax

       midcellx = xclose + (resolution * .5)
       midcelly = yclose - (resolution * .5)
       midcell  = c(midcellx, midcelly)

       datalon = c(xclose, xfar, xfar, xclose ,xclose)
       datalat = c(yclose, yclose, yfar, yfar, yclose)
       id_     = paste0(name, '_', row, '_', col, '_', vegindex)

       # Check to see if already drawn, and if so remove it from df and leaflet map
       if (id_ %in% data$pixel_df$Id){
         remove_polyline(id = id_, all = FALSE)
         data$pixel_df = subset(data$pixel_df, Id!=id_)
         
         if (type_ == '500m'){
           # Remove polygon from data$pixel_sps_500m
           ids_500m = unique(ggplot2::fortify(data$pixel_sps_500m)$id)
           len = length(ids_500m)
           lst = c(1:len)
           pos = which(unique(ggplot2::fortify(data$pixel_sps_500m)$id) %in% c(id_))
           lst_ = lst[-(pos)]
           data$pixel_sps_500m = data$pixel_sps_500m[lst_]
         }else if (type_ == '250m'){
           # Remove polygon from data$pixel_sps_250m
           ids_250m = unique(ggplot2::fortify(data$pixel_sps_250m)$id)
           len = length(ids_250m)
           lst = c(1:len)
           pos = which(unique(ggplot2::fortify(data$pixel_sps_250m)$id) %in% c(id_))
           lst_ = lst[-(pos)]
           data$pixel_sps_250m = data$pixel_sps_250m[lst_]
         }
         print ('Dataframe of all highlighted pixels (250m + 500m)')
         print (data$pixel_df)
         
       }else{
         # Draw the pixel polygon on the leaflet map
         if (input$highlightPixelMode){
           leafletProxy("map") %>%
             addPolygons(datalon, datalat, layerId = id_, weight = 4, opacity = .95, color = 'red', group = '500m Highlighted Pixels', fillOpacity = .1)
         }else if (input$highlightPixelModeNDVI){
           leafletProxy("map") %>%
             addPolygons(datalon, datalat, layerId = id_, weight = 4,  opacity = .95, color = 'blue', group = '250m Highlighted Pixels', fillOpacity = .1)
         }

         ps = paste0('--Cell Id: ', id_, ' --Cell # in Landcover: ', cell,
                     ' --Row: ', row, ' --Column: ', col, ' --Pft Number: ', vegindex,
                     ' --Middle of Cell lon: ', midcell[1], ' lat: ', midcell[2])
         print (ps)

         # Build Dataframe   reactive value = data$pixel_df
         data$pixel_df = rbind(data$pixel_df, data.frame(Id = id_, Site = name, Type = type_, Lat = midcelly, Lon = midcellx, pft = vegindex, 
                                                         pt1_lat = datalat[1], pt1_lon = datalon[1],
                                                         pt2_lat = datalat[2], pt2_lon = datalon[2],
                                                         pt3_lat = datalat[3], pt3_lon = datalon[3],
                                                         pt4_lat = datalat[4], pt4_lon = datalon[4],
                                                         pt5_lat = datalat[5], pt5_lon = datalon[5]))
         
         pixel  = matrix_to_polygon(rbind(c(datalon[1], datalat[1]),
                                          c(datalon[2], datalat[2]), 
                                          c(datalon[3], datalat[3]),
                                          c(datalon[4], datalat[4]),
                                          c(datalon[5], datalat[5])), id_, as.character(type_))
         
         if (type_ == '500m'){
             if (length(data$pixel_sps_500m) == 0){
               data$pixel_sps_500m = pixel
             }else{
               data$pixel_sps_500m = rbind(data$pixel_sps_500m, pixel)
               
           }}else if(type_ == '250m'){
             if (length(data$pixel_sps_250m) == 0){
               data$pixel_sps_250m = pixel
             }else{
               data$pixel_sps_250m = rbind(data$pixel_sps_250m, pixel)
             }}
         
         print ('500m Grid Sp Object info:')
         print ((data$pixel_sps_500m))
         print ('250m Grid Sp Object info:')
         print ((data$pixel_sps_250m))
   
         print ('Dataframe of all highlighted pixels (250m + 500m)')
         print (data$pixel_df)
       }
     }
  }

  
  # Creates boundary box for clipping rasters using lat/lon from phenocam site
  crop_raster = function(lat_, lon_, r_, reclassify=FALSE, primary=NULL){
    height = .03
    width  = .05
    e      = as(extent(lon_-width, lon_ + width, lat_ - height, lat_ + height), 'SpatialPolygons')

    crs(e) <- "+proj=longlat +datum=WGS84 +no_defs"

    r      = raster::crop(r_, e, snap='near')
    
    if (reclassify == FALSE){
      return (r)

    }else if (reclassify == TRUE){

      water = 17*2

      m = c(1,2,
            2,2,
            3,2,
            4,2,
            5,2,
            6,2,
            7,2,
            8,2,
            9,2,
            10,2,
            11,2,
            12,2,
            13,2,
            14,2,
            15,2,
            16,2,
            17,2)


      if(!is.null(primary)){
        prim    = primary*2
        m[prim] = 1
        }

      rclmat = matrix(m, ncol=2, byrow=TRUE)
      rc     = raster::reclassify(r, rclmat)
      if (length(unique(values(rc))) == 1){
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
        rclmat = matrix(m, ncol=2, byrow=TRUE)
        rc     = raster::reclassify(r, rclmat)
      }
      return (rc)
    }
  }


  # Grabs the list of 3_day csv data from phenocam website
  get_site_roi_3day_csvs = function(name){
    idx=is.element(roi_files$site, name)
    num_rois=length(idx[idx==TRUE])
    loc_rois=which(idx==TRUE)
    csv=data.frame()
    if(num_rois==1) {
      df=data.table::fread(roi_files$one_day_summary[idx])
      csv=smooth_ts(df,metrics = c("gcc_mean","gcc_50", "gcc_75","gcc_90"),force = TRUE, 1)} else {
        for(i in loc_rois){
          df=data.table::fread(roi_files$one_day_summary[i])
          c=smooth_ts(df,metrics = c("gcc_mean","gcc_50", "gcc_75","gcc_90"),force = TRUE, 1)
          csv=rbind(csv, c)}}
      
    return(csv)
    # url  = paste('https://phenocam.sr.unh.edu/data/archive/', name, '/ROI/', sep = '')
    # page = read_html(url)
    # 
    # site_hrefs = page %>% html_nodes("a") %>% html_attr("href")
    # # csvs_    = site_hrefs[grep('3day.csv|1day.csv', site_hrefs)]  #How to grab both 1 and 3 day csvs
    # csvs_      = site_hrefs[grep('3day.csv|gcc90', site_hrefs)]
    # csvs_      = csvs_[grep('XX|.png', csvs_, invert=TRUE)]  #invert will take all strings without this
    # csv        = csvs_[grep('gcc90_3day', csvs_)]
    # csv        = csv[1]
    # 
    # csv_url = paste('https://phenocam.sr.unh.edu/data/archive/', name, '/ROI/', csv, sep = '')
    # print (csv_url)
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
      addCircleMarkers(~Lon, ~Lat, label=~Sitename, layerId=~Sitename, labelOptions = labelOptions(noHide = F, direction = "bottom",
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
    lat =  site_data_$Lat
    lon =  site_data_$Lon
    dst = sqrt(los**2 + los**2)
    c   = rotate_pt(lon, lat, (azm_-25), dst)
    b   = rotate_pt(lon, lat, (azm_+25), dst)
    cx  = c[[1]]
    cy  = c[[2]]
    bx  = b[[1]]
    by  = b[[2]]

    datalon = c(lon,cx,bx,lon)
    datalat = c(lat,cy,by,lat)
    camera  = site_data_$Sitename
    id_     = paste('fov',camera, sep='')
    add_polyline(datalon, datalat, id_ = 'azm_', .45, 'red', group = 'azm_')
  }


  # Add a polyline layer to the map
  add_polyline = function(datalon_, datalat_, id_, opacity_, color_='red', group_=NULL){
    leafletProxy("map") %>%
      addPolylines( datalon_,
                    datalat_,
                    layerId = id_,
                    opacity = opacity_,
                    color   = color_,
                    group   = group_)
  }


  add_polygon = function(datalon_, datalat_, id_, opacity_, color_='red', group_ = NULL){
    leafletProxy("map") %>%
      addPolygons( datalon_,
                    datalat_,
                    layerId = id_,
                    opacity = opacity_,
                    color   = color_,
                    group   = group_)
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
    lat                = site_data$Lat
    lon                = site_data$Lon
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
        # Can add differen't markers when we zoom in at some point, but for now we will use these circle markers from above
        addCircleMarkers(lng=lon,lat=lat,label=camera, layerId=camera, labelOptions = labelOptions(noHide = F, direction = "bottom",
                         style = get_marker_style()), opacity = .80, fillColor = getColor(cams=site_data), color = getColor(cams=site_data),
                         radius = 10, fillOpacity = .20, weight=3.5) %>%
        setView(lng = lon, lat = lat, zoom = 13)
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
                             camera_orientation_, degrees_,
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
    site_data = subset(cams_, Sitename == site_name)
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
      if(active == 'TRUE') {
        "blue"
      } else if(active == 'FALSE') {
        "red"
      } else {
        "orange"
      } })
  }

  switch_to_explorer_panel = function(){
    # Ids to show:
    data$pixel_df    = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Id", "Site", "Lat", 'Lon', 'pft'))
    data$pixel_sps_500m = SpatialPolygons(list())
    data$pixel_sps_250m = SpatialPolygons(list())
    
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
    shinyjs::hide(id = 'plotRemoteData')
    shinyjs::hide(id = 'pftSelection')
    shinyjs::hide(id = 'showHidePlot')
    shinyjs::hide(id = 'modisLegend')
    shinyjs::hide(id = 'plotpanel')
    shinyjs::hide(id = 'highlightPixelMode')
    shinyjs::hide(id = 'highlightPixelModeNDVI')
    shinyjs::hide(id = 'plotPixelsNDVI')
    shinyjs::hide(id = 'getData')
    leafletProxy('map') %>%
      clearControls() %>%
      clearShapes() %>%
      clearImages() %>%
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
    shinyjs::show(id = 'highlightPixelMode')
    shinyjs::show(id = 'getData')
    # Ids to hide:
    shinyjs::hide(id = 'explorerTitle')
    shinyjs::hide(id = 'usZoom')
    shinyjs::hide(id = 'showSites')
    shinyjs::hide(id = 'analyzerMode')
    shinyjs::hide(id = 'filterSites')
    shinyjs::hide(id = 'site')
    shinyjs::hide(id = 'siteZoom')
    shinyjs::hide(id = 'showHidePlot')
    shinyjs::hide(id = 'plotRemoteData')
    shinyjs::hide(id = 'doneGetData')
    
    updateCheckboxInput(session, 'highlightPixelMode', value = TRUE)
  }

  # Returns site name from a cached task
  # name_length :  the length of most task names submitted.  kelloggswitchgrass_09_06_18_0839 would be name_length = 5
  get_site_from_task = function(task_name_, name_length){
    elements = strsplit(task_name_, split = '_', fixed=TRUE)
    element_length = length(elements[[1]])
    if (element_length == name_length){
      site_name_ = elements[[1]][1]
      return (site_name_)
    }else if(element_length > name_length){
      num = element_length - name_length
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
  get_appeears_task = function(name, type){
    if (type == 'ndvi'){
      task_pos = grep(name ,appeears_tasks$task_name)
      for (i in c(1:length(task_pos))){
        row = get_site_from_task(appeears_tasks[task_pos[i],]$task_name, 5)
        if (row == name){
          task_ = appeears_tasks[task_pos[i],]$task_name
          return (subset(appeears_tasks, appeears_tasks$task_name == task_))
        }
      }
    }else if (type == 'tds'){
      task_pos = grep(name, appeears_tasks_tds$task_name)
      for (i in c(1:length(task_pos))){
        row = get_site_from_task(appeears_tasks_tds[task_pos[i],]$task_name, 3)
        if (row == name){
          task_ = appeears_tasks_tds[task_pos[i],]$task_name
          return (subset(appeears_tasks_tds, appeears_tasks_tds$task_name == task_))
        }
      }
    }else {print ('failed to ')}
  }


  # Downloads file from bundle (whichever ft is set to (only nc and qa_csv))
  download_bundle_file = function(site_task_id_, ft){
    print (site_task_id_)
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
    print (filepath)
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
