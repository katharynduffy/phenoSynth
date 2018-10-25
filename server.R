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
    data$pixel_df    = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Id", "Site", "Lat", 'Lon', 'pft'))
    data$pixel_sps_500m = SpatialPolygons(list())
    data$pixel_sps_250m = SpatialPolygons(list())
    panel$mode = 'explorer'
    data$df = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('Name', 'Longitude', 'Latitude', 'LeafletId'))
  })

  #--------------------------------------------------------------------------------------------------------------------------------------
  #  OBSERVERS
  #--------------------------------------------------------------------------------------------------------------------------------------

  # Turns ROI off if drawImage is off
  observe({
    checked = input$drawImage
    if (checked == FALSE){
      updateCheckboxInput(session, inputId = 'drawImageROI', value = FALSE)
    }
  })


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
      build_polygon_table(data$df)

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
    build_polygon_table(data$df)

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
    build_polygon_table(data$df)


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
    show_all_sites(map_ = 'map', data_ = variables$sites_df)
  })


  # BUTTON
  # Zooms to the selected site in the Sites dropdown option with BUTTON
  observeEvent(input$siteZoom, {
    print('Running BUTTON Zoom to Selected Site')
    site       = isolate(input$site)
    site_data  = get_site_info(site_)
    zoom_to_site(site, site_data, zoom=TRUE, cams_, input$drawROI)


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
      show_all_sites(map_ = 'map', data_ = variables$sites_df)
      count()
  })


  # Add All sites back to map
  observeEvent(input$showSites, {
    showAll = input$showSites
    # variables$sites_df = cams_
    print('Running add All sites back to map')
    show_all_sites(map_ = 'map', data_ = variables$sites_df)
    count()
  })


  # Observer for the azm changes from 0-360 on the slider
  observeEvent(input$azm, {
    azm = input$azm
    if(is.null(azm))
      return()
    isolate({
      if (input$drawROI == TRUE){
        site      = input$site
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
    event     = isolate(input$map_marker_click)
    
    updateSelectInput(session, 'site', selected = event$id )
    print (event$id)
    site      = event$id
    site_data = get_site_info(site)

    if (is_not_null(event$id)){
      if (isolate(input$map_zoom) < 5){
        zoom_to_site(event$id, site_data, zoom=TRUE, cams_, input$drawROI)
      }
    }
    if(is.null(event))
      return()

    isolate({
      leafletProxy("map", data = variables$sites_df) %>% clearPopups()
      
      site = event$id
      lat             = site_data$Lat
      lon             = site_data$Lon
      description     = site_data$site_description
      elevation       = site_data$Elev
      camera          = site_data$Sitename
      site_type       = site_data$site_type
      cam_orientation = as.character(site_data$camera_orientation)
      degrees         = as.numeric(orientation_key[cam_orientation])
      active          = site_data$active
      date_end        = site_data$date_last
      date_start      = site_data$date_first
      get_site_popup(camera, lat, lon, description, elevation, 
                     site_type, cam_orientation, degrees,
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
  

  # Button switches to Analyzer Mode
  observeEvent(input$analyzerMode,{
    panel$mode = 'analyzer'
    site       = input$site
    site_data  = get_site_info(site)

    data$global_pth = './www/global_landcover_2016.tif'
    global_r   = raster::raster(data$global_pth)

    veg_types  = c()
    print ('Switching to Analyze Mode')
    zoom_to_site(site, site_data, TRUE, cams_, input$drawROI)
    highlighted$group = paste0(site, ' Highlighted Pixels')

    output$analyzerTitle = renderText({paste0('Site:: ', site)})
    switch_to_analyzer_panel()
    updateCheckboxInput(session, 'highlightPixelMode', value = TRUE)

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
        # veg_types = c(veg_types, paste0(add_veg, '_', i))
        veg_types = c(veg_types, add_veg)
      }
      veg_types = unique(veg_types)
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

        addLegend(labels = c3$names, colors = c3$colors, position = "bottomleft", opacity = .95, title = 'MODIS Landcover') %>%
        addLegend(values = c(1,2), position = 'bottomright', title = 'Vegetation Cover Agreement',
                  colors = c('green', 'grey'), labels = c('ROI-Match', 'No-Match')) %>%
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
    data$pixel_df       = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Id", "Site", "Lat", 'Lon', 'pft'))
    data$pixel_sps_500m = SpatialPolygons(list())
    data$pixel_sps_250m = SpatialPolygons(list())
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

      nc_ndvi = data$ndvi_nc

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
            incProgress((1/len)/1.1)
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
          variables_ = c()
          for (i in c(1:polys_len)){
            col = paste0('pixel_', as.character(i))
            cols = c(cols, col)
            a = c(1:(length(ndvis_)))
            b = a[seq((1 + (i-1)), length(a), polys_len)]
            print (col)
            print (b)
            list_[[col]] = ndvis_[b]
            var_ = paste0('pixel_', i)
            variables_ = c(variables_, var_)
          }
          print (polys_len)

          # Grab GCC

          csv = get_site_roi_3day_csvs(name       = site, 
                                       roi_files_ = roi_files)
          
          pData=csv%>%dplyr::select('date', 'year', 'doy', 'gcc_mean', 'smooth_gcc_mean')
          source= rep('PhenoCam GCC', nrow(pData))
          variable= rep('PhenoCam', nrow(pData)) #this is new so that it plots
          pData=cbind(pData, source, variable)
          colnames(pData)=c('date', 'year', 'doy', 'gcc_mean','value', 'source','variable')
          pData$date=as.Date(pData$date)


          #Parse data with Dates
          data_df = data.frame(date = date_list, list_)
          start_ = input$dataDateRange[1]
          end_   = input$dataDateRange[2]

          parsed_data = subset(data_df, date >= start_ & date <= end_)
          parsed_data$date=as.Date(parsed_data$date)
          source      = rep('MODIS NDVI', nrow(parsed_data))
          parsed_data   = cbind(parsed_data, source)
          parsed_data_melt = melt(data.table(parsed_data), measure.vars = variables_)

          # incProgress(.1)
          # combine GCC and NDVI dfs
          
          all_data=full_join(parsed_data_melt, pData)

          p = ggplot(data = all_data, aes(x= date, y=value, color=variable)) +
            geom_line()+
            scale_colour_brewer(palette="Set1") + facet_wrap(~source, ncol=1, scales='free_y') 
          p + theme_minimal()

        })
      }
    #----PHENOCAM------------------------------------------------------------------------------------------------------------------------
    }else if(data_type_selected == 'PhenoCam GCC'){

        phenoCamData = get_site_roi_3day_csvs(name       = site, 
                                              roi_files_ = roi_files)
        parsed_data  = cbind(parsed_data, source)
         ##pickup here
        phenoCamData$date = as.Date(phenoCamData$date)
        source = rep('PhenoCam', nrow(phenoCamData))
        #pvars=c('date', 'year', 'doy', 'gcc_mean', 'smooth_gcc_mean')
        pData=phenoCamData%>%dplyr::select('date', 'year', 'doy', 'gcc_mean', 'smooth_gcc_mean')
        pData=cbind(pData, source)
        sDF=left_join(parsed_data, pData)#pick up here
      # })

    }else if(data_type_selected == 'MODIS EVI'){
      print ('Pretending to plot modis EVI')
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
      v6_QA   = ncvar_get(ndvi_output, "_250m_16_days_VI_Quality")

      data$site_nc = ndvi_output

      # Set lat and lon arrays for NDVI data
      lat_NDVI = ncvar_get(ndvi_output, "lat")
      lon_NDVI = ncvar_get(ndvi_output, "lon")
      
      ### Test section for Different PROJECTIONS
      YlGn = brewer.pal(9, "YlGn")
      newproj = '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs'
      test_raster = raster(t(v6_NDVI[,,1]), xmn=min(lon_NDVI), xmx=max(lon_NDVI), ymn=min(lat_NDVI), ymx=max(lat_NDVI), crs=crs)
      # test_raster = projectRaster(test_raster, crs = newproj, res = 257.5014113)
      # leafletProxy('map') %>% addRasterImage(test_raster, opacity = .7, group = 'test1', col = YlGn)
      ### End Test Section

      
      # Grab the fill value and set to NA
      # incProgress(.1)
      fillvalue = ncatt_get(ndvi_output, "_250m_16_days_NDVI", "_FillValue")
      incProgress(.1)
      v6_NDVI[v6_NDVI == fillvalue$value] = NA
      
      # idx=v6_QA!=0
      # v6_NDVI[idx]=NaN #filtering out poor VI quality values
      data$ndvi_nc = v6_NDVI

      # Define the coordinate referense system proj.4 string
      crs = CRS("+proj=longlat +datum=WGS84")

      # Grab first observation of NDVI and Quality datasets
      v6_NDVI = raster(t(v6_NDVI[,,1]), xmn=min(lon_NDVI), xmx=max(lon_NDVI), ymn=min(lat_NDVI), ymx=max(lat_NDVI), crs=crs)
      v6_NDVI_original = v6_NDVI
      # v6_QA = raster(t(v6_QA[,,1]), xmn=min(lon_NDVI), xmx=max(lon_NDVI), ymn=min(lat_NDVI), ymx=max(lat_NDVI), crs=crs)
      #------------------------------------------------------------------------

      data$r_ndvi_cropped = crop_raster(site_data$Lat, site_data$Lon, v6_NDVI)

      # extracted_ndvi = extract(v6_NDVI_original, data$pixel_sps_250, snap = 'in')
      # print (extracted_ndvi)
      
      shinyjs::show(id = 'highlightPixelModeNDVI')

      # Build grid
      build_raster_grid(data$r_ndvi_cropped, map = 'map')
      updateCheckboxInput(session, 'highlightPixelModeNDVI', value = TRUE)
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

  # Creates a polyline surrounding any MODIS 2016 500m pixel from cropped raster
  showpos = function(x=NULL, y=NULL, name, raster_, type_, map_ = NULL) { # type = '500m' or '250m'
    # If clicked within the Raster on the leaflet map
    r_ = raster_
    cell = cellFromXY(r_, c(x, y))

     if (!is.na(cell)) {

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
                                    # crs = '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs')
         
         # pixel = spTransform(pixel , CRS("+proj=longlat +datum=WGS84"))
         
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


  # Get specific site data and returns lon/lat/camera/description/elevation
  get_site_info = function(site_name){
    site_data = subset(cams_, Sitename == site_name)
    return (site_data)
  }

  # Add reactive counter to help control events at startup of script
  count = function(){
    isolate({
      counter$countervalue = counter$countervalue + 1
    })
  }


}
