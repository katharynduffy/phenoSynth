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

  phenocam    = reactiveValues()

  panel   = reactiveValues(mode = '')

  counter = reactiveValues(countervalue = 0)

  modis   = reactiveValues(data = data.frame(),
                           cached_ndvi = list())

  data    = reactiveValues(
                      draw_mode = FALSE,
                      run   = 0,
                      names = c(),
                      df    = data.frame(),
                      all_data = data.frame(),
                      veg_types = c(),
                      pixel_sps = SpatialPolygons(list()),
                      pixel_sps_500m = SpatialPolygons(list()),
                      pixel_sps_250m = SpatialPolygons(list()))

  # Empty reactive spdf
  value = reactiveValues(drawnPoly = SpatialPolygonsDataFrame(SpatialPolygons(list()),
                                                              data=data.frame()))

  output$phenoTable <- function() {
      cams_ %>%
      kable() %>%
      kable_styling(bootstrap_options = c("striped", "hover", "condensed"))
  }
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  OUTPUTS
  #--------------------------------------------------------------------------------------------------------------------------------------

  # Create the map
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

  # Open landing page and initialze application
  observe({ 
    showModal(tags$div(id = 'frontPage', modalDialog(
      tags$div(id = 'row1'),
      h1('Welcome to Phenosynth'),
      fluidRow(
        column(6, align='center', offset = 3,
               p('PhenoSynth is an open-repository Shiny(R) interface that addresses these factors and allows users to visualize and interact with phenological data across multiple sources including MODIS and eventually LandSat. This tool provides an interface to investigate ‘apples-to-apples’ overlap in vegetation classification, and evaluate agreement in phenological indices and time series across observational datasets, facilitating the scaling of phenological data to regional and continental levels.'))),
      tags$div(id = 'frontPageData',
               fluidRow(
                 column(12, align="center", offset = 0,
                        selectInput('frontPageDataSelection', 'Choose Your Data (click in box) - Phenocam is always included', multiple = TRUE, 
                                    c('Modis Ndvi', 'Landsat Phenometrics')),
                        tags$style(type='text/css', "#frontPageData { vertical-align: middle; height: 50px; width: 100%; font-size: 15px;}"))
               )),
      fluidRow(
        column(4, align='center', offset = 4,
               p('Once you have your choices pop up in the input above, press the button below to enter into the Shiny Application Interface'))),
      footer = modalButton('Enter Phenosynth with Seletected Data')
    )))
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
  
  # hides frequency UI element if GCC isn't selected for data to download/get
  observe({
    data = input$dataTypes_get
    if ('GCC' %in% data){
      shinyjs::show(id = 'phenocamFrequency')
    } else{
      shinyjs::hide(id = 'phenocamFrequency')}
  })


  # Start of Drawing - set highlight pixel to off
  observeEvent(input$map_draw_start, {
    data$draw_mode = TRUE
    print ('Starting Draw Mode')
  })

  # Clears plot
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
    site_data  = get_site_info(site)
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
    data$all_data = data.frame()

    data$global_pth = './www/global_landcover_2016.tif'
    global_r   = raster::raster(data$global_pth)

    veg_types  = c()
    print ('Switching to Analyze Mode')
    zoom_to_site(site, site_data, TRUE, cams_, input$drawROI)
    highlighted$group = paste0(site, ' Highlighted Pixels')

    output$analyzerTitle = renderText({paste0('Site:: ', site)})
    switch_to_analyzer_panel()
    # updateCheckboxInput(session, 'highlightPixelMode', value = TRUE)

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
    }
  })

  # When ROI Vegetation type changes re-plot highlighted veg type
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

  # Button that plots selected Data Types
  observeEvent(input$plotDataButton, {
    shinyBS::toggleModal(session, 'plotDataPopup', toggle = 'close')
    
    # Selected pixels
    sm_pixels = data$pixel_sps_250m
    lg_pixels = data$pixel_sps_500m
    
    print (sm_pixels)
    print (lg_pixels)

    # Inputs from popup
    selected_data = input$dataTypes_plot
    selected_pixel_type = input$pixelTypes
    
    print (paste0('Plotting: ', selected_data))
    
    
    # ------------------PLOT GCC------------------------------------
    if ('GCC' %in% selected_data){
      withProgress(message = 'Building GCC Plot', value = .1, {
      print ('Plotting GCC')
      gcc_p = gcc_plot(phenocam$gcc, phenocam$spring, phenocam$fall)
      }) #END WITH PROGRESS BAR
    } #END GCC PLOT
    
    # ------------------PLOT NPN------------------------------------
    if ('NPN' %in% selected_data){
      print ('Plotting NPN')
      ############################
      # What does this look like #
      #     ?????????????????    #
      ############################
    } #END NPN PLOT
    
    # --------------- TRANSITION DATE EXTRACTION FOR PIXELS ------------
    if ('Transition Dates' %in% selected_data){
      withProgress(message = 'Compiling Transition Dates', value = .1, {
      # Extracting lat/lng values for selected 250m or 500m pixels
      if(selected_pixel_type == '250m'){
        pixels = subset(data$pixel_df, data$pixel_df$Type == '250m')
      }else if(selected_pixel_type =='500m'){
        pixels = subset(data$pixel_df, data$pixel_df$Type == '500m')
      }
      # Inputs of pixels required for get_tds_modis_df function to extract Transition date data
      lats = pixels$Lat
      lngs = pixels$Lon
      
      tds_modis_df = get_tds_modis_df(lats, lngs, data$tds_nc,progress_bar=TRUE)
      # Transition date data under selected pixels
      OGD      = subset(tds_modis_df, tds_modis_df$layer == 'Onset_Greenness_Decrease')
      OGI      = subset(tds_modis_df, tds_modis_df$layer == 'Onset_Greenness_Increase')
      OGMa     = subset(tds_modis_df, tds_modis_df$layer == 'Onset_Greenness_Maximum')
      OGMi     = subset(tds_modis_df, tds_modis_df$layer == 'Onset_Greenness_Minimum')
      }) #END WITH PROGRESS BAR
    } # END TRANSITION DATE EXTRATION FOR PIXELS
    
    # ------------------PLOT NDVI------------------------------------
    if ('NDVI' %in% selected_data){
      withProgress(message = 'Building NDVI Plot', value = .1, {
      print ('Plotting NDVI')
    
      if (is.null(sm_pixels@polygons[1][[1]]) & is.null(lg_pixels@polygons[1][[1]])){
        print ('No pixels selected')
        ndvi_p = plot_ly()
      }else{

        if(selected_pixel_type == '250m'){
          ndvi_brick = data$ndvi_brick
          ndvi_under_pixel = extract(ndvi_brick, sm_pixels)
          cs  = get_custom_color_list(length(ndvi_under_pixel))
          ndvi_p = plot_ly()
          
          for (num in c(1:length(ndvi_under_pixel))){
            incProgress(amount = (1/length(ndvi_under_pixel))*.8)
            px = ndvi_under_pixel[[num]][1,]
            dates = as.Date(names(px),format='X%Y.%m.%d')
            ndvi_brick_df = data.frame(date = dates, pixel = px)
            
            # add smoothing function
            
            ndvi_p = ndvi_p %>%
              add_markers(
              data = ndvi_brick_df,
              x = ~ date,
              y = ~ pixel,
              showlegend = TRUE,
              type = 'scatter',
              mode = 'markers',
              name = paste0(num,'_px_NDVI_250m'),
              marker = list(color = cs[num])
            )
          }
        } else if (selected_pixel_type == '500m'){
          ndvi_brick = data$ndvi_brick
          ndvi_under_pixel = extract(ndvi_brick, lg_pixels)
          values_in_pixel = dim(ndvi_under_pixel[[1]])[[1]]
          cs  = get_custom_color_list(length(ndvi_under_pixel)*values_in_pixel)
          print (cs)
          ndvi_p = plot_ly()
          
          for (num in c(1:length(ndvi_under_pixel))){
            
            for (num_quad in c(1:values_in_pixel)){
              px = ndvi_under_pixel[[num]][num_quad,]
              dates = as.Date(names(px),format='X%Y.%m.%d')
              ndvi_brick_df = data.frame(date = dates, pixel = px)
              
              letter = c('A', 'B', 'C', 'D')[num_quad]
              ndvi_p = ndvi_p %>%
                add_markers(
                  data = ndvi_brick_df,
                  x = ~ date,
                  y = ~ pixel,
                  showlegend = TRUE,
                  type = 'scatter',
                  mode = 'markers',
                  name = paste0(num,'_',letter,'_px_NDVI_500m'),
                  marker = list(color = cs[((num-1)*4)+num_quad])
                )
              }
            }
          }
        }
        })# END WITH PROGRESS BAR
      } #END NDVI PLOT
    
    # ------------------PLOT EVI------------------------------------
    if ('EVI' %in% selected_data){
      withProgress(message = 'Building EVI Plot', value = .1, {
      print ('Plotting EVI')
      selected_pixel_type = input$pixelTypes
      
      if (is.null(sm_pixels@polygons[1][[1]]) & is.null(lg_pixels@polygons[1][[1]])){
        print ('No pixels selected')
        evi_p = plot_ly()
      }else{
      
      if(selected_pixel_type == '250m'){
        evi_brick = data$evi_brick
        evi_under_pixel = extract(evi_brick, sm_pixels)
        cs  = get_custom_color_list(length(evi_under_pixel))
        
        evi_p = plot_ly()
        for (num in c(1:length(evi_under_pixel))){
          incProgress(amount = (1/length(evi_under_pixel))*.8)
          px = evi_under_pixel[[num]][1,]
          dates = as.Date(names(px),format='X%Y.%m.%d')
          evi_brick_df = data.frame(date = dates, pixel = px)
          # add smoothing
          
          evi_p = evi_p %>%
            add_markers(
              data = evi_brick_df,
              x = ~ date,
              y = ~ pixel,
              showlegend = TRUE,
              type = 'scatter',
              mode = 'markers',
              name = paste0(num,'_px_EVI_250m'),
              marker = list(color = cs[num])
            ) 
          # Add transition date points to pixels if Transition dates are selected
          if ('Transition Dates' %in% selected_data){
            evi_p = evi_p %>% 
              add_markers(
                data = subset(OGMa,OGMa$pixel==num),
                x = ~ dates,
                y = ~ value,
                showlegend = TRUE,
                type = 'scatter',
                mode = 'markers',
                marker = list(color = cs[num], size= 10, symbol=2),
                name = paste0(num,'_px_Onset_Greenness_Maximum') 
              ) %>% 
              add_markers(
                data = subset(OGMi,OGMi$pixel==num),
                x = ~ dates,
                y = ~ value,
                showlegend = TRUE,
                type = 'scatter',
                mode = 'markers',
                marker = list(color = cs[num], size= 10, symbol=25),
                name = paste0(num,'_px_Onset_Greenness_Minimum') 
              ) %>% 
              add_markers(
                data = subset(OGI,OGI$pixel==num),
                x = ~ dates,
                y = .25,
                showlegend = TRUE,
                type = 'scatter',
                mode = 'markers',
                marker = list(color = cs[num], size= 10, symbol=5),
                name = paste0(num,'_px_Onset_Greenness_Increase') 
              ) %>% 
              add_markers(
                data = subset(OGD,OGD$pixel==num),
                x = ~ dates,
                y = .25,
                showlegend = TRUE,
                type = 'scatter',
                mode = 'markers',
                marker = list(color = cs[num], size= 10, symbol=6),
                name = paste0(num,'_px_Onset_Greenness_Decrease') 
              )
          }
        } #END 250M LOOP
      } else if (selected_pixel_type == '500m'){
        evi_brick = data$evi_brick
        evi_under_pixel = extract(evi_brick, lg_pixels)
        values_in_pixel = dim(evi_under_pixel[[1]])[[1]]
        cs  = get_custom_color_list(length(evi_under_pixel)*values_in_pixel)
        evi_p = plot_ly()
        
        for (num in c(1:length(evi_under_pixel))){
          for (num_quad in c(1:values_in_pixel)){
            px = evi_under_pixel[[num]][num_quad,]
            dates = as.Date(names(px),format='X%Y.%m.%d')
            evi_brick_df = data.frame(date = dates, pixel = px)
            
            letter = c('A', 'B', 'C', 'D')[num_quad]
            evi_p = evi_p %>%
              add_markers(
                data = evi_brick_df,
                x = ~ date,
                y = ~ pixel,
                showlegend = TRUE,
                type = 'scatter',
                mode = 'markers',
                name = paste0(num,'_',letter,'_px_EVI_500m'),
                marker = list(color = cs[((num-1)*4)+num_quad]))
          }
        }
      } #END 500M LOOP
      }
      print ('END EVI PLOT')
      }) #END WITH PROGRESS BAR
  } #END EVI PLOT


    # ------------------COMPILING PLOT ------------------------------------
    if ( 'NDVI' %in% selected_data |'EVI' %in% selected_data | 'GCC' %in% selected_data | 'Transition Dates' %in% selected_data){
      withProgress(message = 'Compiling All Plots', value = .3, {
      print ('Compiling plot')
      print (selected_data)
      vector_length = length(selected_data)
      if ('Transition Dates' %in% selected_data){
        vector_length = vector_length - 1
      }
      if('NPN' %in% selected_data){
        vector_length = vector_length - 1
      }
      
      plot_list = vector('list', vector_length)
      print (plot_list)
      count = 0
      for (i in selected_data){
        count = count + 1
        print (i)
        if (i == 'GCC'){
          plot_list[[count]] = gcc_p
        }
        if (i =='NDVI'){
          plot_list[[count]] = ndvi_p
        }
        if (i == 'EVI'){
          plot_list[[count]] = evi_p
        }
      }
      
      print ('Compiling intermediate plot')
      intermediate_p = subplot(plot_list, nrows = length(plot_list), shareX = TRUE) %>%
        layout(title='PhenoSynth Plots')
      
      print ('Compiling final plot')
      # Removing unwanted mode bar options on plotly plot
      final_plot = intermediate_p %>% config(displaylogo = FALSE,
                                 modeBarButtonsToRemove = list(
                                   'sendDataToCloud',
                                   'autoScale2d',
                                   'resetScale2d',
                                   'hoverClosestCartesian',
                                   'hoverCompareCartesian',
                                   'toggleSpikelines'
                                 )) %>%
        layout(height = 250* vector_length, width = 1300, inline = TRUE)
      
      # add table for npn gridded tds
      if(selected_pixel_type == '250m'){
        plotTable = subset(data$pixel_df, data$pixel_df$Type == '250m')
      }else if(selected_pixel_type =='500m'){
        plotTable = subset(data$pixel_df, data$pixel_df$Type == '500m')
      }
      output$plotTable <- DT::renderDataTable(
        plotTable %>% select(Site, Type, Lat, Lon),
        filter = 'top',
        options = list(autoWidth = TRUE, scrollY = TRUE)
      )
      
      print ('Rendering plotly')
      # Rendering plotly plot to UI called 'data_plot'
      output$data_plot = renderPlotly({
        final_plot
      })
      
      print (paste0(selected_data, ' Plotting Completed'))
      updateTabsetPanel(session, 'navbar', selected = 'PlotPanel')
        }) #END WITH PROGRESS BAR
      }
  }) #END PLOTTING DATA OBSERVER



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
          if (dim(data$pixel_df)[1] == 0){
            shinyjs::hide(id = 'clearPixels')
          }else if (dim(data$pixel_df)[1] > 0){
            shinyjs::show(id = 'clearPixels')
          }
    })
  
  # Clear all pixels on map by clearing the selected pixel dataframe of 500m and 250m pixels
  observeEvent(input$clearPixels,{
    # Remove drawn pixels from map and then remove them from the dataframe (data$pixel_df)
    ids = data$pixel_df[,1]
    for (id_ in ids){
      remove_polyline(id = id_, all = FALSE)
    }
    data$pixel_df    = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Id", "Site", "Lat", 'Lon', 'pft'))
    data$pixel_sps_500m = SpatialPolygons(list())
    data$pixel_sps_250m = SpatialPolygons(list())
    shinyjs::hide(id = 'clearPixels')
  })


  # Download/get data for desired phenocam/satellite products
  observeEvent(input$getDataButton, {
    site           = input$site
    site_data      = get_site_info(site)
    selected_data  = input$dataTypes_get
    data_options   = c('NDVI', 'EVI', 'GCC', 'Transition Dates', 'NPN')
    
    file_path      = paste0('./www/site_data/', site, '/data_layers/')
    ndvi_filepath = paste0(file_path,'ndvi/')
    evi_filepath = paste0(file_path,'evi/')
    tds_filepath = paste0(file_path,'tds/')
    gcc_filepath = paste0(file_path,'gcc/')
    
    temp_nc_ndvi = './www/deleteme/ndvi/'
    
    freq           = as.numeric(substring(input$phenocamFrequency, 1, 1))
    percentile_gcc = 90 
    
    shinyjs::hide(id = 'getData')
    
    shinyBS::toggleModal(session, 'getDataPopup', toggle = 'close')

    print ('Importing data for:')
    print (selected_data)

    # Set up directories to store data if in Download local data mode
    main = './www/site_data'
    npn_grid_dir  = './www/npn_grid_data'
    if (!file.exists(main)){
      dir.create(file.path(main))
    }
    if (!file.exists(npn_grid_dir)){
      dir.create(npn_grid_dir)
    }
    main_site = paste0(main, '/', site)
    if (!file.exists(main_site)){
      dir.create(file.path(main_site))
    }
    if (!file.exists(file_path)){
      dir.create(file.path(file_path))
    }
    if (!file.exists(ndvi_filepath) & 'NDVI' %in% selected_data){
      print ('building ndvi folder')
      dir.create(file.path(ndvi_filepath))
    }
    if (!file.exists(evi_filepath) & 'EVI' %in% selected_data){
      dir.create(file.path(evi_filepath))
    }
    if (!file.exists(tds_filepath) & 'Transition Dates' %in% selected_data){
      dir.create(file.path(tds_filepath))
    }
    if (!file.exists(gcc_filepath) & 'GCC' %in% selected_data){
      dir.create(file.path(gcc_filepath))
    }
    
    
    
      # Import [NPN]
    if ('NPN' %in% selected_data){
      withProgress(message = 'Importing NPN', value = .1, {
        
      npn_file_name = paste0(npn_grid_dir, '/average_leaf_prism_brick.nc')
      if (length(list.files(npn_grid_dir))==0){
        setProgress(value = .2, detail = 'Downloading NPN')
        data$npn_brick = download_npn_brick(tmp_name = paste0(npn_grid_dir,'/deleteme_npn_grid_'),
                                            out_file = npn_file_name,
                                            layer    = 'si-x:average_leaf_prism')
      }else {
        setProgress(value = .1, detail = 'Importing NPN')
        data$npn_brick = raster::brick(npn_file_name)
      }
      
      }) #END WITH PROGRESS BAR
    } #END IMPORT NPN


      # Import [NDVI] 
      #------------------------------------------------------------------------
    if ('NDVI' %in% selected_data){
      withProgress(message = 'Importing NDVI', value = .1, {
        
      print ('Importing NDVI')
      appeears$ndvi  = get_appeears_task(site, type = 'ndvi')
      
      if (length(list.files(ndvi_filepath))==0){
        setProgress(value = .1, detail = 'Downloading NDVI')
        ndvi_bundle_df = download_bundle_file(appeears$ndvi$task_id, ndvi_filepath)
        setProgress(value = .8, detail = 'NDVI Downloaded')
      }else {
        setProgress(value = .1, detail = 'Importing NDVI')
        ndvi_bundle_df = get_appeears_bundle_df(appeears$ndvi$task_id)
        setProgress(value = .8, detail = 'NDVI Imported')
      }
      
      ndvi_name = subset(ndvi_bundle_df, file_type == 'nc')$file_name
      ndvi_path = paste0(ndvi_filepath, ndvi_name)
      
      ndvi_qc_name = ndvi_bundle_df[grep('Quality-lookup', ndvi_bundle_df$file_name),]$file_name
      ndvi_qc_path = paste0(ndvi_filepath, ndvi_qc_name)
      
      ndvi_brick    = raster::brick(ndvi_path, varname='_250m_16_days_NDVI')
      ndvi_qc_brick = raster::brick(ndvi_path, varname='_250m_16_days_VI_Quality')
      ndvi_qc_csv   = read.csv(ndvi_qc_path)

      # Add ndvi_nc file to memory
      data$ndvi_brick    = ndvi_brick
      data$ndvi_qc_brick = ndvi_qc_brick
      data$ndvi_qc_csv   = ndvi_qc_csv

      # Grab first observation of NDVI and Quality datasets
      raster_1_brick_ndvi = raster(subset(ndvi_brick, 1))
      data$r_ndvi_cropped = crop_raster(site_data$Lat, site_data$Lon, raster_1_brick_ndvi)
      build_raster_grid(data$r_ndvi_cropped, map = 'map')

      shinyjs::show(id = 'highlightPixelModeNDVI')
      shinyjs::show(id = 'highlightPixelMode')
      updateCheckboxInput(session, 'highlightPixelModeNDVI', value = TRUE)

      shinyjs::show(id = 'plotRemoteData')
      
      }) #END WITH PROGRESS BAR
    } #END IMPORT NDVI


      # Import [Transition Dates] netcdfs
      #------------------------------------------------------------------------
    if ('Transition Dates' %in% selected_data){
      withProgress(message = 'Importing Transition Dates', value = 0, {
      print ('Importing Transition Dates')
      appeears$tds  = get_appeears_task(site, type = 'tds')
      
      # if (input$localDownload){
        if (length(list.files(tds_filepath))==0){
          tds_bundle_df = download_bundle_file(appeears$tds$task_id, tds_filepath)
        }else {
          tds_bundle_df = get_appeears_bundle_df(appeears$tds$task_id)
          }
        
        tds_name = subset(tds_bundle_df, file_type == 'nc')$file_name
        tds_path = paste0(tds_filepath, tds_name)
        data$tds_nc    = nc_open(tds_path)
      # }
      shinyjs::show(id = 'plotRemoteData')
      }) #END WITH PROGRESS BAR
    } #END IMPORT TRANSITION DATES


    #   # Import [EVI] netcdf(evi) and csv(qa)
    #   #------------------------------------------------------------------------
    if ('EVI' %in% selected_data){
      withProgress(message = 'Importing NDVI', value = .1, {
      print ('Importing EVI')
      
      appeears$evi  = get_appeears_task(site, type = 'evi')
      
      # if (input$localDownload){
        if (length(list.files(evi_filepath))==0){
          setProgress(value = .1, detail = 'Downloading NDVI')
          evi_bundle_df = download_bundle_file(appeears$evi$task_id, evi_filepath)
          setProgress(value = .1, detail = 'NDVI Downloaded')
        }else {
          setProgress(value = .1, detail = 'Importing NDVI')
          evi_bundle_df = get_appeears_bundle_df(appeears$evi$task_id)
          setProgress(value = .1, detail = 'NDVI Imported')
          }
        print (evi_bundle_df)
        evi_name = subset(evi_bundle_df, file_type == 'nc')$file_name
        evi_path = paste0(evi_filepath, evi_name)
        
        evi_qc_name = evi_bundle_df[grep('Quality-lookup', evi_bundle_df$file_name),]$file_name
        evi_qc_path = paste0(evi_filepath, evi_qc_name)
        
        evi_brick    = raster::brick(evi_path, varname='_250m_16_days_EVI')
        evi_qc_brick = raster::brick(evi_path, varname='_250m_16_days_VI_Quality')
        evi_qc_csv   = read.csv(evi_qc_path)
      # }
      
      # Add evi_nc file to memory
      data$evi_brick    = evi_brick
      data$evi_qc_brick = evi_qc_brick
      data$evi_qc_csv   = evi_qc_csv
      
      if ('NDVI' %!in% selected_data){
        # Grab first observation of evi and Quality datasets
        raster_1_brick_evi = raster(subset(evi_brick, 1))
        data$r_evi_cropped = crop_raster(site_data$Lat, site_data$Lon, raster_1_brick_evi)
        build_raster_grid(data$r_evi_cropped, map = 'map')
      }
      
      shinyjs::show(id = 'highlightPixelModeNDVI')
      shinyjs::show(id = 'highlightPixelMode')
      updateCheckboxInput(session, 'highlightPixelModeNDVI', value = TRUE)
      
      shinyjs::show(id = 'plotRemoteData')
      }) #END WITH PROGRESS BAR
    } #END IMPORT EVI
    

    # Import [GCC] splined Data from phenocam (csv)
    #------------------------------------------------------------------------
    if ('GCC' %in% selected_data){
      withProgress(message = 'Importing GCC', value = .1, {
        
      file_path = gcc_filepath
      print ('Importing Phenocam GCC')
      pft_long = input$pftSelection
      pft = strsplit(pft_long, '_')[[1]][1]
      pft_abb = (subset(pft_df, pft_df$pft_expanded == pft)$pft_abbreviated)

      gcc_filepath    = paste0(file_path, 'gcc', '_',paste0(freq,'_day'), '.csv')
      spring_filepath = paste0(file_path, 'gcc', '_',paste0(freq,'_day_spring_tds'), '.csv')
      fall_filepath   = paste0(file_path, 'gcc', '_',paste0(freq,'_day_fall_tds'), '.csv')

      if (file.exists(gcc_filepath)){
        setProgress(value = .5, detail = 'Importing GCC CSV')
        phenocam$gcc    = read.csv(gcc_filepath, header = TRUE)
        phenocam$spring = read.csv(spring_filepath, header = TRUE)
        phenocam$fall   = read.csv(fall_filepath, header = TRUE)
      }else{
        setProgress(value = .2, detail = 'Downloading GCC data')
        phenocam$data = get_site_roi_csvs(name        = site,
                                          roi_files_  = roi_files,
                                          frequency_  = freq,
                                          percentile_ = percentile_gcc,
                                          roi_type_   = pft_abb)
        setProgress(value = .2, detail = 'GCC data Downloaded')
        
        phenocam$gcc    = phenocam$data[[1]]
        phenocam$spring = phenocam$data[[2]]
        phenocam$fall   = phenocam$data[[3]]
        
        write.csv(phenocam$gcc,    file = gcc_filepath)
        write.csv(phenocam$spring, file = spring_filepath)
        write.csv(phenocam$fall,   file = fall_filepath)
      }

      shinyjs::show(id = 'plotRemoteData')
      }) #END WITH PROGRESS BAR
    } #END IMPORT GCC

    data$imported_types = selected_data
    start_site = as.character(site_data$date_first)
    end_site   = as.character(site_data$date_last)

    updateSliderInput(session, 'dataDateRange',
                      min = as.Date(start_site),
                      max = as.Date(end_site),
                      value = c(as.Date(start_site), as.Date(end_site)))
    shinyjs::hide(id = 'dataDateRange')
    
    # Update plot data input to only include data that has been imported for this site
    updateSelectInput(session, 'dataTypes_plot', choices = selected_data, selected = selected_data)

    print (data$layers_df)
  }) #END GET DATA OBSERVER
  
  # Observer for download data button
  observeEvent(input$downloadDataButton, {
    print ('Download button')
  })

  
  # Observer for the popup
  observeEvent(input$plotRemoteData, {
    shinyjs::hide(id = 'noPixelWarning')
    shinyjs::hide(id = 'buildingPlot')
    shinyjs::hide(id = 'doneBuildingPlot')
    shinyjs::show(id = 'pixelTypes')
    sm_pixels = data$pixel_sps_250m
    lg_pixels = data$pixel_sps_500m
    
    types = data$imported_types
    print (types)
    
    print (sm_pixels)
    print (lg_pixels)
    if (is.null(sm_pixels@polygons[1][[1]]) & is.null(lg_pixels@polygons[1][[1]])){
      print ('no pixels selected')
      shinyjs::hide(id = 'pixelTypes')
      if ('GCC' %in% types){
        updateSelectInput(session, 'dataTypes_plot', choices ='GCC', selected = 'GCC')
        shinyjs::hide(id = 'dataDateRange')
      }else {
        updateSelectInput(session, 'dataTypes_plot', choices ='No Data Available')
      }
      
    }else if (is.null(sm_pixels@polygons[1][[1]])){
      updateSelectInput(session, 'pixelTypes', choices = c('500m'))
      updateSelectInput(session, 'dataTypes_plot', choices = types, selected = types)
    }else if (is.null(lg_pixels@polygons[1][[1]])){
      updateSelectInput(session, 'pixelTypes', choices = c('250m'))
      updateSelectInput(session, 'dataTypes_plot', choices = types, selected = types)
    }else{
      updateSelectInput(session, 'pixelTypes', choices = c('250m', '500m'))
      updateSelectInput(session, 'dataTypes_plot', choices = types, selected = types)
    }
  })
  
  observeEvent(input$dataTypes_plot, {
    types = input$dataTypes_plot
    print (types)
    if ('NDVI' %in% types | 'EVI' %in% types |'Transition Dates' %in% types |'GCC' %in% types){
      shinyjs::show(id = 'plotDataButton')
      if (length(types)==1 & types[1]=='Transition Dates'){
        shinyjs::hide(id = 'plotDataButton')
      }
    } else {
      shinyjs::hide(id = 'plotDataButton')
    }
  })

  output$downloadDataButton <- downloadHandler(
    filename = function() {
      paste0(input$site, '_data.csv')
    },
    content = function(file) {
      write.csv(data$all_data, file)
    }
  )


  #--------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  FUNCTIONS
  #--------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------------------------

  # Creates a polyline surrounding any MODIS 2016 500m or 250m pixel from cropped raster
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
       
       print (datalon)
       print (datalat)
       print (midcell)

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
  
  # Builds the polygon table to display all user created polygons in analyzer mode
  build_polygon_table = function(data_df_){
    # Creating Dataframe with 1 record per shapefile
    if (nrow(data_df_) == 0){
      df = setNames(data.frame(matrix(ncol = 3, nrow = 0)), c('Name', 'Longitude', 'Latitude'))
    }else {
      df = aggregate(data_df_[,c(2,3)], list(data_df_$Name), max)
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
}
