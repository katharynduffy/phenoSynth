# Server file for Shiny App phenoRemote

server = function(input, output, session) {
  observe({ 
    shinyBS::toggleModal(session, 'curatedDataLogin')
  })

  #--------------------------------------------------------------------------------------------------------------------------------------
  #  REACTIVE VALUES
  #--------------------------------------------------------------------------------------------------------------------------------------
  variables = reactiveValues(
                      filter   = 'All',
                      sites_df = cams_,
                      sites    = site_names,
                      color_count = 1,
                      color = 'blue')
  appeears = reactiveValues(
                      none = '')

  highlighted = reactiveValues(
                      group = '')

  phenocam    = reactiveValues()

  panel   = reactiveValues(mode = '')

  counter = reactiveValues(countervalue = 0)

  modis   = reactiveValues(data = data.frame(),
                           cached_ndvi = list())

  data    = reactiveValues(my_user = 'start',
                           tasks_left = 100,
                           site_width = 375,
                           site_height = 225,
                           tasks = NULL,
                           token = NULL,
                           NLCD = FALSE,
                           draw_mode = FALSE,
                           run   = 0,
                           names = c(),
                           plot_data_table = FALSE,
                           df    = data.frame(),
                           all_data = data.frame(),
                           veg_types = c(),
                           select_pixel_mode_was_on = FALSE,
                           pixel_sps = SpatialPolygons(list()),
                           pixel_sps_250m = SpatialPolygons(list()))

  # Empty reactive spdf
  value = reactiveValues(drawnPoly = SpatialPolygonsDataFrame(SpatialPolygons(list()),
                                                              data=data.frame()))
  output$phenoTable = DT::renderDataTable(
        cams_ ,
        filter = 'top',
        options = list(autoWidth = FALSE, scrollY = TRUE, scrollX = TRUE)
  )
  
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  OUTPUTS
  #--------------------------------------------------------------------------------------------------------------------------------------
  
  table1 = dplyr::arrange(appeears_tasks_ndvi_tera, site_name)
  table2 = dplyr::arrange(appeears_tasks_ndvi_aqua, site_name)
  table3 = dplyr::arrange(appeears_tasks_evi_tera, site_name)
  table4 = dplyr::arrange(appeears_tasks_evi_aqua, site_name)
  table5 = dplyr::arrange(appeears_tasks_lc, site_name)
  table6 = dplyr::arrange(appeears_tasks_tds, site_name)
  # Appeears ndvi_tera table
  output$appeearsTable1 = DT::renderDataTable({
    DT::datatable(table1 %>% dplyr::select(site_name, task_name, task_id, created))
  })
  # Appeears ndvi_aqua table
  output$appeearsTable2 = DT::renderDataTable({
    DT::datatable(table2 %>% dplyr::select(site_name, task_name, task_id, created))
  })
  # Appeears evi_tera table
  output$appeearsTable3 = DT::renderDataTable({
    DT::datatable(table3 %>% dplyr::select(site_name, task_name, task_id, created))
  })
  # Appeears evi_aqua table
  output$appeearsTable4 = DT::renderDataTable({
    DT::datatable(table4 %>% dplyr::select(site_name, task_name, task_id, created))
  })
  # Appeears MDOIS LC table
  output$appeearsTable5 = DT::renderDataTable({
    DT::datatable(table5 %>% dplyr::select(site_name, task_name, task_id, created))
  })
  # Appeears MODIS Tds table
  output$appeearsTable6 = DT::renderDataTable({
    DT::datatable(table6 %>% dplyr::select(site_name, task_name, task_id, created))
  })
  # Appeears my_appeears_tasks table
  output$appeearsTable7 = DT::renderDataTable({
    DT::datatable(data.frame())
  })

  # Create the map
  output$map = renderLeaflet({
    leaflet('map', data = variables$sites_df, options= leafletOptions(zoomControl=TRUE, doubleClickZoom = FALSE)) %>%
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
        circleMarkerOptions = FALSE,
        circleOptions       = FALSE,
        editOptions         = editToolbarOptions(selectedPathOptions = selectedPathOptions()),
        polygonOptions      = drawPolygonOptions(
                                              showArea     = TRUE,
                                              repeatMode   = FALSE,
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
      # addStyleEditor(openOnLeafletDraw = TRUE) %>%
      addMeasure(position          = "topleft",
                 primaryLengthUnit = "meters",
                 primaryAreaUnit   = "sqmeters",
                 activeColor       = "#3D535D",
                 completedColor    = "#7D4479") %>%
      # Adds the layers options to top left of Map
      addLayersControl(
        baseGroups    = c("World Imagery", "Open Topo Map"),
        position      = c("topleft"),
        options       = layersControlOptions(collapsed = FALSE))
    })

  # Adds the mouse lat / lon to an output (we can change this to anything)
  output$mouse = renderText({
    if(is.null(input$hover_coordinates)) {
      "Mouse outside of map"
    } else {
      paste0("Lat: ", input$hover_coordinates[1],
             "\nLng: ", input$hover_coordinates[2])
    }
  })

  # Open landing page and initialze application
  observe({ 
    if (EMAIL_MODE == FALSE){
      shinyjs::hide(id = 'emailShp')
    }
    switch_to_explorer_panel()
    data$pixel_df    = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Pixel", "Site", "lat", 'lon', 'pft'))
    data$pixel_sps_250m = SpatialPolygons(list())
    data$midcell_pixel_sin = SpatialPoints(data.frame(x = 0, y = 0), proj4string=CRS(sinu_crs))[-1,]
    panel$mode = 'explorer'
    data$paois_df = setNames(data.frame(matrix(ncol = 4, nrow = 0)), c('Name', 'Longitude', 'Latitude', 'LeafletId'))
  })

  #--------------------------------------------------------------------------------------------------------------------------------------
  #  OBSERVERS
  #--------------------------------------------------------------------------------------------------------------------------------------
  
  # Login Switch
  observeEvent(input$navbar, {
    if (input$navbar == 'AppEEARS' & is.null(data$token)){
      if (data$my_user == 'Phenocam'){
        shinyjs::show(id = 'appeearsLogin')
      } else {
        shinyjs::show(id = 'curatedDataLogin')
        shinyBS::toggleModal(session, 'curatedDataLogin')
      }
    }
  })
  
  # Login to Appeears
  observeEvent(input$butLogin, {
    withBusyIndicatorServer("butLogin", {
    print ('Loging into curated dataset')
    
    data$my_user = input$username
    my_pass = input$pwInp
    
    data$token_response = AppEEARS4R::appeears_start_session(data$my_user,my_pass)
    data$token         = paste("Bearer", data$token_response$token)
    
    if (is.null(data$token_response$token)){
      message('invalid username/password combo')
      data$my_user = 'start'
      shinyalert("Login error", 'Invalid username/password combo, try again.' , type = "error")
    }else {
      message('login successful')
      shinyBS::toggleModal(session, 'curatedDataLogin')
      # updateTabsetPanel(session, 'navbar', selected = 'curatedPanel')
      shinyjs::show(id = 'appeearsLogout')
      shinyjs::show(id = 'appeearsTools')
      shinyjs::hide(id = 'appeearsLogin')
      shinyjs::hide(id = 'submitionsLeft')
      print (length(setdiff(cams_$site, appeears_tasks_ndvi_tera$site_name)))
      if (length(setdiff(cams_$site, appeears_tasks_ndvi_tera$site_name)) > 0){
        shinyjs::show(id = 'siteDifference')
        shinyjs::hide(id = 'submitTasks')
        updateSelectInput(session, id = 'siteDifference', choices = setdiff(cams_$site, appeears_tasks_ndvi_tera$site_name))
      }else {
        shinyjs::hide(id = 'siteDifference')
        shinyjs::hide(id = 'submitTasks')
      }
    }
    })
  })
  
  
  # Login Popup
  observeEvent(input$appeearsLogin,{
    shinyBS::toggleModal(session, 'curatedDataLogin')
  })
  
  # Import AppEEARS tasks
  observeEvent(input$pullAppeearsTasks,{
    withBusyIndicatorServer("pullAppeearsTasks", {
      if (is.null(data$tasks)){
        token         = data$token
        response      = GET("https://lpdaacsvc.cr.usgs.gov/appeears/api/task", add_headers(Authorization = token))
        task_response = prettify(jsonlite::toJSON(content(response), auto_unbox = TRUE))
        data$tasks = jsonlite::fromJSON(txt=task_response)
        # Appeears ndvi_tera table
        output$appeearsTable7 = DT::renderDataTable({
          DT::datatable(data$tasks %>% 
              dplyr::select(task_name, status, task_id, created) %>% 
              dplyr::mutate(created = as.Date(created)))
        })
        data$tasks_left = get_submitions_remaining(data$tasks)
        updateTextInput(session, 'submitionsLeft', value = data$tasks_left)
        shinyjs::show(id = 'submitionsLeft')
        shinyjs::show(id = 'myTasks')
      }else{
        print ('Already grabbed tasks')
      }
    })
  })
  
  # Submit tasks to Appeears for Diff between ROI and Cached Tasks
  observeEvent(input$submitTasks, {
    print ('Pretending to send tasks to AppEEARS')
  })
  
  # Just use local Phenocam data, no password or username required
  observeEvent(input$byPassLogin,{
    data$my_user = 'phenocam'
    shinyBS::toggleModal(session, 'curatedDataLogin')
    shinyjs::show(id = 'appeearsLogin')
    print (message('Not logging into Appeears and only using Phenocam tasks'))
  })
  
  # Logout of AppEEARS
  observeEvent(input$appeearsLogout,{
    # Remove token and tasks when logging out
    data$token_response = NULL
    data$token          = NULL
    data$my_user        = 'start'
    # data$tasks          = data.frame(task_name = '<none>', status = '<none>',
    #                                  task_id = '<none>', created = '<none>')
    shinyjs::hide(id = 'appeearsLogout')
    shinyjs::hide(id = 'appeearsTools')
    updateTabsetPanel(session, 'navbar', selected = 'Site explorer')
  })
  
  # Turns ROI off if drawImage is off
  observe({
    checked = input$drawImage
    if (checked == FALSE){
      updateCheckboxInput(session, inputId = 'drawImageROI', value = FALSE)
    }
  })
  
  # Hides frequency UI element if GCC isn't selected for data to download/get
  observe({
    data = input$dataTypes_plot
    if ('GCC' %in% data){
      shinyjs::show(id = 'phenocamFrequency')
    } else{
      shinyjs::hide(id = 'phenocamFrequency')}
  })
  
  # # Opacity slider for NLCD
  # observeEvent(input$nlcdOpacity, {
  #   if (data$NLCD){
  #     opa = input$nlcdOpacity
  #     print (opa)
  #   }
  # })

  # Start of Drawing - set highlight pixel to off
  observeEvent(input$map_draw_start, {
    data$draw_mode = TRUE
    print ('Map Draw Start')
    # Turn off highlight pixel mode if it is on
    if(input$highlightPixelModeNDVI == TRUE){
      updateCheckboxInput(session, 'highlightPixelModeNDVI', value=FALSE)
      data$select_pixel_mode_was_on = TRUE
    }
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

  # Event occurs when drawing a new feature starts
  observeEvent(input$map_draw_new_feature, {
      # Leaflet ID to add to the shapefile dataframe
      id = input$map_draw_new_feature$properties$`_leaflet_id`

      # Site name combined with run # for new polygon feature
      data$run   = data$run + 1
      if (panel$mode =='analyzer'){
        name_ = paste(c(isolate(input$site),data$run), collapse='_')
      }else{
        name_ = paste0('no_site_',data$run)
      }
      data$names = c(data$names, name_)

      # Grabbing lat/lon values for new leaflet polygon
      coor      = unlist(input$map_draw_new_feature$geometry$coordinates)
      Longitude = coor[seq(1, length(coor), 2)]
      Latitude  = coor[seq(2, length(coor), 2)]

      # Building Dataframe with points from newly created leaflet feature
      c = 0
      for (x in Longitude){
        c       = c + 1
        data$paois_df = rbind(data$paois_df, data.frame(Name = name_, Longitude = x, Latitude = Latitude[c], LeafletId = id))}

      # Creating a SpatialPolygon that can be added to our spatial polygons dataframe (value$drawnPoly)
      poly    = Polygon(cbind(Longitude, Latitude))
      polys   = Polygons(list(poly), ID = name_)
      spPolys = SpatialPolygons(list(polys))

      # Adding new polygon to a spatial polygons dataframe
      value$drawnPoly = rbind(value$drawnPoly,
                             SpatialPolygonsDataFrame(spPolys, data = data.frame(notes = NA,
                                                               row.names = row.names(spPolys))))

      # Updating the select input for the download availability of created leaflet features
      updateSelectInput(session, 'shapefiles', choices = unique(data$paois_df$Name))
      updateSelectInput(session, 'shapefiles2', choices = unique(data$paois_df$Name))

      # Building the polygon table from the data$paois_df dataframe containing all of the leaflet polygon data
      build_polygon_table(data$paois_df)

      # print (data$paois_df)
      # sets highlight pixel to on
      data$draw_mode = FALSE
      print ('Exiting Draw Mode')

      print (input$map_draw_stop)
      shiny::showTab('navbar', 'paoiTab')
  })


  # When edited feature gets saved
  observeEvent(input$map_draw_edited_features, {
    # Leaflet ID to edit
    id = input$map_draw_edited_features$features[[1]]$properties$`_leaflet_id`

    # Grabbing lat/lon values for new leaflet polygon
    coor      = unlist(input$map_draw_edited_features$features[[1]]$geometry$coordinates[[1]])
    Longitude = coor[seq(1, length(coor), 2)]
    Latitude  = coor[seq(2, length(coor), 2)]
    name_     = unique(subset(data$paois_df, LeafletId == id)$Name)

    # Deletes all rows with id being edited
    data$paois_df = subset(data$paois_df, LeafletId != id)

    # Adds back the edited polygon to the dataframe (data$paois_df)
    c = 0
    for (x in Longitude){
      c       = c + 1
      data$paois_df = rbind(data$paois_df, data.frame(Name = name_, Longitude = x, Latitude = Latitude[c], LeafletId = id))}

    # Updating the polygon table from the data$paois_df dataframe containing all of the leaflet polygon data
    build_polygon_table(data$paois_df)

    print (data$paois_df)
  })


  # When feature is deleted
  observeEvent(input$map_draw_deleted_features, {
    # Leaflet ID to delete
    id      = input$map_draw_deleted_features$features[[1]]$properties$`_leaflet_id`

    # Deletes all rows with id being edited
    print (data$paois_df)
    data$paois_df = subset(data$paois_df, LeafletId != id)

    # Updating the select input for the download availability of created leaflet features
    updateSelectInput(session, 'shapefiles', choices = unique(data$paois_df$Name))

    # Updating the polygon table from the data$paois_df dataframe containing all of the leaflet polygon data
    build_polygon_table(data$paois_df)
  })
  
  observeEvent(input$shapefiles, {
    file = input$shapefiles
    updateTextInput(session, inputId = 'savePaoiFilename', value = file)
  })

  # Save shapefile button
  observeEvent(input$downloadShp,{
    site_name = input$site
    WGScoor   = subset(data$paois_df, data$paois_df$Name == input$shapefiles)
    xy        = select(WGScoor, Longitude, Latitude)
    xy_matrix = data.matrix(xy)
    
    p   = Polygon(xy_matrix)
    ps  = Polygons(list(p),1)
    sps = SpatialPolygons(list(ps))
    proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

    file     = input$savePaoiFilename
    folder   = get_download_folder()
    folder   = paste0(folder,site_name,'_paoi/')
    filename = paste(folder, file, sep='')
    dir.create(folder)
    
    print (filename)
    shapefile(sps, filename, overwrite=TRUE)
    shinyBS::toggleModal(session, 'saveShpPopup', toggle = 'close')
  })
  
  # Upload shapefile or KML?
  observeEvent(input$shpFileName, {
    imported_shpfile = input$shpFileName
    
    # Add filter and make sure the user selected atleast the .dbf, .prj, .shp, and .shx!
    
    temp_dir_name = dirname(imported_shpfile$datapath[1])
    # Rename files
    for (i in 1:nrow(imported_shpfile)) {
      file.rename(imported_shpfile$datapath[i],
        paste0(temp_dir_name, "/", imported_shpfile$name[i]))
    }
    print (imported_shpfile)
    shp_file_name = paste(temp_dir_name,
      imported_shpfile$name[grep(pattern = "*.shp$", imported_shpfile$name)], sep = "/")
    print (shp_file_name)
    uploaded_shp = readOGR(shp_file_name)
    
    if (as.character(crs(uploaded_shp)) != wgs_crs){
      print ('Wrong crs:')
      print (crs(uploaded_shp))
      print ('spTransforming it to WGS84')
      
      new_test_shape = spTransform(uploaded_shp, wgs_crs)
      leafletProxy("map") %>% addPolygons(data = new_test_shape)
    } else{
      leafletProxy("map") %>% addPolygons(data = uploaded_shp)
    }
  })
  
  # Email shapefile button
  observeEvent(input$emailShpButton, {
    print ('Send Email')
    site_name = input$site
    
    WGScoor              = subset(data$paois_df, data$paois_df$Name == input$shapefiles2)
    xy = select(WGScoor, Longitude, Latitude)
    xy_matrix = data.matrix(xy)
    p = Polygon(xy_matrix)
    ps = Polygons(list(p),1)
    sps = SpatialPolygons(list(ps))
    proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
    
    tmp_dir  = paste0('./www/', site_name, '_paoi')
    file     = isolate(input$shapefiles2)
    filename = paste0(tmp_dir,'/', file)
    
    dir.create(tmp_dir)
    shapefile(sps, filename, overwrite=TRUE)
    
    email_body = toString(WGScoor)
    
    sender <- EMAIL_UN
    # recipients <- c("kyle.enns13@alumni.colostate.edu")
    recipients <- c(EMAIL_UN)
    mailR::send.mail(from = sender,
                     to = recipients,
                     subject = paste0("Phenosynth Shapefile"),
                     body = paste0('<site_name>',site_name , '<name>',input$paoiUser,'\n<comment>', input$paoiNotes, '\n', email_body),
                     smtp = list(host.name = "smtp.gmail.com", port = 465,
                                 user.name = EMAIL_UN, # UN and PW stored in the config.R file
                                 passwd = EMAIL_PW, ssl = TRUE),
                     attach.files = c(paste0(filename,'.shp'),
                                      paste0(filename,'.dbf'),
                                      paste0(filename,'.prj'),
                                      paste0(filename,'.shx')),
                     authenticate = TRUE,
                     send = TRUE)
    
    # Remove the shapefile/folder
    unlink(tmp_dir, recursive = TRUE)
    shinyBS::toggleModal(session, 'emailShpPopup', toggle = 'close')
    
    #Convert the points to polygons: see spatialpolygons post in stackoverflow
    # https://stackoverflow.com/questions/26620373/spatialpolygons-creating-a-set-of-polygons-in-r-from-coordinates
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
        sub = subset(cams_, active == 'TRUE')
        if (dim(sub)[1]==0){
          updateSelectInput(session, 'filterSites', selected = 'All')
          updateSelectInput(session, 'site', choices = cams_$site)
        }else {
          variables$sites_df = sub
          updateSelectInput(session, 'site', choices = variables$sites_df$site)
        }
      }
      if ('Inactive' %in% variables$filter){
        sub = subset(cams_, active == 'FALSE')
        if (dim(sub)[1]==0){
          updateSelectInput(session, 'filterSites', selected = 'All')
          updateSelectInput(session, 'site', choices = cams_$site)
        }else {
          variables$sites_df = sub
          updateSelectInput(session, 'site', choices = variables$sites_df$site)
        }
        
      }
      if ('Type1' %in% variables$filter){
        sub = subset(cams_, site_type == 'I')
        if (dim(sub)[1]==0){
          updateSelectInput(session, 'filterSites', selected = 'All')
          updateSelectInput(session, 'site', choices = cams_$site)
        }else {
          variables$sites_df = sub
          updateSelectInput(session, 'site', choices = variables$sites_df$site)
        }
      }
      if ('Type2' %in% variables$filter){
        sub = subset(cams_, site_type == 'II')
        if (dim(sub)[1]==0){
          updateSelectInput(session, 'filterSites', selected = 'All')
          updateSelectInput(session, 'site', choices = cams_$site)
        }else {
          variables$sites_df = sub
          updateSelectInput(session, 'site', choices = variables$sites_df$site)
        }
      }
      if ('Type3' %in% variables$filter){
        sub = subset(cams_, site_type == 'III')
        if (dim(sub)[1]==0){
          updateSelectInput(session, 'filterSites', selected = 'All')
          updateSelectInput(session, 'site', choices = cams_$site)
        }else {
          variables$sites_df = sub
          updateSelectInput(session, 'site', choices = variables$sites_df$site)
        }
      }
      if ('NEON' %in% variables$filter){
        sub = subset(cams_, group == 'NEON' | group == "NEON AMERIFLUX" | group == "NEON LTAR LTER AMERIFLUX")
        if (dim(sub)[1]==0){
          updateSelectInput(session, 'filterSites', selected = 'All')
          updateSelectInput(session, 'site', choices = cams_$site)
        }else {
          variables$sites_df = sub
          updateSelectInput(session, 'site', choices = variables$sites_df$site)
        }
      }
    }
    variables$sites = variables$sites_df$site
    updateSelectInput(session, 'site', choices = variables$sites)
    show_all_sites(map_ = 'map', data_ = variables$sites_df)
  })


  # BUTTON
  # Zooms to the selected site in the Sites dropdown option with BUTTON
  observeEvent(input$siteZoom, {
    print('Running BUTTON Zoom to Selected Site')
    site       = isolate(input$site)
    site_data  = get_site_info(site)
    zoom_to_site(site, site_data, zoom_ = TRUE, cams_, input$drawROI)


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
    azm = as.numeric(input$azm)
    if (input$drawROI == TRUE){
      site      = input$site
      site_data = get_site_info(site)
      run_add_polyline(site_data, azm)
    }
  })

  # Draws fov polyline for a site location
  observeEvent(input$drawROI, {
    roi_bool = input$drawROI
    if (roi_bool == TRUE){
      print ('Drawing fov for phenocam site')
      site            = input$site
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
        zoom_to_site(site_ = event$id, site_data_ = site_data, 
                     zoom_  =  TRUE, 
                     data_ = cams_, 
                     draw_ = input$drawROI)
      }
    }
    if(is.null(event))
      return()

    leafletProxy("map", data = variables$sites_df) %>% clearPopups()

    site = event$id
    lat             = site_data$lat
    lon             = site_data$lon
    description     = site_data$site_description
    elevation       = site_data$Elev
    camera          = site_data$site
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
  
  
  # Change size of phenocam Image
  observeEvent(input$imagePlus,{
    data$site_width = data$site_width + (50 * (5/3))
    data$site_height = data$site_height + 50
  })
  # Change size of phenocam Image
  observeEvent(input$imageMinus,{
    data$site_width = data$site_width - (50 * (5/3))
    data$site_height = data$site_height - 50
  })

  # Site Explorer Mode Images
  # CheckBox to Show image for site from dropdown
  observe({
    print ('Doing something to Image')
    draw_bool = input$drawImage
    this_site = input$site
    roi_bool  = input$drawImageROI
    
    removeUI(selector = '#phenocamSiteImage')
    img_url = get_img_url(this_site)

    if (draw_bool == TRUE){
      pft_      = input$pftSelection
      pft       = strsplit(pft_, '_')[[1]][1]
      if (is.na(pft)){
        pft_abbr = subset(roi_files, roi_files$site == this_site)[1,]$roitype
      }else{
        pft_abbr  = subset(pft_df, pft_df$pft_expanded == pft)$pft_abbreviated
      }
      
      image_width = paste0('width:', data$site_width ,'px; ')
      image_height = paste0('height:', data$site_height ,'px; ')
      
      shinyjs::show(id = 'currentImage')
      insertUI(selector = '#image',
        ui = tags$div(id='phenocamSiteImage',
                      tags$img(src=img_url, class= 'img',
                               style=paste0("position: absolute; z-index: 1; top:0px; left:0px;",image_width, image_height))))}
      if (roi_bool == TRUE){
        pft_      = input$pftSelection
        pft       = strsplit(pft_, '_')[[1]][1]
        if (is.na(pft)){
          pft_abbr = subset(roi_files, roi_files$site == this_site)[1,]$roitype
        }else{
          pft_abbr  = (subset(pft_df, pft_df$pft_expanded == pft)$pft_abbreviated)
        }
        
          roi_url = get_roi_url(name = this_site, pft_abr = pft_abbr)
          print (roi_url)
          if (roi_url != 'Not Found'){
          insertUI(selector = '#phenocamSiteImage',
            ui =  tags$img(src=roi_url,
                       class= 'roi', style=paste0('position: absolute; z-index: 2; top:0px; left:0px;',image_width, image_height)))}

    }else if (draw_bool == FALSE){
      removeUI(selector = '#phenocamSiteImage')
      shinyjs::hide(id = 'currentImage')
    }
  })


  # Button switches to Analyzer Mode
  observeEvent(input$analyzerMode,{
    withBusyIndicatorServer("analyzerMode", {
    panel$mode = 'analyzer'
    site       = input$site
    data$site  = site
    site_data  = get_site_info(site)
    data$all_data = data.frame()
    # Reactive variables for changing and tracking color of highlighted pixel
    variables$color_count = 1
    variables$color_list = c()
    variables$color_list_reserve = rainbow(20)
    data$nlcd_breakdown_df = data.frame()
    
    # Setting raster grid FALSE since it doesn't exist yet
    data$raster_grid = FALSE
    
    # Set up directories to store data
    file_path     = paste0('./www/site_data/', site, '/data_layers/')
    main          = './www/site_data'
    npn_grid_dir  = './www/npn_grid_data'
    lc_filepath        = paste0(file_path, 'lc/')
    ndvi_filepath      = paste0(file_path,'ndvi/')
    ndvi_tera_filepath = paste0(ndvi_filepath, 'tera/')
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
    if (!file.exists(ndvi_filepath)){
      dir.create(file.path(ndvi_filepath))
    }
    if (!file.exists(ndvi_tera_filepath)){
      dir.create(file.path(ndvi_tera_filepath))
    }
    if (!file.exists(lc_filepath)){
      dir.create(file.path(lc_filepath))
    }
    

    # Landcover layer
    # Download or Import landcover for this site
    print ('Importing Landcover')
    appeears$landcover = get_appeears_task(site, type = 'landcover')
    if (length(list.files(lc_filepath))==0){
      lc_bundle_df = download_bundle_file(appeears$landcover$task_id, lc_filepath)
      lc_name  = subset(lc_bundle_df, file_type == 'nc')$file_name
    }else {
      # lc_bundle_df = get_appeears_bundle_df(appeears$landcover$task_id)
      lc_files = list.files(lc_filepath)
      lc_name  = lc_files[grepl('MCD12Q1.006_500m_aid0001.nc', lc_files)]
    }
    print (lc_filepath)
    print (lc_name)
    # NDVI layer
    # Download or Import NDVI for this site to use to resample landcover
    appeears$ndvi_tera = get_appeears_task(site, type = 'ndvi_tera')
    if (length(list.files(ndvi_tera_filepath))==0){
      ndvi_bundle_df_tera = download_bundle_file(appeears$ndvi_tera$task_id, ndvi_tera_filepath)
      ndvi_tera_name   = subset(ndvi_bundle_df_tera, file_type == 'nc')$file_name
    }else {
      # ndvi_bundle_df_tera = get_appeears_bundle_df(appeears$ndvi_tera$task_id)
      ndvi_files = list.files(ndvi_tera_filepath)
      ndvi_tera_name  = ndvi_files[grepl('MOD13Q1.006_250m_aid0001.nc', ndvi_files)]
    }
    
    # Bringing in 250m sinu and re-projecting to merc
    # ndvi_tera_name   = subset(ndvi_bundle_df_tera, file_type == 'nc')$file_name
    ndvi_tera_path   = paste0(ndvi_tera_filepath, ndvi_tera_name)
    ndvi_tera_brick  = raster::brick(ndvi_tera_path, varname='_250m_16_days_NDVI', crs=sinu_crs)
    ndvi_raster_t    = raster::subset(ndvi_tera_brick, 1)
    ndvi_raster_merc = projectRaster(from = ndvi_raster_t, crs = merc_crs, res = res(ndvi_raster_t))
    # Bringing in 500m sinu, resampling to 250m, and then re-projecting back to 500m to merc
    lc_path  = paste0(lc_filepath, lc_name)
    lc_brick  = raster::brick(lc_path, crs=sinu_crs) #ONAQ breaks here
    lc_raster = raster::subset(lc_brick, 1)
    lc_raster_ = raster::resample(x = lc_raster, y = ndvi_raster_t, crs = sinu_crs, method='ngb')
    lc_raster_merc = projectRaster(from = lc_raster_, crs = merc_crs, method='ngb', res = res(ndvi_raster_t))
    # lc_raster_merc = projectRaster(from = lc_raster_, crs = merc_crs, method='ngb', res = res(ndvi_raster_t)*2)
    
    veg_types  = c()
    print ('Switching to Analyze Mode')
    zoom_to_site(site, site_data, TRUE, cams_, input$drawROI, zoom_value = 14)
    highlighted$group = paste0(site, ' Highlighted Pixels')

    output$analyzerTitle = renderText({paste0('Site:: ', site)})
    switch_to_analyzer_panel()

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
        veg_types = c(veg_types, add_veg)
      }
      veg_types = unique(veg_types)
      data$veg_types = veg_types

      # Building Landcover layer and color pallette for specific pft composition in clipped raster
      lat_wgs = site_data$lat
      lng_wgs = site_data$lon
      # from wgs to sinusoidal
      pt_sinu = from_crs1_to_crs2_lon_lat(lon_ = lng_wgs, lat_ = lat_wgs, from_crs = wgs_crs, to_crs = sinu_crs)
      data$lat_sin = pt_sinu@coords[2]
      data$lng_sin = pt_sinu@coords[1]
      # from wgs to web mercator
      pt_merc = from_crs1_to_crs2_lon_lat(lon_ = lng_wgs, lat_ = lat_wgs, from_crs = wgs_crs, to_crs = merc_crs)
      data$lat_merc = pt_merc@coords[2]
      data$lng_merc = pt_merc@coords[1]
      
      data$r_landcover = crop_raster(data$lat_merc, data$lng_merc, lc_raster_merc, height = 10000, width = 10000, crs_str = merc_crs)
      
      # Read in NLCD if site is within NLCD extent (in Mercator)
      data$NLCD = FALSE
      site_nlcd_file = paste0('./www/landsat_lc/', site, '_landsat_lc.tif')
      
      # If NLCD layer exists for site, add it to map
      if (file.exists(site_nlcd_file)){
        site_nlcd_raster = raster::raster(site_nlcd_file) 
        data$r_nlcd = site_nlcd_raster
        key_df = read.csv('./www/landsat_lc/nlcd_key.csv')
        data$nlcd_c = build_landsat_lc_pallet(data$r_nlcd, key_df)
        data$NLCD = TRUE
        shinyjs::show(id = 'nlcdOpacity')
      }
      
      updateSelectInput(session, 'pftSelection', choices = veg_types)
      
      data$veg_types = veg_types
      print (veg_types)

      pft = strsplit(veg_types[1], '_')[[1]][1]
      print (pft)
      pft_key = (subset(pft_df, pft_df$pft_expanded == pft)$pft_key)
      print (as.numeric(pft_key))

      data$c3 = build_pft_palette(data$r_landcover)
      rc   = crop_raster(lat_ = data$lat_merc, lon_ = data$lng_merc , r_ = data$r_landcover, crs_str = merc_crs, reclassify=TRUE, primary = as.numeric(pft_key), crop=FALSE)
      leafletProxy('map') %>%
        clearControls() %>%
        clearImages() %>%
        addRasterImage(data$r_landcover, opacity = .65, project=TRUE, group='MODIS Land Cover 2016', colors = data$c3$palette) %>%
        addRasterImage(rc, opacity = .2, project=TRUE, group= 'Vegetation Cover Agreement', colors= c('green','gray')) %>%
        addLegend(labels = data$c3$names, colors = data$c3$colors, position = "bottomleft", opacity = .95, title = 'MODIS Landcover', group = 'MODIS Landcover') %>%
        addLegend(values = c(1,2), position = 'bottomright', title = 'Vegetation Cover Agreement',
                  colors = c('green', 'grey'), labels = c('ROI-Match', 'No-Match')) %>%
        addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
                         overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement'),
                         position = c("topleft"), 
                         options = layersControlOptions(collapsed = FALSE))
      
      # If NLCD layer exists for site, add it to map
      if (data$NLCD){
        # modis to landsat lookup - Removing Evergreen broadleaf forest and Deciduous needleaf forest and the 2nd Shrubland
        landsat_lc = Landsat_Landcover %>% 
          mutate(Landsat.Class = replace(Landsat.Class, MODIS.Class == 3, NA)) %>%
          mutate(Landsat.Class = replace(Landsat.Class, MODIS.Class == 2, NA)) %>% 
          mutate(Landsat.Class = replace(Landsat.Class, MODIS.Class == 7, NA))
        # create a landsat to modis lookup (so that no landsat values are left out)
        landsat_lc_lookup = read.csv('./www/landsat_lc/nlcd_key.csv') %>% 
          dplyr::select(ID,NLCD.Land.Cover.Class) %>% left_join(landsat_lc, by = c('ID' = 'Landsat.Class')) %>%
          mutate(MODIS.Class = replace(MODIS.Class, ID == 12, NA)) %>%
          left_join(pft_df, by = c('MODIS.Class' = 'pft_key'))
        
        # Build crosswalk matrix for reclassify function (rcl)
        from_values = landsat_lc_lookup$ID
        becomes_values   = landsat_lc_lookup$MODIS.Class
        
        # Build matrix to use in reclassify function
        m = matrix(ncol = 2, nrow = length(from_values))
        m[,1] = from_values
        m[,2] = becomes_values
        
        # reclassified nlcd layer to match modis values
        data$rc_nlcd = reclassify(data$r_nlcd, m)
        # Color palette for both nlcd and modis landcover
        data$rc_nlcd_c = build_pft_palette(data$rc_nlcd)
        data$nlcd_modis_c = build_pft_palette(data$r_landcover, data$rc_nlcd)
        
        leafletProxy('map') %>% addRasterImage(data$rc_nlcd, colors = data$rc_nlcd_c$palette, opacity = .7, group = '2016 NLCD') %>%
          clearControls() %>% 
          addLegend(labels = data$nlcd_modis_c$names, colors = data$nlcd_modis_c$colors, position = "bottomleft", opacity = .95, title = 'Landcover')  %>%
          addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
            overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '2016 NLCD'),
            position = c("topleft"), 
            options = layersControlOptions(collapsed = FALSE)) %>% 
          hideGroup("2016 NLCD")
      }
      
    }
    }) # End busy indicator
  }) # End analyzerMode Observer

  # When ROI Vegetation type changes re-plot highlighted veg type, change roi mask to overlay, and 
  #  the csv data to import form phenocam API
  observeEvent(input$pftSelection, {
    if (panel$mode == 'analyzer'){
        # Change vegetation cover agreement to match selected ROI in pftSelection
        print ('Running pft Selection')
        site       = input$site
        site_data  = get_site_info(site)
        pft        = input$pftSelection

        # pft = strsplit(pft, '_')[[1]][1]
        pft_key = (subset(pft_df, pft_df$pft_expanded == pft)$pft_key)
        pft_abbr = as.character(subset(pft_df, pft_df$pft_expanded == pft)$pft_abbreviated)
        
        if (pft == 'Shrubland'){pft_abbr = 'SH'}
        if (pft == 'Mixed Forest'){pft_abbr = 'MF'}
        
        data$pft_abbr = pft_abbr
        print (as.numeric(pft_key))
        
        rc   = crop_raster(lat_ = data$lat_merc, lon_ = data$lng_merc , 
                           r_ = data$r_landcover, crs_str = merc_crs, 
                           reclassify=TRUE, primary = as.numeric(pft_key), crop=FALSE)

        leafletProxy('map') %>%
          clearImages() %>%
          addRasterImage(data$r_landcover, opacity = .65, project=TRUE, group='MODIS Land Cover 2016', colors = data$c3$palette) %>%
          addRasterImage(rc, opacity = .35, project=TRUE, group= 'Vegetation Cover Agreement', colors= c('green','gray')) %>%
          addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
                           overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement'),
                           position = c("topleft"),
                           options = layersControlOptions(collapsed = FALSE))
        
        # If NLCD layer exists for site, add it to map
        if (data$NLCD & data$raster_grid){
          leafletProxy('map') %>% addRasterImage(data$rc_nlcd, colors = data$rc_nlcd_c$palette, opacity = .7, group = '2016 NLCD') %>%
            addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
              overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '2016 NLCD', '250m MODIS Grid'),
              position = c("topleft"), 
              options = layersControlOptions(collapsed = FALSE)) %>% 
            hideGroup("2016 NLCD")
        }else if (data$NLCD){
          leafletProxy('map') %>% addRasterImage(data$rc_nlcd, colors = data$rc_nlcd_c$palette, opacity = .7, group = '2016 NLCD') %>%
            addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
              overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '2016 NLCD'),
              position = c("topleft"), 
              options = layersControlOptions(collapsed = FALSE)) %>% 
            hideGroup("2016 NLCD")
        }else if (data$raster_grid){
          leafletProxy('map') %>% addRasterImage(data$rc_nlcd, colors = data$rc_nlcd_c$palette, opacity = .7, group = '2016 NLCD') %>%
            addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
              overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '250m MODIS Grid'),
              position = c("topleft"), 
              options = layersControlOptions(collapsed = FALSE)) %>% 
            hideGroup("2016 NLCD")
        }
    }
  })

  #Button switches to Site explorer mode
  observeEvent(input$siteExplorerMode,{
    print ('Switching to Explorer Mode')
    panel$mode = 'explorer'
    updateCheckboxInput(session, inputId = 'drawROI', value=FALSE)
    switch_to_explorer_panel()
    data$pixel_df       = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Pixel", "Site", "lat", 'lon', 'pft'))
    data$pixel_sps_250m = SpatialPolygons(list())
    data$midcell_pixel_sin = SpatialPoints(data.frame(x = 0, y = 0), proj4string=CRS(sinu_crs))[-1,]
  })

  # Button that plots selected Data Types
  observeEvent(input$plotDataButton, {
    withBusyIndicatorServer("plotDataButton", {
    shinyBS::toggleModal(session, 'plotDataPopup', toggle = 'close')
    
    # Selected pixels
    sm_pixels = data$midcell_pixel_sin
    merc_pixels = data$pixel_sps_250m
    
    shinyjs::html('plotTitle', paste0('Phenocam Site: ', data$site))
    
    
    # Loop through the merc_pixels and calculate their percentage cover of NLCD? do here, or do when selecting? Might take too long..
    # This code is a duplicate of code in showpos function 
    # if (data$NLCD == TRUE){
    #   pixel_percentages = c()
    #   pixel_heterogens = c()
    #   data_df_at_pixel_final = data.frame()
    #   num_pixels = length(merc_pixels)
    #   for (p in 1:num_pixels){
    #     this_pixel = merc_pixels[p]
    #     id_ = sp::getSpPPolygonsIDSlots(this_pixel)
    #     selected_pixel = raster::crop(data$rc_nlcd, this_pixel, snap = 'out' )
    #     # reproject with higher resolution by setting the resolution equal to 1
    #     selected_pixel_high_res   = raster::projectRaster(from = selected_pixel, crs = merc_crs, method='ngb', res = res(selected_pixel)/40.5)
    #     # selected_pixel_high_res   = raster::projectRaster(from = selected_pixel, crs = merc_crs, method='ngb', res = res(selected_pixel)/5)
    #     selected_pixel_high_res_c = raster::crop(selected_pixel_high_res, this_pixel, snap = 'in' )
    #     
    #     # Build dataframe with frequency of landcover PFT values at this pixel
    #     data_df_at_pixel = as.data.frame(table(selected_pixel_high_res_c@data@values), stringsAsFactors=FALSE) %>% 
    #       mutate(Var1 = as.double(Var1)) %>%
    #       left_join(pft_df, by = c('Var1' = 'pft_key')) %>% 
    #       mutate(id = id_)
    #     
    #     # Calculate percentage of selected values from total values
    #     total_pixels = sum(data_df_at_pixel$Freq)
    #     selected_values = subset(data_df_at_pixel, data_df_at_pixel$Var1 == data$pft)$Freq
    #     selected_percentage = selected_values/total_pixels 
    #     heterogeneity_at_pixel = length(data_df_at_pixel$Var1)
    #     
    #     if (length(selected_percentage) ==0){
    #       selected_percentage = 0}
    #     if (length(heterogeneity_at_pixel) ==0){
    #       heterogeneity_at_pixel = 0}
    #     
    #     pixel_heterogens  = c(pixel_heterogens, heterogeneity_at_pixel)
    #     pixel_percentages = c(pixel_percentages, selected_percentage)
    #     data_df_at_pixel_final = rbind(data_df_at_pixel_final, data_df_at_pixel)
    #   }
    #   data$nlcd_breakdown_df = data_df_at_pixel_final
    # }
    
    # Inputs from popup
    selected_data = input$dataTypes_plot
    selected_pixel_type = input$pixelTypes
    
    # Code that builds a list and char vector to be used in group checkboxes for plotting
    # - Allows for renaming of checkboxes dynamically based on selected plotting data
    plot_selected = c()
    plot_choices = list()
    download_choices = c()
    if ('GCC' %in% selected_data){
      plot_choices[['GCC']] = 'GCC'
      plot_selected = c(plot_selected, 'GCC')
      download_choices = c(download_choices, 'GCC Data', 'GCC Spring Transition Dates', 'GCC Fall Transition Dates')}
    if ('NDVI' %in% selected_data){
      plot_choices[['High Quality NDVI']] = 'hiq_ndvi'
      plot_choices[['All NDVI']] = 'all_ndvi'
      plot_selected = c(plot_selected, 'hiq_ndvi', 'all_ndvi')
      download_choices = c(download_choices, 'NDVI')
      download_choices = c(download_choices, 'Selected Pixel CSV')}
    if ('EVI' %in% selected_data){
      plot_choices[['High Quality EVI']] = 'hiq_evi'
      plot_choices[['All EVI']] = 'all_evi'
      plot_selected = c(plot_selected, 'hiq_evi', 'all_evi')
      if ('Selected Pixel CSV' %!in% download_choices){
        download_choices = c(download_choices, 'Selected Pixel CSV')
      }
      download_choices = c(download_choices, 'EVI')}
    if ('Transition Dates' %in% selected_data){
      plot_choices[['Transition Dates (EVI/NDVI)']] = 'tds_sat'
      plot_selected = c(plot_selected, 'tds_sat')
      download_choices = c(download_choices, 'MODIS Transition Dates')}
    
    updateSelectInput(session, 'dataTypes_download', choices = download_choices)
    updateCheckboxGroupInput(session, inputId = 'plotTheseBoxes',
                             choices  = plot_choices,
                             selected = plot_selected,
                             inline   = TRUE)
    
    print (paste0('Plotting: ', selected_data))
    
    if (is.null(sm_pixels@coords[1][[1]])){
      output$plotTable <- DT::renderDataTable(
        data.frame(empty = 'empty'),
        filter = 'top',
        options = list(autoWidth = TRUE, scrollY = TRUE)
      )
    }else{
      output$plotTable = DT::renderDataTable(
        data$pixel_df_table,
        filter = 'top',
        options = list(autoWidth = TRUE, scrollY = TRUE))
    }
    
    
    # ------------------PLOT GCC------------------------------------
    if ('GCC' %in% selected_data){
      withProgress(message = 'Building GCC Plot', value = .1, {
      print ('Plotting GCC')
        
      # Get current pft from pftSelection observer
      pft_abbr = data$pft_abbr
      freq_ = as.numeric(substring(input$phenocamFrequency, 1, 1))

      # Input into plotting gcc function
      gcc_p = gcc_plot(phenocam$gcc_all[[pft_abbr]][[paste0('gcc_', freq_,'day')]], 
                       phenocam$gcc_all[[pft_abbr]][[paste0('spring_', freq_,'day')]], 
                       phenocam$gcc_all[[pft_abbr]][[paste0('fall_', freq_,'day')]])

      data$gcc_p = gcc_p
      # Show and switch to plotpanel
      if (length(selected_data) == 1){
        # shiny::showTab('navbar','PlotPanel')
        # updateTabsetPanel(session, 'navbar', selected = 'PlotPanel')
      }
      }) #END WITH PROGRESS BAR
    } #END GCC PLOT
    
    # ------------------PLOT NPN------------------------------------
    if ('NPN' %in% selected_data){
      print ('Plotting NPN')
      ############################
      # Add the NPN data into the 
      # Dataframe which has all 
      # of the selected pixels
      # and all their corresponding 
      # data
      ############################
    } #END NPN PLOT
    
    # --------------- TRANSITION DATE EXTRACTION FOR PIXELS ------------
    if ('Transition Dates' %in% selected_data){
      withProgress(message = 'Compiling Transition Dates', value = .1, {
      # Extracting lat/lng values for selected 250m pixels
      data$tds_path
      data$tds_nc
      
      # all raster bricks for transition dates data
      td_v6_names = c('Greenup', 'Dormancy', 'Maturity', 'MidGreendown', 'MidGreenup', 'Peak', 'Senescence','QA_Overall', 'QA_Detailed')
      # Get start date to add values from transition date data to
      dunits = ncatt_get(data$tds_nc,'Greenup', "units")$value
      start_date = as.Date(strsplit(dunits, 'days since ')[[1]][2], format = '%m-%d-%Y')
      years = format(as.Date(data$td_v6_ncs['Greenup'][[1]]@data@names, 'X%Y.%m.%d'), '%Y')
      pixel_df_all_tds = data.frame()
      for (name in td_v6_names){
        incProgress(amount = (1/length(td_v6_names)))
        pixel_ids = as.vector(sm_pixels@data$ID)
        # Extract at transition date name
        tds_values = raster::extract(data$td_v6_ncs[name][[1]], sm_pixels)
        rownames(tds_values) = pixel_ids
        for (pixel in pixel_ids){
          dates_at_pixel = as.vector(tds_values[pixel,]) + start_date
          pixel_df = data.frame(dates = dates_at_pixel, layer = name, pixel = pixel, year = years, value = NA, stringsAsFactors = FALSE )
          pixel_df_all_tds = rbind(pixel_df_all_tds, pixel_df)
        }
      }
      data$pixel_df_all_tds = pixel_df_all_tds
      as_tibble(pixel_df_all_tds)

      }) #END WITH PROGRESS BAR
    } # END TRANSITION DATE EXTRATION FOR PIXELS
    
    # ------------------PLOT NDVI------------------------------------
    if ('NDVI' %in% selected_data){
      withProgress(message = 'Building NDVI Plot', value = .4, {
      print ('Plotting NDVI')
    
      if (is.null(sm_pixels@coords[1][[1]])){
        print ('No pixels selected')
        ndvi_p = plot_ly()
      }else{
        ndvi_under_pixel_tera = raster::extract(data$ndvi_tera_brick, sm_pixels)
        ndvi_under_pixel_aqua = raster::extract(data$ndvi_aqua_brick, sm_pixels)
        
        qc_ndvi_under_pixel_tera = raster::extract(data$ndvi_qc_tera_brick, sm_pixels)
        qc_ndvi_under_pixel_aqua = raster::extract(data$ndvi_qc_aqua_brick, sm_pixels)

        ndvi_p = plot_ly()

        for (num in c(1:length(sm_pixels@data$ID))){
          incProgress(amount = (1/length(sm_pixels@data$ID))*.8)
          pixel_id = sm_pixels@data$ID[num]
          
          ndvi_tera = ndvi_under_pixel_tera[num,]
          ndvi_qc_tera = qc_ndvi_under_pixel_tera[num,]
          
          ndvi_aqua = ndvi_under_pixel_aqua[num,]
          ndvi_qc_aqua = qc_ndvi_under_pixel_aqua[num,]
          
          dates_tera = as.Date(names(ndvi_tera),format='X%Y.%m.%d')
          dates_aqua = as.Date(names(ndvi_aqua),format='X%Y.%m.%d')
          
          ndvi_brick_df_tera = data.frame(date = dates_tera, 
                                     pixel = pixel_id, 
                                     ndvi_raw = as.vector(ndvi_tera), 
                                     ndvi_qc = as.vector(ndvi_qc_tera),
                                     type    = 'TERA')
          ndvi_brick_df_aqua = data.frame(date = dates_aqua, 
                                          pixel = pixel_id, 
                                          ndvi_raw = as.vector(ndvi_aqua), 
                                          ndvi_qc = as.vector(ndvi_qc_aqua),
                                          type    = 'AQUA')
          
          # Add Filtered Data Column to TERA
          qc_tera_df = subset(qc_df_tera, qc_df_tera$MODLAND == 'VI produced with good quality' & qc_df_tera$VI.Usefulness == 'Decreasing quality' | 
              qc_df_tera$VI.Usefulness == 'Highest quality' & qc_df_tera$MODLAND == 'VI produced with good quality')
          qa_tera_v  = qc_tera_df$Value
          ndvi_brick_df_tera$ndvi_filtered = ifelse(ndvi_brick_df_tera$ndvi_qc %in% qa_tera_v ,ndvi_brick_df_tera$ndvi_raw, NA)
          
          # Add Filtered Data Column to AQUA
          qc_aqua_df = subset(qc_df_aqua, qc_df_aqua$MODLAND == 'VI produced with good quality' & qc_df_aqua$VI.Usefulness == 'Decreasing quality' | 
              qc_df_aqua$VI.Usefulness == 'Highest quality' & qc_df_aqua$MODLAND == 'VI produced with good quality')
          qa_aqua_v  = qc_aqua_df$Value
          ndvi_brick_df_aqua$ndvi_filtered = ifelse(ndvi_brick_df_aqua$ndvi_qc %in% qa_aqua_v ,ndvi_brick_df_aqua$ndvi_raw, NA)
          
          # Combine aqua and tera for this pixel
          ndvi_brick_df = rbind(ndvi_brick_df_tera, ndvi_brick_df_aqua)
          
          # Add ndvi_brick_df data (one pixel worth) to a larger df with all pixels and ndvi
          if (num == 1){
            ndvi_pixel_data_df = ndvi_brick_df
          }else {
            ndvi_pixel_data_df = rbind(ndvi_pixel_data_df, ndvi_brick_df)
          }}
        
        data$ndvi_pixels = ndvi_pixel_data_df
        print (as_tibble(data$ndvi_pixels))
      }
        })# END WITH PROGRESS BAR
      } #END NDVI PLOT
    
    # ------------------PLOT EVI------------------------------------
    if ('EVI' %in% selected_data){
      withProgress(message = 'Building EVI Plot', value = .4, {
      print ('Plotting EVI')
      
      if (is.null(sm_pixels@coords[1][[1]])){
        print ('No pixels selected')
        evi_p = plot_ly()
      }else{
        
        evi_under_pixel_tera = raster::extract(data$evi_tera_brick, sm_pixels)
        evi_under_pixel_aqua = raster::extract(data$evi_aqua_brick, sm_pixels)
        
        qc_evi_under_pixel_tera = raster::extract(data$evi_qc_tera_brick, sm_pixels)
        qc_evi_under_pixel_aqua = raster::extract(data$evi_qc_aqua_brick, sm_pixels)
      
        evi_p = plot_ly()
        
        for (num in c(1:length(sm_pixels@data$ID))){
          incProgress(amount = (1/length(sm_pixels@data$ID))*.8)
          pixel_id = sm_pixels@data$ID[num]
          
          evi_tera = evi_under_pixel_tera[num,]
          evi_qc_tera = qc_evi_under_pixel_tera[num,]
          
          evi_aqua = evi_under_pixel_aqua[num,]
          evi_qc_aqua = qc_evi_under_pixel_aqua[num,]
          
          dates_tera = as.Date(names(evi_tera),format='X%Y.%m.%d')
          dates_aqua = as.Date(names(evi_aqua),format='X%Y.%m.%d')
          
          evi_brick_df_tera = data.frame(date = dates_tera, 
                                          pixel = pixel_id, 
                                          evi_raw = evi_tera, 
                                          evi_qc = evi_qc_tera,
                                          type    = 'TERA')
          evi_brick_df_aqua = data.frame(date = dates_aqua, 
                                          pixel = pixel_id, 
                                          evi_raw = evi_aqua, 
                                          evi_qc = evi_qc_aqua,
                                          type    = 'AQUA')
          
          # Add Filtered Data Column to TERA
          qc_tera_df = subset(qc_df_tera, qc_df_tera$MODLAND == 'VI produced with good quality' & qc_df_tera$VI.Usefulness == 'Decreasing quality' | 
              qc_df_tera$VI.Usefulness == 'Highest quality' & qc_df_tera$MODLAND == 'VI produced with good quality')
          qa_tera_v  = qc_tera_df$Value
          evi_brick_df_tera$evi_filtered = ifelse(evi_brick_df_tera$evi_qc %in% qa_tera_v ,evi_brick_df_tera$evi_raw, NA)
          
          # Add Filtered Data Column to AQUA
          qc_aqua_df = subset(qc_df_aqua, qc_df_aqua$MODLAND == 'VI produced with good quality' & qc_df_aqua$VI.Usefulness == 'Decreasing quality' | 
              qc_df_aqua$VI.Usefulness == 'Highest quality' & qc_df_aqua$MODLAND == 'VI produced with good quality')
          qa_aqua_v  = qc_aqua_df$Value
          evi_brick_df_aqua$evi_filtered = ifelse(evi_brick_df_aqua$evi_qc %in% qa_aqua_v ,evi_brick_df_aqua$evi_raw, NA)
          
          # Combine aqua and tera for this pixel
          evi_brick_df = rbind(evi_brick_df_tera, evi_brick_df_aqua)
          
          # Add evi_brick_df data (one pixel worth) to a larger df with all pixels and evi
          if (num == 1){
            evi_pixel_data_df = evi_brick_df
          }else {
            evi_pixel_data_df = rbind(evi_pixel_data_df, evi_brick_df)
          }
            } #END 250M LOOP

          
          # evi_pixel_data_df$evi_filtered = ifelse(ndvi_pixel_data_df$ndvi_qc == 2112 | ndvi_pixel_data_df$ndvi_qc == 4160 | ndvi_pixel_data_df$ndvi_qc == 4163 | ndvi_pixel_data_df$ndvi_qc == 6208 | ndvi_pixel_data_df$ndvi_qc == 6211,
          #                                         evi_pixel_data_df$evi_raw, NA)
          data$evi_pixels = evi_pixel_data_df
          print (as_tibble(data$evi_pixels))

      }
        
      }) #END WITH PROGRESS BAR
    } #END EVI PLOT
    
    # Raise an error if no data is selected to plot
    if (length(selected_data) == 0){
      shinyalert("Plotting error", 'No data selected. Try again.' , type = "error")
    }
    
    # Show plot panel tab if GCC, NDVI, or EVI are selected
    if ('GCC' %in% selected_data | 'NDVI' %in% selected_data | 'EVI' %in% selected_data){
      shiny::showTab('navbar','PlotPanel')
      updateTabsetPanel(session, 'navbar', selected = 'PlotPanel')
    }
    }) # END BUSY INDICATOR
  }) #END PLOTTING DATA OBSERVER
  
  #-----------------------------------------------------------------------------------------------------------------
  
  # Plot the data
  output$data_plot = renderPlotly({
    print ('Building plot Data tab plots')
    selected_data = input$dataTypes_plot
    selected_plots = input$plotTheseBoxes
    data$plotTable = subset(data$pixel_df, data$pixel_df$Type == '250m')
    # data$plotTable = subset(data$pixel_df, data$pixel_df$Site == data$site)
    print (selected_plots)

    # Catches empty dataframe so that GCC can still plot
    if (dim(data$plotTable)[1]==0){
    }else {
      s  = req(input$plotTable_rows_all)
      sd = data$plotTable[s, , drop = FALSE]
      
      print (as_tibble(sd))
    }
    
    if ('NDVI' %in% selected_data){
      ndvi_pixel_data_df = subset(data$ndvi_pixels, data$ndvi_pixels$pixel %in% sd$Pixel)
      print (as_tibble(ndvi_pixel_data_df))
      rownames(ndvi_pixel_data_df) = NULL
      
      # NDVI HIGH QUALITY
      if ('hiq_ndvi' %in% selected_plots){
        if (length(unique(ndvi_pixel_data_df$ndvi_filtered)) == 1){
          selected_plots = selected_plots[ - which(selected_plots %in% 'hiq_ndvi')]
        }else {
          p_ndvi = ndvi_pixel_data_df %>%
            subset(ndvi_pixel_data_df$pixel %in% sd$Pixel) %>%
            select(pixel, date, ndvi_filtered) %>%
            mutate(pixel = paste0('NDVI_high_', pixel), Date = date) %>%
            arrange(pixel, Date) %>%
            plot_ly(x = ~Date,
                    y = ~ndvi_filtered) %>%
            add_trace(
              mode = 'markers',
              type = "scatter",
              color = ~pixel,
              colors = c('green', 'dark green'),
              marker = list(size = 5),
              showlegend = TRUE,
              legendgroup = ~pixel,
              text = ~paste("Date: ", Date,
                            '<br>Pixel: ', pixel,
                            '<br>Data: NDVI')) %>%
            layout(xaxis = list(title = "Date"))
          
          # Spline added here
          mNDVIhq = ndvi_pixel_data_df %>%
            filter(!is.na(ndvi_pixel_data_df$ndvi_filtered)) %>%
            group_by(date) %>%
            summarise(meanNDVI = mean(ndvi_filtered))
          # Loess ndvi df
          smooth_ndvi_df_hq = data.frame(date = as.Date(mNDVIhq$date), ndvi_smooth = fitted(smooth.spline(mNDVIhq$meanNDVI~as.Date(mNDVIhq$date)), data=mNDVIhq))
          
          p_ndvi= p_ndvi %>%
            add_trace(
              x = smooth_ndvi_df_hq$date, 
              y = smooth_ndvi_df_hq$ndvi_smooth,
              mode = "lines",
              line = list(width = 2, color = "rgb(120,120,120)"),
              name = "NDVI loess HiQ",
              showlegend = TRUE) %>% layout(xaxis = list(title = "Date"))
          
          p_ndvi = add_title_to_plot(df = p_ndvi,
                                     x_title_ = 'NDVI (High Quality data)',
                                     y_title_ = 'NDVI value')
        }
      }
      # NDVI RAW
      if ('all_ndvi' %in% selected_plots){
        mNDVI = ndvi_pixel_data_df %>%
          filter(!is.na(ndvi_pixel_data_df$ndvi_raw)) %>%
          group_by(date) %>%
          summarise(meanNDVI = mean(ndvi_raw))
        
        clean_ndvi_pixel_data_df = ndvi_pixel_data_df %>%
          subset(ndvi_pixel_data_df$pixel %in% sd$Pixel) %>%
          mutate(pixel = paste0('NDVI_all_', pixel), Date = date) %>%
          select(pixel, Date, ndvi_raw) %>%
          arrange(pixel, Date)
        
        p_ndvi_raw = plot_ly() %>%
          add_trace(
            data = clean_ndvi_pixel_data_df,
            x = ~Date,
            y = ~ndvi_raw,
            mode = 'markers',
            type = "scatter",
            color = ~pixel,
            colors = c('green', 'dark green'),
            marker = list(size = 5),
            showlegend = TRUE,
            legendgroup = ~pixel,
            text = ~paste("Date: ", Date,
                          '<br>Pixel: ', pixel,
                          '<br>Data: NDVI'))
        
        # Spline added here
        # Loess ndvi df
        smooth_ndvi_df = data.frame(date = as.Date(mNDVI$date), ndvi_smooth = fitted(smooth.spline(mNDVI$meanNDVI~as.Date(mNDVI$date)), data=mNDVI))
        p_ndvi_raw = p_ndvi_raw %>%
          add_trace(
            x = smooth_ndvi_df$date, 
            y = smooth_ndvi_df$ndvi_smooth,
            mode = "lines",
            line = list(width = 2, color = "rgb(120,120,120)"),
            name = "NDVI loess All",
            showlegend = TRUE
          ) %>%layout(xaxis = list(title = "Date"))
        
        p_ndvi_raw = add_title_to_plot(df = p_ndvi_raw,
                                       x_title_ = 'NDVI (All data)',
                                       y_title_ = 'NDVI value')
      }
    }
    
    if ('EVI' %in% selected_data){
      # EVI HIGH QUALITY
      evi_pixel_data_df = subset(data$evi_pixels, data$evi_pixels$pixel %in% sd$Pixel)
      print (as_tibble(evi_pixel_data_df))
      mEVI = evi_pixel_data_df %>%
        filter(!is.na(evi_pixel_data_df$evi_raw)) %>%
        group_by(date) %>%
        summarise(meanEVI = mean(evi_raw))
      
      if ('hiq_evi' %in% selected_plots){
        if (length(unique(evi_pixel_data_df$evi_filtered)) == 1){
          selected_plots = selected_plots[ - which(selected_plots %in% 'hiq_evi')]
        }else {
          p_evi = evi_pixel_data_df %>%
            subset(evi_pixel_data_df$pixel %in% sd$Pixel) %>%
            select(pixel, date, evi_filtered) %>%
            mutate(pixel = paste0('EVI_high_', pixel), Date = date) %>%
            arrange(pixel, Date) %>%
            plot_ly(x = ~Date,
                    y = ~evi_filtered) %>%
            add_trace(
              mode = 'markers',
              type = "scatter",
              color = ~pixel,
              colors = c('light blue', 'blue'),
              marker = list(size = 5),
              showlegend = TRUE,
              legendgroup = ~pixel,
              text = ~paste("Date: ", Date,
                            '<br>Pixel: ', pixel,
                            '<br>Data: EVI')) %>%
            layout(xaxis = list(title = "Date"))
          
          ## New spline edits here
          if (length(unique(evi_pixel_data_df$evi_filtered)) >= 5){
            mEVIhq = evi_pixel_data_df %>%
              filter(!is.na(evi_pixel_data_df$evi_filtered)) %>%
              group_by(date) %>%
              summarise(meanEVI = mean(evi_filtered))
            
            # Loess evi df
            smooth_evi_df_hq = data.frame(date = as.Date(mEVIhq$date), evi_smooth = fitted(smooth.spline(mEVIhq$meanEVI~as.Date(mEVIhq$date)), data=mEVIhq))
            # Add loess curve
            p_evi= p_evi %>%
              add_trace(
                x = smooth_evi_df_hq$date, 
                y = smooth_evi_df_hq$evi_smooth,
                mode = "lines",
                line = list(width = 2, color = "rgb(120,120,120)"),
                name = "EVI loess HiQ",
                showlegend = TRUE) %>% layout(xaxis = list(title = "Date"))
          }
          
          p_evi = add_title_to_plot(df = p_evi,
                                    x_title_ = 'EVI (High Quality data)',
                                    y_title_ = 'EVI value')
        }
      }
      # EVI RAW
      if ('all_evi' %in% selected_plots){
        clean_evi_pixel_data_df =  evi_pixel_data_df %>%
          subset(evi_pixel_data_df$pixel %in% sd$Pixel) %>%
          mutate(pixel = paste0('EVI_all_', pixel), Date = date) %>%
          select(pixel, Date, evi_raw) %>%
          arrange(pixel, Date) 
        p_evi_raw = 
          plot_ly() %>%
          add_trace(
            data = clean_evi_pixel_data_df,
            x = ~Date,
            y = ~evi_raw,
            mode = 'markers',
            type = "scatter",
            color = ~pixel,
            colors = c('light blue', 'blue'),
            marker = list(size = 5),
            showlegend = TRUE,
            legendgroup = ~pixel,
            text = ~paste("Date: ", Date,
                          '<br>Pixel: ', pixel,
                          '<br>Data: EVI'))
        # Loess evi df
        smooth_evi_df = data.frame(date = as.Date(mEVI$date), evi_smooth = fitted(smooth.spline(mEVI$meanEVI~as.Date(mEVI$date)), data=mEVI))
        # Add loess curve
        p_evi_raw = p_evi_raw %>%
          add_trace(
            x = smooth_evi_df$date, 
            y = smooth_evi_df$evi_smooth,
            mode = "lines",
            line = list(width = 2, color = "rgb(120,120,120)"),
            name = "EVI loess All",
            showlegend = TRUE
          ) %>% layout(xaxis = list(title = "Date"))
        
        p_evi_raw = add_title_to_plot(df = p_evi_raw,
                                      x_title_ = 'EVI (All data)',
                                      y_title_ = 'EVI value')
      }
    }
 
    if ('GCC' %in% selected_data){
      # GCC FROM PHENOCAM
      if ('GCC' %in% selected_plots){
        gcc_p = add_title_to_plot(df = data$gcc_p,
                                  x_title_ = paste0('Phenocam Greenness (GCC) : ',as.character(input$pftSelection)),
                                  y_title_ = 'GCC value')
      }
    }
    
    
    if ('Transition Dates' %in% selected_data){
      if ('tds_sat' %in% selected_plots){
        # Grab Transition date data from df (and only pixels in dataframe below plots)
        tds_df = data$pixel_df_all_tds %>% subset(pixel %in% sd$Pixel)
        # Filter out NA values in the date field
        tds_df = tds_df[!is.na(tds_df$dates),]
        # If tds_df is empty, aka no Transition date data available, skip adding transition dates
        if (dim(tds_df)[1] == 0){
          print ('Empty transition data')
        }else {
          # Clean the Transition dates Dataframe to exclude the QA values
          tds_clean_df = tds_df %>% 
            subset(tds_df$layer != 'QA_Overall' & tds_df$layer != 'QA_Detailed') %>%
            dplyr::mutate(year = as.character(year))
          
          # Average the dates for each layer per year
          tds_averages_df = tds_clean_df %>% dplyr::group_by(layer, year) %>% summarise(mean(dates))
          
          # Add a fake avg_date field to be able to plot all the values on the same X axis (1 year)
          tds_averages_df['avg_date'] = as.Date(paste0( min(tds_averages_df$year) , '-' , format(tds_averages_df$`mean(dates)`,'%m-%d') ))
          
          # Order the tds_averages_df by the target char vector
          target = c('Greenup', 'Maturity', 'MidGreendown', 'MidGreenup', 'Peak', 'Senescence', 'Dormancy')
          tds_averages_df = left_join(data.frame(layer = target), tds_averages_df, by = 'layer')
          
          # Transition dates df
          tds_averages_df['avg_date'] = as.Date(format(tds_averages_df$`mean(dates)`,'%Y-%m-%d'))
          
          ##### COOL PLOT #### #### #### #### ####
          # Transition date plot for comparison across years:
          # n = length(unique(tds_averages_df$layer))
          # color_palette = colorRampPalette(c("green", "blue"))(n)
          # plot_ly(x = tds_averages_df$avg_date, y = tds_averages_df$year-2000,
          #   text = paste0(tds_averages_df$year), sort = FALSE,
          #   color = tds_averages_df$layer, colors=color_palette)
          #### #### #### #### #### #### #### ####
          tds_colors = c('Brown', 'Green', 'Dark Green', 'Dark Green', 'Light Green', 'Red', 'Orange')
          
          # Add transition dates to NDVI raw and high quality
          if ('NDVI' %in% selected_data){
            if ('hiq_ndvi' %in% selected_plots){
              if (length(unique(ndvi_pixel_data_df$ndvi_filtered)) == 1){
                selected_plots = selected_plots[ - which(selected_plots %in% 'hiq_ndvi')]
              }else {
                # Calculate y value for transition dates
                all_ndvi_hq_td = c()
                for (x in 1:length(tds_averages_df$avg_date)){
                  this_hq_ndvi = smooth_ndvi_df_hq$ndvi_smooth[birk::which.closest(tds_averages_df$avg_date[x], smooth_ndvi_df_hq$date)]
                  all_ndvi_hq_td = c(all_ndvi_hq_td, this_hq_ndvi)
                }
                tds_averages_df['hq_ndvi_avg'] = all_ndvi_hq_td
                
                p_ndvi = p_ndvi %>%
                  # Greenup
                  add_markers(x = subset(tds_averages_df, layer == 'Greenup')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Greenup')$hq_ndvi_avg, type = 'scatter',
                    marker = list(color = "#7FFF00", symbol = "circle", size = 10),
                    name = "Greenup") %>% 
                  # Maturity
                  add_markers(x = subset(tds_averages_df, layer == 'Maturity')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Maturity')$hq_ndvi_avg, type = 'scatter',
                    marker = list(color = "#66CD00", symbol = "square", size = 10),
                    name = "Maturity") %>% 
                  # Peak
                  add_markers(x = subset(tds_averages_df, layer == 'Peak')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Peak')$hq_ndvi_avg, type = 'scatter',
                    marker = list(color = "#458B00", symbol = "star", size = 10),
                    name = "Peak") %>% 
                  # MidGreendown
                  add_markers(x = subset(tds_averages_df, layer == 'MidGreendown')$avg_date, 
                    y = subset(tds_averages_df, layer == 'MidGreendown')$hq_ndvi_avg, type = 'scatter',
                    marker = list(color = "#FFB90F", symbol = "triange", size = 10),
                    name = "MidGreendown") %>% 
                  # Senescence
                  add_markers(x = subset(tds_averages_df, layer == 'Senescence')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Senescence')$hq_ndvi_avg, type = 'scatter',
                    marker = list(color = "#CD950C", symbol = "square", size = 10),
                    name = "Senescence") %>% 
                  # Dormancy
                  add_markers(x = subset(tds_averages_df, layer == 'Dormancy')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Dormancy')$hq_ndvi_avg, type = 'scatter',
                    marker = list(color = "Brown", symbol = "circle", size = 10),
                    name = "Dormancy")
              }
            }
            if ('all_ndvi' %in% selected_plots){
              all_ndvi_td = c()
              for (x in 1:length(tds_averages_df$avg_date)){
                this_all_ndvi = smooth_ndvi_df$ndvi_smooth[birk::which.closest(tds_averages_df$avg_date[x], smooth_ndvi_df$date)]
                all_ndvi_td = c(all_ndvi_td, this_all_ndvi)
              }
              tds_averages_df['all_ndvi_avg'] = all_ndvi_td
              
              p_ndvi_raw = p_ndvi_raw %>%
                # Greenup
                add_markers(x = subset(tds_averages_df, layer == 'Greenup')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Greenup')$all_ndvi_avg, type = 'scatter',
                  marker = list(color = "#7FFF00", symbol = "circle", size = 10),
                  name = "Greenup") %>% 
                # Maturity
                add_markers(x = subset(tds_averages_df, layer == 'Maturity')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Maturity')$all_ndvi_avg, type = 'scatter',
                  marker = list(color = "#66CD00", symbol = "square", size = 10),
                  name = "Maturity") %>% 
                # Peak
                add_markers(x = subset(tds_averages_df, layer == 'Peak')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Peak')$all_ndvi_avg, type = 'scatter',
                  marker = list(color = "#458B00", symbol = "star", size = 10),
                  name = "Peak") %>% 
                # MidGreendown
                add_markers(x = subset(tds_averages_df, layer == 'MidGreendown')$avg_date, 
                  y = subset(tds_averages_df, layer == 'MidGreendown')$all_ndvi_avg, type = 'scatter',
                  marker = list(color = "#FFB90F", symbol = "triange", size = 10),
                  name = "MidGreendown") %>% 
                # Senescence
                add_markers(x = subset(tds_averages_df, layer == 'Senescence')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Senescence')$all_ndvi_avg, type = 'scatter',
                  marker = list(color = "#CD950C", symbol = "square", size = 10),
                  name = "Senescence") %>% 
                # Dormancy
                add_markers(x = subset(tds_averages_df, layer == 'Dormancy')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Dormancy')$all_ndvi_avg, type = 'scatter',
                  marker = list(color = "Brown", symbol = "circle", size = 10),
                  name = "Dormancy")
            }
          }
          # Add transition dates to NDVI raw and high quality
          if ('EVI' %in% selected_data){
            if ('hiq_evi' %in% selected_plots){
              if (length(unique(evi_pixel_data_df$evi_filtered)) == 1){
                selected_plots = selected_plots[ - which(selected_plots %in% 'hiq_evi')]
              }else {
                # Calculate y value for transition dates
                all_evi_hq_td = c()
                for (x in 1:length(tds_averages_df$avg_date)){
                  this_hq_evi = smooth_evi_df_hq$evi_smooth[birk::which.closest(tds_averages_df$avg_date[x], smooth_evi_df_hq$date)]
                  all_evi_hq_td = c(all_evi_hq_td, this_hq_evi)
                }
                tds_averages_df['hq_evi_avg'] = all_evi_hq_td
                
                p_evi = p_evi %>%
                  # Greenup
                  add_markers(x = subset(tds_averages_df, layer == 'Greenup')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Greenup')$hq_evi_avg, type = 'scatter',
                    marker = list(color = "#7FFF00", symbol = "circle", size = 10),
                    name = "Greenup") %>% 
                  # Maturity
                  add_markers(x = subset(tds_averages_df, layer == 'Maturity')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Maturity')$hq_evi_avg, type = 'scatter',
                    marker = list(color = "#66CD00", symbol = "square", size = 10),
                    name = "Maturity") %>% 
                  # Peak
                  add_markers(x = subset(tds_averages_df, layer == 'Peak')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Peak')$hq_evi_avg, type = 'scatter',
                    marker = list(color = "#458B00", symbol = "star", size = 10),
                    name = "Peak") %>% 
                  # MidGreendown
                  add_markers(x = subset(tds_averages_df, layer == 'MidGreendown')$avg_date, 
                    y = subset(tds_averages_df, layer == 'MidGreendown')$hq_evi_avg, type = 'scatter',
                    marker = list(color = "#FFB90F", symbol = "triange", size = 10),
                    name = "MidGreendown") %>% 
                  # Senescence
                  add_markers(x = subset(tds_averages_df, layer == 'Senescence')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Senescence')$hq_evi_avg, type = 'scatter',
                    marker = list(color = "#CD950C", symbol = "square", size = 10),
                    name = "Senescence") %>% 
                  # Dormancy
                  add_markers(x = subset(tds_averages_df, layer == 'Dormancy')$avg_date, 
                    y = subset(tds_averages_df, layer == 'Dormancy')$hq_evi_avg, type = 'scatter',
                    marker = list(color = "Brown", symbol = "circle", size = 10),
                    name = "Dormancy")
              }
            }
            if ('all_evi' %in% selected_plots){
              all_evi_td = c()
              for (x in 1:length(tds_averages_df$avg_date)){
                this_all_evi = smooth_evi_df$evi_smooth[birk::which.closest(tds_averages_df$avg_date[x], smooth_evi_df$date)]
                all_evi_td = c(all_evi_td, this_all_evi)
              }
              tds_averages_df['all_evi_avg'] = all_evi_td
              
              p_evi_raw = p_evi_raw %>%
                # Greenup
                add_markers(x = subset(tds_averages_df, layer == 'Greenup')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Greenup')$all_evi_avg, type = 'scatter',
                  marker = list(color = "#7FFF00", symbol = "circle", size = 10),
                  name = "Greenup") %>% 
                # Maturity
                add_markers(x = subset(tds_averages_df, layer == 'Maturity')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Maturity')$all_evi_avg, type = 'scatter',
                  marker = list(color = "#66CD00", symbol = "square", size = 10),
                  name = "Maturity") %>% 
                # Peak
                add_markers(x = subset(tds_averages_df, layer == 'Peak')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Peak')$all_evi_avg, type = 'scatter',
                  marker = list(color = "#458B00", symbol = "star", size = 10),
                  name = "Peak") %>% 
                # MidGreendown
                add_markers(x = subset(tds_averages_df, layer == 'MidGreendown')$avg_date, 
                  y = subset(tds_averages_df, layer == 'MidGreendown')$all_evi_avg, type = 'scatter',
                  marker = list(color = "#FFB90F", symbol = "triange", size = 10),
                  name = "MidGreendown") %>% 
                # Senescence
                add_markers(x = subset(tds_averages_df, layer == 'Senescence')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Senescence')$all_evi_avg, type = 'scatter',
                  marker = list(color = "#CD950C", symbol = "square", size = 10),
                  name = "Senescence") %>% 
                # Dormancy
                add_markers(x = subset(tds_averages_df, layer == 'Dormancy')$avg_date, 
                  y = subset(tds_averages_df, layer == 'Dormancy')$all_evi_avg, type = 'scatter',
                  marker = list(color = "Brown", symbol = "circle", size = 10),
                  name = "Dormancy")
            }
          }
        }
      }
    }
    
    
    print (selected_data)
    print (selected_plots)

    vector_length = length(selected_plots)
    if ('tds_sat' %in% selected_plots){
      vector_length = vector_length - 1
    }
  
    plot_list = vector('list', vector_length)
    count = 0
    for (i in selected_plots){
      count = count + 1
      print (i)
      if (i == 'GCC'){
        plot_list[[count]] = gcc_p
      }
      if (i =='hiq_ndvi'){
        plot_list[[count]] = p_ndvi
      }
      if (i =='all_ndvi'){
        plot_list[[count]] = p_ndvi_raw
      }
      if (i =='hiq_evi'){
        plot_list[[count]] = p_evi
      }
      if (i =='all_evi'){
        plot_list[[count]] = p_evi_raw
      }
    }
    
    vector_length = length(plot_list)
    length_ = 250 * vector_length

    p = subplot(plot_list, nrows = length(plot_list), shareX = TRUE)
    p  %>% plotly::config(displaylogo = FALSE,
                  modeBarButtonsToRemove = list(
                    'sendDataToCloud',
                    'autoScale2d',
                    'resetScale2d',
                    'hoverClosestCartesian',
                    'hoverCompareCartesian',
                    'toggleSpikelines',
                    'lasso2d',
                    'select2d')) %>%
      layout(height = length_, inline = TRUE)
    
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
              if(input$highlightPixelModeNDVI == TRUE){
                pt_merc = from_crs1_to_crs2_lon_lat(lon_ = lon_, lat_ = lat_, from_crs = wgs_crs, to_crs = merc_crs)
                lat_merc = pt_merc@coords[2]
                lng_merc = pt_merc@coords[1]
                # showpos(x = lng_merc , y = lat_merc, site, data$r_ndvi_cropped, '250m')
                showpos(x = lng_merc , y = lat_merc, site, data$r_landcover)
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
    variables$color_count = 1
    variables$color_list = c()
    variables$color_list_reserve = rainbow(20)
    
    data$pixel_df    = setNames(data.frame(matrix(ncol = 5, nrow = 0)), c("Pixel", "Site", "lat", 'lon', 'pft'))
    data$pixel_sps_250m = SpatialPolygons(list())
    data$midcell_pixel_sin = SpatialPoints(data.frame(x = 0, y = 0), proj4string=CRS(sinu_crs))[-1,]
    shinyjs::hide(id = 'clearPixels')
  })

  # Checkbox for deleting data
  observe({
    delete_data = input$boolDeleteData
    if (delete_data){
      shinyjs::show(id = 'removeCachedData')
    } else{
      shinyjs::hide(id = 'removeCachedData')
    }
  })

  # Refresh data
  observeEvent(input$removeCachedData,{
    site           = input$site
    # Remove the site directory recursively
    dir_to_remove          = paste0('./www/site_data/', site)
    print (paste0('Removing this directory: ', dir_to_remove))
    unlink(dir_to_remove, recursive=TRUE)
    shinyBS::toggleModal(session, 'removeCachedDataModal', toggle = 'close')
    updateCheckboxInput(session, 'boolDeleteData', value = FALSE)
  })
  
  
  # Download/get data for desired phenocam/satellite products
  observeEvent(input$getDataButton, {

    withBusyIndicatorServer("getDataButton", {

    site           = input$site
    site_data      = get_site_info(site)
    selected_data  = input$dataTypes_get
    data_options   = c('NDVI', 'EVI', 'GCC', 'Transition Dates', 'NPN')

    file_path          = paste0('./www/site_data/', site, '/data_layers/')
    ndvi_filepath      = paste0(file_path,'ndvi/')
    ndvi_tera_filepath = paste0(ndvi_filepath, 'tera/')
    ndvi_aqua_filepath = paste0(ndvi_filepath, 'aqua/')
    evi_filepath       = paste0(file_path,'evi/')
    evi_tera_filepath  = paste0(evi_filepath, 'tera/')
    evi_aqua_filepath  = paste0(evi_filepath, 'aqua/')
    tds_filepath       = paste0(file_path,'tds/')
    gcc_filepath       = paste0(file_path,'gcc/')

    freq           = as.numeric(substring(input$phenocamFrequency, 1, 1))
    percentile_gcc = 90

    # shinyjs::hide(id = 'getData')

    # shinyBS::toggleModal(session, 'getDataPopup', toggle = 'close')

    print ('Importing data for:')
    print (selected_data)

    # Set up directories to store data
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
      dir.create(file.path(ndvi_filepath))
    }
    if (!file.exists(ndvi_tera_filepath) & 'NDVI' %in% selected_data){
      dir.create(file.path(ndvi_tera_filepath))
    }
    if (!file.exists(ndvi_aqua_filepath) & 'NDVI' %in% selected_data){
      dir.create(file.path(ndvi_aqua_filepath))
    }
    if (!file.exists(evi_filepath) & 'EVI' %in% selected_data){
      dir.create(file.path(evi_filepath))
    }
    if (!file.exists(evi_tera_filepath) & 'EVI' %in% selected_data){
      dir.create(file.path(evi_tera_filepath))
    }
    if (!file.exists(evi_aqua_filepath) & 'EVI' %in% selected_data){
      dir.create(file.path(evi_aqua_filepath))
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
        incProgress(amount = .3, detail = 'Downloading NPN')
        data$npn_brick = download_npn_brick(tmp_name = paste0(npn_grid_dir,'/deleteme_npn_grid_'),
                                            out_file = npn_file_name,
                                            layer    = 'si-x:average_leaf_prism')
      }else {
        incProgress(amount = .2, detail = 'Importing NPN')
        data$npn_brick = raster::brick(npn_file_name)
      }
      }) #END WITH PROGRESS BAR
    } #END IMPORT NPN


    # Import [Transition Dates] netcdfs
    # ------------------------------------------------------------------------
    if ('Transition Dates' %in% selected_data){
      withProgress(message = 'Importing Transition Dates', value = .1, {
        print ('Importing Transition Dates')
        appeears$tds  = get_appeears_task(site, type = 'tds')

        if (length(list.files(tds_filepath))==0){
          tds_bundle_df = download_bundle_file(appeears$tds$task_id, tds_filepath)
          tds_name = subset(tds_bundle_df, file_type == 'nc')$file_name
        }else {
          # tds_bundle_df = get_appeears_bundle_df(appeears$tds$task_id)
          tds_files = list.files(tds_filepath)
          tds_name  = tds_files[grepl('MCD12Q2.006_500m_aid0001.nc', tds_files)]
        }
        incProgress(amount = .1)
        # tds_name = subset(tds_bundle_df, file_type == 'nc')$file_name
        data$tds_path = paste0(tds_filepath, tds_name)
        data$tds_nc    = nc_open(data$tds_path)
        incProgress(amount = .1)
        lon_td = ncvar_get(data$tds_nc, "xdim")
        nlon = length(lon_td)
        lat_td = ncvar_get(data$tds_nc, "ydim")
        nlat = length(lat_td)

        incProgress(amount = .1)
        xmin = lon_td[1]
        xmax = lon_td[length(lon_td)]
        ymin = lat_td[length(lat_td)]
        ymax = lat_td[1]

        incProgress(amount = .1)
        td_v6_names = c('Dormancy', 'Greenup', 'Maturity', 'MidGreendown', 'MidGreenup', 'Peak', 'Senescence','QA_Overall', 'QA_Detailed')
        td_v6_ncs   = list()
        for (name in td_v6_names){
          array = ncvar_get(data$tds_nc, name)
          this_layer = raster::brick(data$tds_path, varname=name)
          this_layer_1 = setExtent(this_layer, extent(xmin,xmax,ymin,ymax))
          dim(this_layer_1) = c(dim(array)[3],dim(array)[2],dim(array)[4])
          crs(this_layer_1) = sinu_crs
          this_layer_2 = setValues(this_layer_1, values = array[1,,,])
          td_v6_ncs[name] = this_layer_2
        }
        incProgress(amount = .2)
        data$td_v6_ncs = td_v6_ncs

      }) #END WITH PROGRESS BAR
    } #END IMPORT TRANSITION DATES


      # Import [NDVI]
      #------------------------------------------------------------------------
    if ('NDVI' %in% selected_data){
      withProgress(message = 'Importing NDVI', value = .2, {

      print ('Importing NDVI')
      # Bring in tera and aqua data
      appeears$ndvi_aqua = get_appeears_task(site, type = 'ndvi_aqua')
      appeears$ndvi_tera = get_appeears_task(site, type = 'ndvi_tera')
      # appeears$ndvi  = get_appeears_task(site, type = 'ndvi')

      print (appeears$ndvi_aqua)
      print (appeears$ndvi_tera)

      if (length(list.files(ndvi_tera_filepath))==0){
        incProgress(amount = .1, detail = 'Downloading NDVI TERA')
        ndvi_bundle_df_tera = download_bundle_file(appeears$ndvi_tera$task_id, ndvi_tera_filepath)
        ndvi_tera_name     = subset(ndvi_bundle_df_tera, file_type == 'nc')$file_name
        ndvi_qc_tera_name  = ndvi_bundle_df_tera[grep('Quality-lookup', ndvi_bundle_df_tera$file_name),]$file_name
      }else {
        incProgress(amount = .1, detail = 'Importing NDVI TERA')
        # ndvi_bundle_df_tera = get_appeears_bundle_df(appeears$ndvi_tera$task_id)
        ndvi_files_t = list.files(ndvi_tera_filepath)
        ndvi_tera_name  = ndvi_files_t[grepl('MOD13Q1.006_250m_aid0001.nc', ndvi_files_t)]
        ndvi_qc_tera_name = ndvi_files_t[grepl('Quality-lookup.csv', ndvi_files_t)]
      }

      if (length(list.files(ndvi_aqua_filepath))==0){
        incProgress(amount = .1, detail = 'Downloading NDVI AQUA')
        ndvi_bundle_df_aqua = download_bundle_file(appeears$ndvi_aqua$task_id, ndvi_aqua_filepath)
        ndvi_aqua_name     = subset(ndvi_bundle_df_aqua, file_type == 'nc')$file_name
        ndvi_qc_aqua_name  = ndvi_bundle_df_aqua[grep('Quality-lookup', ndvi_bundle_df_aqua$file_name),]$file_name
      }else {
        incProgress(amount = .1, detail = 'Importing NDVI AQUA')
        # ndvi_bundle_df_aqua = get_appeears_bundle_df(appeears$ndvi_aqua$task_id)
        ndvi_files_a = list.files(ndvi_aqua_filepath)
        ndvi_aqua_name  = ndvi_files_a[grepl('MYD13Q1.006_250m_aid0001.nc', ndvi_files_a)]
        ndvi_qc_aqua_name = ndvi_files_a[grepl('Quality-lookup.csv', ndvi_files_a)]
      }

      incProgress(amount = .1, detail = 'Processing NDVI')
      # TERA data (ndvi)
      # ndvi_tera_name     = subset(ndvi_bundle_df_tera, file_type == 'nc')$file_name
      ndvi_tera_path     = paste0(ndvi_tera_filepath, ndvi_tera_name)
      # ndvi_qc_tera_name  = ndvi_bundle_df_tera[grep('Quality-lookup', ndvi_bundle_df_tera$file_name),]$file_name
      ndvi_qc_tera_path  = paste0(ndvi_tera_filepath, ndvi_qc_tera_name)
      # bricks
      data$ndvi_tera_brick    = raster::brick(ndvi_tera_path, varname='_250m_16_days_NDVI',  crs = sinu_crs)
      data$ndvi_qc_tera_brick = raster::brick(ndvi_tera_path, varname='_250m_16_days_VI_Quality',  crs = sinu_crs)
      data$ndvi_qc_csv_tera   = read.csv(ndvi_qc_tera_path)

      incProgress(amount = .1)
      # AQUA data (ndvi)
      # ndvi_aqua_name     = subset(ndvi_bundle_df_aqua, file_type == 'nc')$file_name
      ndvi_aqua_path     = paste0(ndvi_aqua_filepath, ndvi_aqua_name)
      # ndvi_qc_aqua_name  = ndvi_bundle_df_aqua[grep('Quality-lookup', ndvi_bundle_df_aqua$file_name),]$file_name
      ndvi_qc_aqua_path  = paste0(ndvi_aqua_filepath, ndvi_qc_aqua_name)
      # bricks
      data$ndvi_aqua_brick    = raster::brick(ndvi_aqua_path, varname='_250m_16_days_NDVI',  crs = sinu_crs)
      data$ndvi_qc_aqua_brick = raster::brick(ndvi_aqua_path, varname='_250m_16_days_VI_Quality',  crs = sinu_crs)
      data$ndvi_qc_csv_aqua   = read.csv(ndvi_qc_aqua_path)

      incProgress(amount = .1)
      # Grab first observation of NDVI and Quality datasets
      r_for_grid = raster::raster(ndvi_tera_path,  crs = sinu_crs)
      r_for_grid_merc = projectRaster(from = r_for_grid, crs = merc_crs, res = 231.6563582638875)
      r_for_grid_cropped_merc = crop_raster(data$lat_merc, data$lng_merc, r_for_grid_merc, height = 10000, width = 10000, crs_str = merc_crs)
      data$r_ndvi_cropped = r_for_grid_cropped_merc

      incProgress(amount = .1)

      grid = build_raster_grid(r_for_grid_cropped_merc, map = 'map', crs='merc')
      data$raster_grid = TRUE

      # ADD NLCD back in after building grid
      if (data$NLCD){
        leafletProxy('map') %>% addRasterImage(data$rc_nlcd, colors = data$rc_nlcd_c$palette, opacity = .7, group = '2016 NLCD') %>%
          addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
            overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '2016 NLCD', '250m MODIS Grid'),
            position = c("topleft"),
            options = layersControlOptions(collapsed = FALSE))
      }

      shinyjs::show(id = 'highlightPixelModeNDVI')
      updateCheckboxInput(session, 'highlightPixelModeNDVI', value = TRUE)

      }) #END WITH PROGRESS BAR
    } #END IMPORT NDVI


    #   # Import [EVI] netcdf(evi) and csv(qa)
    #   #------------------------------------------------------------------------
    if ('EVI' %in% selected_data){
      withProgress(message = 'Importing EVI', value = .2, {
      print ('Importing EVI')

      appeears$evi_aqua = get_appeears_task(site, type = 'evi_aqua')
      appeears$evi_tera = get_appeears_task(site, type = 'evi_tera')
      # appeears$evi  = get_appeears_task(site, type = 'evi')

      print (as_tibble(appeears$evi_aqua))
      print (as_tibble(appeears$evi_tera))

      if (length(list.files(evi_tera_filepath))==0){
        incProgress(amount = .2, detail = 'Downloading EVI TERA')
        evi_bundle_df_tera = download_bundle_file(appeears$evi_tera$task_id, evi_tera_filepath)
        evi_tera_name     = subset(evi_bundle_df_tera, file_type == 'nc')$file_name
        evi_qc_tera_name  = evi_bundle_df_tera[grep('Quality-lookup', evi_bundle_df_tera$file_name),]$file_name
      }else {
        incProgress(amount = .2, detail = 'Importing EVI TERA')
        # evi_bundle_df_tera = get_appeears_bundle_df(appeears$evi_tera$task_id)
        evi_files_t = list.files(evi_tera_filepath)
        evi_tera_name  = evi_files_t[grepl('MOD13Q1.006_250m_aid0001.nc', evi_files_t)]
        evi_qc_tera_name = evi_files_t[grepl('Quality-lookup.csv', evi_files_t)]
      }

      if (length(list.files(evi_aqua_filepath))==0){
        incProgress(amount = .2, detail = 'Downloading EVI AQUA')
        evi_bundle_df_aqua = download_bundle_file(appeears$evi_aqua$task_id, evi_aqua_filepath)
        evi_aqua_name     = subset(evi_bundle_df_aqua, file_type == 'nc')$file_name
        evi_qc_aqua_name  = evi_bundle_df_aqua[grep('Quality-lookup', evi_bundle_df_aqua$file_name),]$file_name
      }else {
        incProgress(amount = .2, detail = 'Importing EVI AQUA')
        # evi_bundle_df_aqua = get_appeears_bundle_df(appeears$evi_aqua$task_id)
        evi_files_a = list.files(evi_aqua_filepath)
        evi_aqua_name  = evi_files_a[grepl('MYD13Q1.006_250m_aid0001.nc', evi_files_a)]
        evi_qc_aqua_name = evi_files_a[grepl('Quality-lookup.csv', evi_files_a)]
      }

      # print (as_tibble(evi_bundle_df_tera))
      # print (as_tibble(evi_bundle_df_aqua))

      incProgress(amount = .1, detail = 'Processing EVI')
      # TERA data (evi)
      # evi_tera_name     = subset(evi_bundle_df_tera, file_type == 'nc')$file_name
      evi_tera_path     = paste0(evi_tera_filepath, evi_tera_name)
      # evi_qc_tera_name  = evi_bundle_df_tera[grep('Quality-lookup', evi_bundle_df_tera$file_name),]$file_name
      evi_qc_tera_path  = paste0(evi_tera_filepath, evi_qc_tera_name)
      # bricks
      data$evi_tera_brick    = raster::brick(evi_tera_path, varname='_250m_16_days_EVI', crs = sinu_crs)
      data$evi_qc_tera_brick = raster::brick(evi_tera_path, varname='_250m_16_days_VI_Quality', crs = sinu_crs)
      data$evi_qc_csv_tera   = read.csv(evi_qc_tera_path)

      incProgress(amount = .1)
      # AQUA data (evi)
      # evi_aqua_name     = subset(evi_bundle_df_aqua, file_type == 'nc')$file_name
      evi_aqua_path     = paste0(evi_aqua_filepath, evi_aqua_name)
      # evi_qc_aqua_name  = evi_bundle_df_aqua[grep('Quality-lookup', evi_bundle_df_aqua$file_name),]$file_name
      evi_qc_aqua_path  = paste0(evi_aqua_filepath, evi_qc_aqua_name)
      # bricks
      data$evi_aqua_brick    = raster::brick(evi_aqua_path, varname='_250m_16_days_EVI', crs = sinu_crs)
      data$evi_qc_aqua_brick = raster::brick(evi_aqua_path, varname='_250m_16_days_VI_Quality', crs = sinu_crs)
      data$evi_qc_csv_aqua   = read.csv(evi_qc_aqua_path)

      incProgress(amount = .1)
      # Builds the Grid for EVI when NDVI is not imported
      if ('NDVI' %!in% selected_data){
        # Grab first observation of evi and Quality datasets
        r_for_grid = raster::raster(evi_tera_path, crs = sinu_crs)
        crs(r_for_grid) = sinu_crs
        r_for_grid_merc = projectRaster(from = r_for_grid, crs = merc_crs, res = 231.6563582638875)
        r_for_grid_cropped_merc = crop_raster(data$lat_merc, data$lng_merc, r_for_grid_merc, height = 10000, width = 10000, crs_str = merc_crs)
        data$r_evi_cropped = r_for_grid_cropped_merc

        incProgress(amount = .1)
        grid = build_raster_grid(r_for_grid_cropped_merc, map = 'map', crs='merc')
        data$raster_grid = TRUE

        # ADD NLCD back in after building grid
        if (data$NLCD){
          leafletProxy('map') %>% addRasterImage(data$rc_nlcd, colors = data$rc_nlcd_c$palette, opacity = .7, group = '2016 NLCD') %>%
            addLayersControl(baseGroups = c("World Imagery", "Open Topo Map"),
              overlayGroups = c('MODIS Land Cover 2016', 'Vegetation Cover Agreement', '2016 NLCD', '250m MODIS Grid'),
              position = c("topleft"),
              options = layersControlOptions(collapsed = FALSE))
        }

        shinyjs::show(id = 'highlightPixelModeNDVI')
        updateCheckboxInput(session, 'highlightPixelModeNDVI', value = TRUE)
      }
      }) #END WITH PROGRESS BAR
    } #END IMPORT EVI


    # Import [GCC] splined Data from phenocam (csv)
    #------------------------------------------------------------------------
    if ('GCC' %in% selected_data){
      withProgress(message = 'Importing GCC', value = .1, {
      phenocam$gcc_all = list()
      file_path = gcc_filepath
      pc_metrics = 'gcc_90'
      frequency = 3

      pft_abbr = data$pft_abbr
      # How many days old the GCC must be to refresh (re-download) it from phenocam
      refresh_at = 7
      todays_date = Sys.Date()

      # Grab all pfts at this site
      pft_abbrs = as.character(subset(pft_df, pft_df$pft_expanded %in% data$veg_types)$pft_abbreviated)

      # Loop through pfts to save out the data
      for (pft_ in pft_abbrs){

        gcc_filepath_1day    = paste0(file_path, 'gcc_',pft_, '_1_day', '.csv')
        spring_filepath_1day = paste0(file_path, 'gcc_',pft_, '_1_day_spring_tds', '.csv')
        fall_filepath_1day   = paste0(file_path, 'gcc_',pft_, '_1_day_fall_tds', '.csv')

        gcc_filepath_3day    = paste0(file_path, 'gcc_',pft_, '_3_day', '.csv')
        spring_filepath_3day = paste0(file_path, 'gcc_',pft_, '_3_day_spring_tds', '.csv')
        fall_filepath_3day   = paste0(file_path, 'gcc_',pft_, '_3_day_fall_tds', '.csv')

        # Add ping phenocam api to make sure it's up?
        # Catch data that is older than the Refresh rate, Delete it if it is and re-download it
        if (file.exists(gcc_filepath)){
          print ('enter 1')
          these_gccs = c(gcc_filepath_1day, spring_filepath_1day, fall_filepath_1day,
            gcc_filepath_3day, spring_filepath_3day, fall_filepath_3day)
          dates_modified = as.Date(file.info(these_gccs)$mtime)
          # If one of the dates is out of sink, rm all 3 and redownload
          if (length(unique(dates_modified)) > 1){
            print ('Will Download GCC from Phenocam API.')
            file.remove(these_gccs)
            # Download gcc back
            incProgress(amount = .2, detail = 'Downloading GCC data')

            # Download phenocam 1_day, 3_day gcc and tds data
            site_pc_data = get_site_roi_csvs(name = site,
              roi_files_ = roi_files,
              metrics_ = pc_metrics)

            # Subsetting data for 1 and 3 day at this pft
            gcc_1day  = site_pc_data[paste0(pft_, '_gcc_1day')][[1]]
            tds_1day = site_pc_data[paste0(pft_, '_tds_1day')][[1]]
            spring_1day = subset(tds_1day, tds_1day$direction == 'rising')
            fall_1day   = subset(tds_1day, tds_1day$direction == 'falling')

            gcc_3day  = site_pc_data[paste0(pft_, '_gcc_3day')][[1]]
            tds_3day = site_pc_data[paste0(pft_, '_tds_3day')][[1]]
            spring_3day = subset(tds_3day, tds_3day$direction == 'rising')
            fall_3day   = subset(tds_3day, tds_3day$direction == 'falling')

            # Save global variable for phenocam data
            phenocam$gcc_all[[pft_]] = list(gcc_1day    = gcc_1day,
                                            spring_1day = spring_1day,
                                            fall_1day   = fall_1day,
                                            gcc_3day    = gcc_3day,
                                            spring_3day = spring_3day,
                                            fall_3day   = fall_3day)

            # Write out the phenocam gcc data
            write.csv(gcc_1day,    file = gcc_filepath_1day)
            write.csv(spring_1day, file = spring_filepath_1day)
            write.csv(fall_1day,   file = fall_filepath_1day)
            write.csv(gcc_3day,    file = gcc_filepath_3day)
            write.csv(spring_3day, file = spring_filepath_3day)
            write.csv(fall_3day,   file = fall_filepath_3day)

          # Remove files if they are older than the refresh_at value (7 days) and redownload
          }else if(length(these_gccs[dates_modified < (todays_date - refresh_at)]) > 0){
            print ('Will Download GCC from Phenocam API.')
            file.remove(these_gccs[dates_modified < (todays_date - refresh_at)])
            # Download phenocam 1_day, 3_day gcc and tds data
            site_pc_data = get_site_roi_csvs(name = site,
                                            roi_files_ = roi_files,
                                            metrics_ = pc_metrics)
            incProgress(amount = .2, detail = 'Downloading GCC data')

            # Subsetting data for 1 and 3 day at this pft
            gcc_1day  = site_pc_data[paste0(pft_, '_gcc_1day')][[1]]
            tds_1day = site_pc_data[paste0(pft_, '_tds_1day')][[1]]
            spring_1day = subset(tds_1day, tds_1day$direction == 'rising')
            fall_1day   = subset(tds_1day, tds_1day$direction == 'falling')

            gcc_3day  = site_pc_data[paste0(pft_, '_gcc_3day')][[1]]
            tds_3day = site_pc_data[paste0(pft_, '_tds_3day')][[1]]
            spring_3day = subset(tds_3day, tds_3day$direction == 'rising')
            fall_3day   = subset(tds_3day, tds_3day$direction == 'falling')

            # Save global variable for phenocam data
            phenocam$gcc_all[[pft_]] = list(gcc_1day    = gcc_1day,
              spring_1day = spring_1day,
              fall_1day   = fall_1day,
              gcc_3day    = gcc_3day,
              spring_3day = spring_3day,
              fall_3day   = fall_3day)

            # Write out the phenocam gcc data
            write.csv(gcc_1day,    file = gcc_filepath_1day)
            write.csv(spring_1day, file = spring_filepath_1day)
            write.csv(fall_1day,   file = fall_filepath_1day)
            write.csv(gcc_3day,    file = gcc_filepath_3day)
            write.csv(spring_3day, file = spring_filepath_3day)
            write.csv(fall_3day,   file = fall_filepath_3day)

            # If dates are current, just use local gcc files
          }else {
            print (paste0('Will import GCC from local file. Current within ', refresh_at, ' days.'))
            incProgress(amount = .2, detail = 'Importing GCC CSV')

            gcc_1day = read.csv(gcc_filepath_1day, stringsAsFactors = FALSE)
            spring_1day = read.csv(spring_filepath_1day, stringsAsFactors = FALSE)
            fall_1day = read.csv(fall_filepath_1day, stringsAsFactors = FALSE)
            gcc_3day = read.csv(gcc_filepath_3day, stringsAsFactors = FALSE)
            spring_3day = read.csv(spring_filepath_3day, stringsAsFactors = FALSE)
            fall_3day = read.csv(fall_filepath_3day, stringsAsFactors = FALSE)

            phenocam$gcc_all[[pft_]] = list(gcc_1day    = gcc_1day,
                                            spring_1day = spring_1day,
                                            fall_1day   = fall_1day,
                                            gcc_3day    = gcc_3day,
                                            spring_3day = spring_3day,
                                            fall_3day   = fall_3day)
          }

          # Download gcc data if it doesn't exist at all
        }else{
          incProgress(amount = .2, detail = 'Downloading GCC data')
          print ('Will Download GCC from Phenocam API.')

          # Download phenocam 1_day, 3_day gcc and tds data
          site_pc_data = get_site_roi_csvs(name = site,
            roi_files_ = roi_files,
            metrics_ = pc_metrics)
          # Subsetting data for 1 and 3 day at this pft
          gcc_1day  = site_pc_data[paste0(pft_, '_gcc_1day')][[1]]
          tds_1day = site_pc_data[paste0(pft_, '_tds_1day')][[1]]
          spring_1day = subset(tds_1day, tds_1day$direction == 'rising')
          fall_1day   = subset(tds_1day, tds_1day$direction == 'falling')

          gcc_3day  = site_pc_data[paste0(pft_, '_gcc_3day')][[1]]
          tds_3day = site_pc_data[paste0(pft_, '_tds_3day')][[1]]
          spring_3day = subset(tds_3day, tds_3day$direction == 'rising')
          fall_3day   = subset(tds_3day, tds_3day$direction == 'falling')

          # Save global variable for phenocam data
          phenocam$gcc_all[[pft_]] = list(gcc_1day    = gcc_1day,
                                          spring_1day = spring_1day,
                                          fall_1day   = fall_1day,
                                          gcc_3day    = gcc_3day,
                                          spring_3day = spring_3day,
                                          fall_3day   = fall_3day)

          # Write out the phenocam gcc data
          write.csv(gcc_1day,    file = gcc_filepath_1day)
          write.csv(spring_1day, file = spring_filepath_1day)
          write.csv(fall_1day,   file = fall_filepath_1day)
          write.csv(gcc_3day,    file = gcc_filepath_3day)
          write.csv(spring_3day, file = spring_filepath_3day)
          write.csv(fall_3day,   file = fall_filepath_3day)
        }
      } # PFT loop
      # Show frequency selection
      shinyjs::show(id = 'phenocamFrequency')

      }) #END WITH PROGRESS BAR
    } #END IMPORT GCC

    # Update plot data input to only include data that has been imported for this site
    updateSelectInput(session, 'dataTypes_plot', choices = selected_data, selected = selected_data)
    # Toggle get data popup off
    shinyBS::toggleModal(session, 'getDataPopup', toggle = 'close')
    # Show plot data button if NDVI, EVI, or GCC are imported
    if ('GCC' %in% selected_data | 'NDVI' %in% selected_data | 'EVI' %in% selected_data){
      shinyjs::show(id = 'plotRemoteData')
    }
    print (selected_data)

    }) #END BUSY INDICATOR
  }) #END GET DATA OBSERVER
  
  
  # Observer for the popup
  observeEvent(input$plotRemoteData, {
    shinyjs::hide(id = 'noPixelWarning')
    shinyjs::hide(id = 'buildingPlot')
    shinyjs::hide(id = 'doneBuildingPlot')
    shinyjs::show(id = 'pixelTypes')
    sm_pixels = data$pixel_sps_250m
    types = input$dataTypes_get

    if (is.null(sm_pixels@polygons[1][[1]])){
      print ('no pixels selected')
      shinyjs::hide(id = 'pixelTypes')
      if ('GCC' %in% types){
        updateSelectInput(session, 'dataTypes_plot', choices ='GCC', selected = 'GCC')
      }else {
        updateSelectInput(session, 'dataTypes_plot', choices ='No Data Available')
      }
    } else{
      updateSelectInput(session, 'dataTypes_plot', choices = types, selected=types)
    }
  })
  
  observeEvent(input$dataTypes_plot, {
    types = input$dataTypes_plot
    print (types)
    if ('NDVI' %in% types | 'EVI' %in% types |'Transition Dates' %in% types |'GCC' %in% types |'NPN' %in% types){
      shinyjs::show(id = 'plotDataButton')
      if (length(types)==1 & types[1]=='Transition Dates'){
        shinyjs::hide(id = 'plotDataButton')
      }
      if (length(types)==1 & types[1]=='NPN'){
        shinyjs::hide(id = 'plotDataButton')
      }
      if (length(types)==2 & 'NPN' %in% types &'Transition Dates' %in% types){
        shinyjs::hide(id = 'plotDataButton')
      }
    } else {
      shinyjs::hide(id = 'plotDataButton')
    }
  })

  output$downloadDataButton = downloadHandler(
    filename = function() {
      if (input$dataTypes_download == 'NDVI'){
        paste0(input$site, '_ndvi_data.csv')
      } else if (input$dataTypes_download == 'EVI'){
        paste0(input$site, '_evi_data.csv')
      }else if (input$dataTypes_download == 'GCC Data'){
        paste0(input$site, '_gcc_data_', data$pft_abbr ,'.csv')
      }else if (input$dataTypes_download == 'GCC Spring Transition Dates'){
        paste0(input$site, '_gcc_spring_td_', data$pft_abbr ,'.csv')
      }else if (input$dataTypes_download == 'GCC Fall Transition Dates'){
        paste0(input$site, '_gcc_fall_td_', data$pft_abbr ,'.csv')
      }else if (input$dataTypes_download == 'MODIS Transition Dates'){
        paste0(input$site, '_tds_data.csv')
      }else if (input$dataTypes_download == 'Selected Pixel CSV')
        paste0(input$site, '_selected_pixels.csv')
    },
    content = function(file) {
      if (input$dataTypes_download == 'NDVI'){
        data = data$ndvi_pixels
      } else if (input$dataTypes_download == 'EVI'){
        data = data$evi_pixels
      }else if (input$dataTypes_download == 'GCC Data'){
        data = phenocam$gcc_all[[data$pft_abbr]]$gcc
      }else if (input$dataTypes_download == 'GCC Spring Transition Dates'){
        data = phenocam$gcc_all[[data$pft_abbr]]$spring
      }else if (input$dataTypes_download == 'GCC Fall Transition Dates'){
        data = phenocam$gcc_all[[data$pft_abbr]]$fall
      }else if (input$dataTypes_download == 'MODIS Transition Dates'){
        data = data$pixel_df_all_tds
      }else if (input$dataTypes_download == 'Selected Pixel CSV'){
        data = data$pixel_df
      }
      write.csv(data, file, row.names = FALSE)
    }
  )
  

  #--------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------------------------
  #  FUNCTIONS
  #--------------------------------------------------------------------------------------------------------------------------------------
  #--------------------------------------------------------------------------------------------------------------------------------------

  # Creates a polyline surrounding any MODIS 2016 or 250m pixel from cropped raster
  showpos = function(x=NULL, y=NULL, name, raster_, type_= '250m', map_ = NULL) {
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

       # Mercator Coordinates
       xclose_merc = ((col - 1) * resolution) + xmin
       xfar_merc   = (col * resolution) + xmin
       yclose_merc = -((row - 1) * resolution) + ymax
       yfar_merc   = -(row * resolution) + ymax

       midcellx_merc = xclose_merc + (resolution * .5)
       midcelly_merc = yclose_merc - (resolution * .5)
       midcell_merc  = c(midcellx_merc, midcelly_merc)
       
       datalon_merc = c(xclose_merc, xfar_merc, xfar_merc, xclose_merc ,xclose_merc)
       datalat_merc = c(yclose_merc, yclose_merc, yfar_merc, yfar_merc, yclose_merc)
       
       # Sinusoidal coordinates
       midcell_pt_sin  = from_crs1_to_crs2_lon_lat(lon_ = midcellx_merc, lat_ = midcelly_merc, from_crs = merc_crs, to_crs = sinu_crs)
       midcellx_sin = midcell_pt_sin@coords[1]
       midcelly_sin = midcell_pt_sin@coords[2]
       midcell_sin  = c(midcellx_sin, midcelly_sin)
       
       xyclosefar_pts_sin = from_crs1_to_crs2_lon_lat(lon_ = c(xclose_merc,xfar_merc), lat_ = c(yclose_merc,yfar_merc), from_crs = merc_crs, to_crs = sinu_crs)
       xclose_sin = xyclosefar_pts_sin@coords[1,1]
       xfar_sin   = xyclosefar_pts_sin@coords[2,1]
       yclose_sin = xyclosefar_pts_sin@coords[1,2]
       yfar_sin   = xyclosefar_pts_sin@coords[2,2]
       
       # WGS coordinates
       midcell_pt_wgs  = from_crs1_to_crs2_lon_lat(lon_ = midcellx_merc, lat_ = midcelly_merc, from_crs = merc_crs, to_crs = wgs_crs)
       midcellx_wgs = midcell_pt_wgs@coords[1]
       midcelly_wgs = midcell_pt_wgs@coords[2]
       midcell_wgs  = c(midcellx_wgs, midcelly_wgs)

       xyclosefar_pts_wgs = from_crs1_to_crs2_lon_lat(lon_ = c(xclose_merc,xfar_merc), lat_ = c(yclose_merc,yfar_merc), from_crs = merc_crs, to_crs = wgs_crs)
       xclose_wgs = xyclosefar_pts_wgs@coords[1,1]
       xfar_wgs   = xyclosefar_pts_wgs@coords[2,1]
       yclose_wgs = xyclosefar_pts_wgs@coords[1,2]
       yfar_wgs   = xyclosefar_pts_wgs@coords[2,2]

       datalon_wgs = c(xclose_wgs, xfar_wgs, xfar_wgs, xclose_wgs ,xclose_wgs)
       datalat_wgs = c(yclose_wgs, yclose_wgs, yfar_wgs, yfar_wgs, yclose_wgs)
       id_     = paste0(row, '_', col)

       # Check to see if already drawn, and if so remove it from df and leaflet map
       if (id_ %in% data$pixel_df$Pixel){
         remove_polyline(id = id_, all = FALSE)
         row_to_remove = subset(data$pixel_df, data$pixel_df$Pixel==id_)
         data$pixel_df = subset(data$pixel_df, Pixel!=id_)
         print ('row to remove')
         print (row_to_remove)
         
         color_to_remove = as.character(row_to_remove$pixel_color)
         variables$color_list_reserve = c(color_to_remove, variables$color_list_reserve)
         variables$color_list = variables$color_list[!variables$color_list %in% color_to_remove]

         if (type_ == '250m'){
           # Remove polygon from data$pixel_sps_250m
           ids_250m = unique(ggplot2::fortify(data$pixel_sps_250m)$id)
           len = length(ids_250m)
           lst = c(1:len)
           pos = which(unique(ggplot2::fortify(data$pixel_sps_250m)$id) %in% c(id_))
           lst_ = lst[-(pos)]
           data$pixel_sps_250m = data$pixel_sps_250m[lst_]
           # Remove point from data$midcell_pixel_sin
           data$midcell_pixel_sin = data$midcell_pixel_sin[data$midcell_pixel_sin@data$ID != id_,]
         }

       }else{
         # Draw the pixel polygon on the leaflet map
         if (input$highlightPixelModeNDVI){
           variables$color_count = length(unique(data$pixel_df$Pixel)) + 1

           if (length(variables$color_list_reserve) > 0){
           # Add color to the current color list being displayed in the app
           variables$color = variables$color_list_reserve[1]
           variables$color_list = c(variables$color_list, variables$color)
           # Remove the new color from the reserve of colors
           variables$color_list_reserve = variables$color_list_reserve[!variables$color_list_reserve %in% variables$color]
           }else{
             variables$color = 'black'
           }
           
           # Create Label
           pixel_label = paste0('Pixel Id: ', id_)
           
           # Add polygon to leaflet map
           leafletProxy("map") %>%
             addPolygons(datalon_wgs, datalat_wgs, layerId = id_, weight = 4,  opacity = .95, color = variables$color, 
               group = '250m Highlighted Pixels', fillOpacity = .1, label = pixel_label)
         }

         ps = paste0('--Cell Id: ', id_, ' --Cell # in Landcover: ', cell,
                     ' --Row: ', row, ' --Column: ', col, ' --Pft Number: ', vegindex,
                     ' --Middle of Cell lon: ', midcell_wgs[1], ' lat: ', midcell_wgs[2])
         print (ps)
    

         pixel  = matrix_to_polygon(rbind(c(datalon_merc[1], datalat_merc[1]),
                                          c(datalon_merc[2], datalat_merc[2]),
                                          c(datalon_merc[3], datalat_merc[3]),
                                          c(datalon_merc[4], datalat_merc[4]),
                                          c(datalon_merc[5], datalat_merc[5])), id_, as.character(type_), crs = merc_crs)
         
         
         
         # If NLCD layer is at this site, calculate heterogeneity and pft % coverages under pixel
         if (data$NLCD == TRUE){
           selected_pixel = raster::crop(data$rc_nlcd, pixel, snap = 'out' )
           # reproject with higher resolution by setting the resolution equal to 1
           selected_pixel_high_res   = raster::projectRaster(from = selected_pixel, crs = merc_crs, method='ngb', res = res(selected_pixel)/40.5)
           # selected_pixel_high_res   = raster::projectRaster(from = selected_pixel, crs = merc_crs, method='ngb', res = res(selected_pixel)/5)
           selected_pixel_high_res_c = raster::crop(selected_pixel_high_res, pixel, snap = 'in' )
           
           # Build dataframe with frequency of landcover PFT values at this pixel
           data_df_at_pixel = as.data.frame(table(selected_pixel_high_res_c@data@values), stringsAsFactors=FALSE) %>% 
             mutate(Var1 = as.double(Var1)) %>%
             left_join(pft_df, by = c('Var1' = 'pft_key')) %>% 
             mutate(id = id_)
           
           # Calculate percentage of selected values from total values
           total_pixels = sum(data_df_at_pixel$Freq)
           selected_values = subset(data_df_at_pixel, data_df_at_pixel$Var1 == data$pft)$Freq
           selected_percentage = selected_values/total_pixels 
           heterogeneity_at_pixel = length(data_df_at_pixel$Var1)
           
           if (length(selected_percentage) ==0){
             selected_percentage = 0}
           if (length(heterogeneity_at_pixel) ==0){
             heterogeneity_at_pixel = 0}
           
           if (length(data$nlcd_breakdown_df)==0){
             data$nlcd_breakdown_df = data_df_at_pixel
           }else {
             data$nlcd_breakdown_df = rbind(data$nlcd_breakdown_df, data_df_at_pixel)
           }
           
           # Calculate majority pft, majority pft % cover, and unique hterogeneity pfts
           data_df_at_pixel
           
           pft_mode = subset(data_df_at_pixel, data_df_at_pixel$Freq == max(data_df_at_pixel$Freq))$pft_expanded
           mode_percentage = max(data_df_at_pixel$Freq) / sum(data_df_at_pixel$Freq)
           heterogeneity_pft = dim(data_df_at_pixel)[1]
         }
         
         

         # Subset the pixel dataframe for plotting tab
         if (data$NLCD == FALSE){
           # Build Dataframe   reactive value = data$pixel_df
           data$pixel_df = rbind(data$pixel_df, data.frame(Pixel = id_, Site = name, pixel_color = variables$color, 
             Lat_wgs = midcelly_wgs, Lon_wgs = midcellx_wgs, Type = '250m',
             pft = vegindex, Lat_sin = midcelly_sin, Lon_sin = midcellx_sin,
             pt1_lat = datalat_wgs[1], pt1_lon = datalon_wgs[1],
             pt2_lat = datalat_wgs[2], pt2_lon = datalon_wgs[2],
             pt3_lat = datalat_wgs[3], pt3_lon = datalon_wgs[3],
             pt4_lat = datalat_wgs[4], pt4_lon = datalon_wgs[4],
             pt5_lat = datalat_wgs[5], pt5_lon = datalon_wgs[5]))
           row.names(data$pixel_df) = c()
           data$pixel_df_table = data$pixel_df %>%
             select(Site, Pixel, Lat_wgs, Lon_wgs) %>% datatable(options = list(searchHighlight = TRUE, pageLength = 10), filter = 'top') %>%
             formatStyle('Pixel', fontWeight = 'bold', backgroundColor = styleEqual(unique(data$pixel_df$Pixel), c(unique(as.character(data$pixel_df$pixel_color)))))
         }else {
           # Build Dataframe   reactive value = data$pixel_df
           data$pixel_df = rbind(data$pixel_df, data.frame(Pixel = id_, Site = name, pixel_color = variables$color, 
             Lat_wgs = midcelly_wgs, Lon_wgs = midcellx_wgs, PFT_Mode = pft_mode, Type = '250m',
             Mode_Percentage = mode_percentage, Heterogeneity_PFT = heterogeneity_pft,
             pft = vegindex, Lat_sin = midcelly_sin, Lon_sin = midcellx_sin,
             pt1_lat = datalat_wgs[1], pt1_lon = datalon_wgs[1],
             pt2_lat = datalat_wgs[2], pt2_lon = datalon_wgs[2],
             pt3_lat = datalat_wgs[3], pt3_lon = datalon_wgs[3],
             pt4_lat = datalat_wgs[4], pt4_lon = datalon_wgs[4],
             pt5_lat = datalat_wgs[5], pt5_lon = datalon_wgs[5]))
           row.names(data$pixel_df) = c()
           data$pixel_df_table = data$pixel_df %>%
             select(Site, Pixel, PFT_Mode, Mode_Percentage, Heterogeneity_PFT) %>% datatable(options = list(searchHighlight = TRUE, pageLength = 10), filter = 'top') %>%
             formatStyle('Pixel', fontWeight = 'bold', backgroundColor = styleEqual(unique(data$pixel_df$Pixel), c(unique(as.character(data$pixel_df$pixel_color)))))
         }

         coords_ = data.frame(x = midcellx_sin, y = midcelly_sin)
         row.names(coords_) = id_ 
         center_point = SpatialPointsDataFrame(coords_, data=data.frame(ID=id_), proj4string=CRS(sinu_crs))
         
          if(type_ == '250m'){
             if (length(data$pixel_sps_250m) == 0){
               data$pixel_sps_250m = pixel
               data$midcell_pixel_sin = center_point
             }else{
               data$pixel_sps_250m = rbind(data$pixel_sps_250m, pixel)
               data$midcell_pixel_sin = rbind(data$midcell_pixel_sin, center_point)
             }}
         print (data$pixel_df)
       }
     }
  }


  # Get specific site data and returns lon/lat/camera/description/elevation
  get_site_info = function(site_name){
    site_data = subset(cams_, site == site_name)
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
