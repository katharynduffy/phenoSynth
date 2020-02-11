# Edit function for custom naming convention for this test
submit_all_tasks_for_site_appeears = function(appeears_token, # STR - APPEEARS TOKEN FROM USERNAME PASSWORD LOGIN
                                 roi_sites,      # CHR VECTOR - ROI SITE NAMES TO SUBMIT TO APPEEARS
                                 phenocam_roi_df,
                                 endDate_         =  as.character(Sys.Date()),
                                 projection_name_ = 'sinu_modis',   # 'geographic'
                                 type_            = 'netcdf4',      # geotiff'
                                 degree           = .25,
                                 task_type_       = 'area',
                                 layers_          = NULL,
                                 test             = TRUE,
                                 void_size_       = FALSE){
  
  
  if (length(roi_sites)==0){
    stop('Input [roi_sites] is empty.')
  } else if (void_size_ == FALSE){
      if (length(roi_sites)>16){
        stop('Input [roi_sites] is too big, requests must be a length of 100 or less')
        }
    }

  token = appeears_token
  count = 0
  
  for (roi in roi_sites){
    # Grab row for current roi in loop
    data = subset(phenocam_roi_df, phenocam_roi_df$site == roi)[1,]
    
    site       = data$site
    lat        = data$lat
    lon        = data$lon
    
    # build boundary box based on lat/lon from site
    # bbox needs to look like this: {min_longitude},{min_latitude},{max_longitude},{max_latitude}
    bbox_ = as.character(paste0(lon-degree,',', lon+degree,',', lat-degree,',', lat+degree))
    
    xmin = lon-degree
    xmax = lon+degree
    ymin = lat-degree
    ymax = lat+degree
    
    pt1 = c(xmin,ymin)
    pt2 = c(xmax,ymin)
    pt3 = c(xmax,ymax)
    pt4 = c(xmin,ymax)
    
    coords_ = list(pt1, pt2, pt3, pt4, pt1)
    
    if (is.null(layers_)){
      layers_ = c('NDVI_tera_sinu', 'EVI_tera_sinu', 'NDVI_aqua_sinu', 'EVI_aqua_sinu', 'TDS_v6', 'LC_sinu_nc')
    }
    for (layer_ in layers_){
      startDate_       = '01-01-2000'
      # Create task name for each roi
      if (layer_ == 'NDVI_tera_sinu'){
        task_name_   = paste0(site, '_', 'NDVI_v6_tera_sinu')
        task_layers      = list(list(layer = '_250m_16_days_NDVI', product = 'MOD13Q1.006'))
      }else if (layer_ == 'EVI_tera_sinu'){
        task_name_   = paste0(site, '_', 'EVI_v6_tera_sinu')
        task_layers      = list(list(layer = '_250m_16_days_EVI', product = 'MOD13Q1.006'))
      }else if (layer_ == 'NDVI_aqua_sinu'){
        task_name_   = paste0(site, '_', 'NDVI_v6_aqua_sinu')
        task_layers      = list(list(layer = '_250m_16_days_NDVI', product = 'MYD13Q1.006'))
      }else if (layer_ == 'EVI_aqua_sinu'){
        task_name_   = paste0(site, '_', 'EVI_v6_aqua_sinu')
        task_layers      = list(list(layer = '_250m_16_days_EVI', product = 'MYD13Q1.006'))
      }else if (layer_ == 'TDS_v6'){
        task_name_   = paste0(site, '_', 'TDs_v6')
        task_layers      = list(list(layer = 'Dormancy', product = 'MCD12Q2.006'),
                            list(layer = 'Greenup', product = 'MCD12Q2.006'),
                            list(layer = 'Maturity', product = 'MCD12Q2.006'),
                            list(layer = 'MidGreendown', product = 'MCD12Q2.006'),
                            list(layer = 'MidGreenup', product = 'MCD12Q2.006'),
                            list(layer = 'Peak', product = 'MCD12Q2.006'),
                            list(layer = 'QA_Overall', product = 'MCD12Q2.006'),
                            list(layer = 'Senescence', product = 'MCD12Q2.006'))
      } else if(layer_ == 'LC_sinu_nc'){
        task_name_ = paste0(site, '_', 'LC_sinu_v6')
        type_   = 'netcdf4'
        task_layers = list(list(layer = 'LC_Type1', product = 'MCD12Q1.006'))
        startDate_ = '01-01-2018'
        endDate_   = '12-12-2018'
      }
      # Check to see task name
      print (task_name_)
      
      task_list = list(task_type = task_type_, 
                       task_name = task_name_,
                       params    =  list(dates  = list(list(startDate = startDate_,
                                                            endDate   = endDate_)),
                                         layers = task_layers,
                                         output = list(format     = list(type = type_),
                                                       projection = projection_name_),
                                         geo    = list(type       = 'FeatureCollection',
                                                       fileName   = 'User-Drawn-Polygon',
                                                       features   = list(list(type   = 'Feature',
                                                                              geometry = list (type        = 'Polygon',
                                                                                               coordinates = list(coords_)))))))
      #############################
      ### Submits requests here ###
      #############################
      
      task_json = rjson::toJSON(task_list)
      print (rjson::toJSON(task_list))
      
      if (test == FALSE){
        # # submit the task request
        print (site)
        response <- POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/task", body = task_json, encode = "json",
                         add_headers(Authorization = token, "Content-Type" = "application/json"))
        task_response <- prettify(jsonlite::toJSON(content(response), auto_unbox = TRUE))
        print (task_response)
      }
      
      count = count + 1
      
      #############################
      ### Submits requests here ###
      #############################
    }# Layer loop end
  }# Site loop end
  print (count)
}


# Edit function for custom naming convention for this test
submit_appeears_tasks = function(appeears_token, # STR - APPEEARS TOKEN FROM USERNAME PASSWORD LOGIN
                                     roi_sites,      # CHR VECTOR - ROI SITE NAMES TO SUBMIT TO APPEEARS
                                     data_layer,     # STRING - DATA LAYER TO REQUEST FROM APPEEARS ('EVI', 'NDVI',TDS)
                                     startDate_       = '01-01-2000',
                                     endDate_         =  as.character(Sys.Date()),
                                     projection_name_ = 'geographic',   # 'sinusoidal'
                                     type_            = 'netcdf4',      # geotiff'
                                     degree           = .5,
                                     task_type_       = 'area',
                                     test             = TRUE){
  
  if (length(roi_sites)==0){
    stop('Input [roi_sites] is empty.')
  } else if (length(roi_sites)>100){
    stop('Input [roi_sites] is too big, requests must be a length of 100 or less')
  }
  
  # Import phenocam roi dataframe from phenocam roi API
  rois            = jsonlite::fromJSON('https://phenocam.sr.unh.edu/api/roilists/?format=json&limit=2000')
  phenocam_roi_df = rois$results %>% distinct(site,lat,lon)
  
  token = appeears_token
  count = 0
  
  for (roi in roi_sites){
    # Grab row for current roi in loop
    data = subset(phenocam_roi_df, phenocam_roi_df$site == roi)[1,]
    
    site       = data$site
    lat        = data$lat
    lon        = data$lon
    print(lat)
    print(lon)
    
    # Create task name for each roi
    if (data_layer == 'NDVI_tera'){
      task_name_   = paste0(site, '_', 'NDVI_v6_tera')
      task_layers      = list(list(layer = '_250m_16_days_NDVI', product = 'MOD13Q1.006'))
    }else if (data_layer == 'EVI_tera'){
      task_name_   = paste0(site, '_', 'EVI_MOD13Q1_v6_tera')
      task_layers      = list(list(layer = '_250m_16_days_EVI', product = 'MOD13Q1.006'))
    }else if (data_layer == 'NDVI_aqua'){
      task_name_   = paste0(site, '_', 'NDVI_v6_aqua')
      task_layers      = list(list(layer = '_250m_16_days_NDVI', product = 'MYD13Q1.006'))
    }else if (data_layer == 'EVI_aqua'){
      task_name_   = paste0(site, '_', 'EVI_MOD13Q1_v6_aqua')
      task_layers      = list(list(layer = '_250m_16_days_EVI', product = 'MYD13Q1.006'))
    }else if (data_layer == 'TDS'){
      task_name_   = paste0(site, '_', 'TDs_v5')
      task_layers      = list(list(layer = 'NBAR_EVI_Onset_Greenness_Maximum', product = 'MCD12Q2.005'),
                          list(layer = 'NBAR_EVI_Onset_Greenness_Minimum', product = 'MCD12Q2.005'),
                          list(layer = 'Onset_Greenness_Decrease', product = 'MCD12Q2.005'),
                          list(layer = 'Onset_Greenness_Increase', product = 'MCD12Q2.005'),
                          list(layer = 'Onset_Greenness_Maximum', product = 'MCD12Q2.005'),
                          list(layer = 'Onset_Greenness_Minimum', product = 'MCD12Q2.005'))
    }    else if (data_layer == 'TDS_v6'){
      task_name_   = paste0(site, '_', 'TDs_v6')
      task_layers      = list(list(layer = 'Dormancy', product = 'MCD12Q2.006'),
                          list(layer = 'Greenup', product = 'MCD12Q2.006'),
                          list(layer = 'Maturity', product = 'MCD12Q2.006'),
                          list(layer = 'MidGreendown', product = 'MCD12Q2.006'),
                          list(layer = 'MidGreenup', product = 'MCD12Q2.006'),
                          list(layer = 'Peak', product = 'MCD12Q2.006'),
                          list(layer = 'QA_Overall', product = 'MCD12Q2.006'),
                          list(layer = 'Senescence', product = 'MCD12Q2.006'))
    }else {
      stop(paste0('Cannot find Data_layer: ',data_layer))
    }
    
    # build boundary box based on lat/lon from site
    # bbox needs to look like this: {min_longitude},{min_latitude},{max_longitude},{max_latitude}
    bbox_ = as.character(paste0(lon-degree,',', lon+degree,',', lat-degree,',', lat+degree))
    
    xmin = lon-degree
    xmax = lon+degree
    ymin = lat-degree
    ymax = lat+degree
    
    pt1 = c(xmin,ymin)
    pt2 = c(xmax,ymin)
    pt3 = c(xmax,ymax)
    pt4 = c(xmin,ymax)
    
    coords_ = list(pt1, pt2, pt3, pt4, pt1)
    
    task_list = list(task_type = task_type_, 
                     task_name = task_name_,
                     params    =  list(dates  = list(list(startDate = startDate_,
                                                          endDate   = endDate_)),
                                       layers = task_layers,
                                       output = list(format     = list(type = type_),
                                                     projection = projection_name_),
                                       geo    = list(type       = 'FeatureCollection',
                                                     fileName   = 'User-Drawn-Polygon',
                                                     features   = list(list(type   = 'Feature',
                                                                            geometry = list (type        = 'Polygon',
                                                                                             coordinates = list(coords_)))))))
    
    
    #############################
    ### Submits requests here ###
    #############################
    
    task_json = rjson::toJSON(task_list)
    print (rjson::toJSON(task_list))
    
    if (test == FALSE){
      # # submit the task request
      print ('real run')
      print (site)
      response <- POST("https://lpdaacsvc.cr.usgs.gov/appeears/api/task", body = task_json, encode = "json",
                       add_headers(Authorization = token, "Content-Type" = "application/json"))
      task_response <- prettify(jsonlite::toJSON(content(response), auto_unbox = TRUE))
      print (task_response)
    }
    
    count = count + 1
    
    #############################
    ### Submits requests here ###
    #############################
  }
  print (count)
}