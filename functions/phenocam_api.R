
get_phenocam_camera_df = function(pc_roi_df){
  # Use the phenocamapi package to pull in the phenocams data
  cams = phenocamapi::get_phenos()
  # Changing blank values in the camera orientation field to 'N' as a default
  cams$camera_orientation[cams$camera_orientation == ''] = 'N'
  cams$camera_orientation[is.na(cams$camera_orientation)] = 'N'
  
  roi_sites = unique(pc_roi_df$site)
  num_roi_sites = length(roi_sites)
  
  # Match the two dataframes where the ROI exists
  idx = is.element(cams$site, roi_sites)
  cams_with_rois = cams[idx,]
  # Return cams with roi dataframe
  return (cams_with_rois)
}


get_phenocam_roi_df = function(){
  print ('Grabbing phenocam rois-api')
  roi_files = phenocamapi::get_rois()
  return(roi_files)
}


# Cache out the cams_ and rois_ dataframes from phenocam api
cache_phenocam_data = function(dir = '.'){
  todays_date = Sys.Date()
  todays_cams_name = paste0(dir, '/www/phenocam_data/cams_', todays_date)
  todays_rois_name = paste0(dir, '/www/phenocam_data/rois_', todays_date)
  if(dir.exists(paste0(dir, '/www/phenocam_data'))==FALSE){
    dir.create(paste0(dir, '/www/phenocam_data'))
    roi_files = get_phenocam_roi_df()
    cams_     = get_phenocam_camera_df(pc_roi_df = roi_files) %>% dplyr::select(-c('flux_networks'))
    write.csv(roi_files, todays_rois_name)
    write.csv(cams_, todays_cams_name)
  } else{
    # If directory exists check to see if todays cams and rois exist
    if (file.exists(todays_cams_name) & file.exists(todays_rois_name)){
      cams_ = read.csv(todays_cams_name, stringsAsFactors = FALSE)
      roi_files = read.csv(todays_rois_name, stringsAsFactors = FALSE)
    }else {
      roi_files = get_phenocam_roi_df()
      cams_     = get_phenocam_camera_df(pc_roi_df = roi_files) %>% dplyr::select(-c('flux_networks'))
      write.csv(roi_files, todays_rois_name)
      write.csv(cams_, todays_cams_name)
    }
  }
  return(list('cams'=cams_, 'rois'=roi_files))
}


# Remove any phenocam data that is older than 30 days
remove_old_cached_phenocam_data = function(days_back = 30, dir = '.'){
  todays_date = Sys.Date()
  if(dir.exists(paste0(dir, '/www/phenocam_data'))){
    phenocam_cached_files = list.files(paste0(dir, '/www/phenocam_data'))
    if(phenocam_cached_files > 0){
      # Extract the dates from the files
      cams_cached = phenocam_cached_files[grepl('cams_',phenocam_cached_files )]
      rois_cached = phenocam_cached_files[grepl('rois_',phenocam_cached_files )]
      cams_dates = as.Date(unlist(strsplit(cams_cached, 'cams_'))[unlist(strsplit(cams_cached, 'cams_')) != ''])
      rois_dates = as.Date(unlist(strsplit(rois_cached, 'rois_'))[unlist(strsplit(rois_cached, 'rois_')) != ''])
      remove_cams_dates = paste0('cams_',cams_dates[cams_dates < (todays_date -days_back)])
      remove_rois_dates = paste0('rois_',rois_dates[cams_dates < (todays_date -days_back)])
      remove_cams_dates = remove_cams_dates[file.exists(paste0(dir, '/www/phenocam_data/',remove_cams_dates))]
      remove_rois_dates = remove_rois_dates[file.exists(paste0(dir, '/www/phenocam_data/',remove_rois_dates))]
      file.remove(paste0(dir, '/www/phenocam_data/',remove_cams_dates))
      file.remove(paste0(dir, '/www/phenocam_data/',remove_rois_dates))
    }
  }
}